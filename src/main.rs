use anyhow::Result;
use std::{
    collections::{HashMap, HashSet},
    env, fs, io,
    io::{ErrorKind, Write},
    path::PathBuf,
    rc::Rc,
    sync::Mutex,
};
use swc_common::{sync::Lrc, Globals, Mark, SourceFile, SourceMap, GLOBALS};
use swc_ecma_ast::{
    AssignExpr, CallExpr, Expr, ExprOrSpread, ExprOrSuper, ExprStmt, Ident, Lit, Module,
    ModuleDecl, ModuleItem, Stmt, Str,
};
use swc_ecma_codegen::{text_writer::JsWriter, Emitter};
use swc_ecma_parser::{lexer::Lexer, JscTarget, Parser, StringInput, Syntax};
use swc_ecma_transforms::modules::common_js::common_js;
use swc_ecma_visit::{noop_visit_type, Fold, Node, Visit, VisitWith};
use uuid::Uuid;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if let Some(entry) = args.get(1) {
        let root_with_deps_top_down = create_deps_tree(&PathBuf::from(entry));
        let u_root_with_deps_top_down = root_with_deps_top_down.unwrap();
        let src = generate_bundle(u_root_with_deps_top_down);

        fs::write("out.js", src)?;

        Ok(())
    } else {
        Err(anyhow::Error::msg(
            "entry point should be provided".to_string(),
        ))
    }
}

pub fn init_source_map() -> Lrc<SourceMap> {
    Default::default()
}

pub fn load_file(sm: &SourceMap, path: &PathBuf) -> Result<Lrc<SourceFile>, io::Error> {
    if path.is_dir() {
        sm.load_file(&path.join("index.js"))
    } else {
        let path_as_str = path
            .to_str()
            .ok_or(format!("pathbuf: {:?} to str converted", &path))
            .map_err(|e| io::Error::new(ErrorKind::Other, e))?;
        if !path_as_str.ends_with(".js") {
            sm.load_file(&PathBuf::from(format!("{}.js", path_as_str)))
        } else {
            sm.load_file(path)
        }
    }
}

pub fn init_lexer<'a>(input_file: &'a Lrc<SourceFile>) -> Lexer<'a, StringInput<'a>> {
    Lexer::new(
        Syntax::Es(Default::default()),
        JscTarget::Es2015,
        StringInput::from(&**input_file),
        None,
    )
}

fn get_transformed_module<'a>(file_path: &PathBuf) -> Result<ParsedModule> {
    let sm = init_source_map();
    let parsed_module = get_parsed_module(file_path, &sm);

    transform_module(parsed_module?.body, &file_path, sm)
}

fn get_parsed_module<'a>(file_path: &PathBuf, sm: &Lrc<SourceMap>) -> Result<Module> {
    let lf = load_file(&sm.clone(), &file_path.clone())?;
    let lexer = init_lexer(&lf);
    let mut parser = init_parser(lexer);

    parser
        .parse_module()
        .map_err(|e| anyhow::Error::msg(format!("error during module parsing: {:?}", e)))
}

pub fn init_parser<'a>(lexer: Lexer<'a, StringInput<'a>>) -> Parser<Lexer<'a, StringInput<'a>>> {
    Parser::new_from(lexer)
}

struct ImportsTraverser {
    imports: Vec<String>,
}

impl ImportsTraverser {
    pub fn match_require(&mut self, sym: String, args: &Vec<ExprOrSpread>) {
        if sym.to_string() == "require" {
            if args.len() == 1 {
                match &args[0] {
                    ExprOrSpread { expr, .. } => match &**expr {
                        Expr::Lit(Lit::Str(Str { value, .. })) => {
                            self.imports.push(value.to_string());
                        }
                        _ => (),
                    },
                }
            }
        }
    }
}

impl Visit for ImportsTraverser {
    noop_visit_type!();

    fn visit_module_decl(&mut self, n: &ModuleDecl, _parent: &dyn Node) {
        match n {
            ModuleDecl::Import(decl) => self.imports.push(decl.src.value.to_string()),
            _ => (),
        }

        n.visit_children_with(self)
    }

    fn visit_module_item(&mut self, n: &ModuleItem, _parent: &dyn Node) {
        match n {
            ModuleItem::Stmt(Stmt::Expr(ExprStmt { expr, .. })) => match &**expr {
                Expr::Call(CallExpr { callee, args, .. }) => match callee {
                    ExprOrSuper::Expr(i) => match &**i {
                        Expr::Ident(Ident { sym, .. }) => {
                            self.match_require(sym.to_string(), args);
                        }
                        _ => (),
                    },
                    _ => (),
                },
                _ => (),
            },
            ModuleItem::ModuleDecl(ModuleDecl::Import(decl)) => {
                self.imports.push(decl.src.value.to_string())
            }

            _ => (),
        }

        n.visit_children_with(self)
    }

    fn visit_call_expr(&mut self, n: &CallExpr, _parent: &dyn Node) {
        match n {
            CallExpr { callee, args, .. } => match callee {
                ExprOrSuper::Expr(i) => match &**i {
                    Expr::Ident(Ident { sym, .. }) => {
                        self.match_require(sym.to_string(), args);
                    }
                    _ => (),
                },
                _ => (),
            },
        }

        n.visit_children_with(self)
    }

    fn visit_assign_expr(&mut self, n: &AssignExpr, _parent: &dyn Node) {
        match n {
            AssignExpr { right, .. } => match &**right {
                Expr::Call(CallExpr { callee, args, .. }) => match callee {
                    ExprOrSuper::Expr(i) => match &**i {
                        Expr::Ident(Ident { sym, .. }) => {
                            self.match_require(sym.to_string(), args);
                        }
                        _ => (),
                    },
                    _ => (),
                },
                _ => (),
            },
        }
        n.visit_children_with(self)
    }
}

#[derive(Debug, Clone)]
struct ParsedModule {
    pub id: String,
    pub abs_path: PathBuf,
    pub imports: Vec<PathBuf>,
    pub source_code: String,
    pub deps_map: Option<HashMap<String, String>>,
}
fn transform_module(
    items: Vec<ModuleItem>,
    path_to_module: &PathBuf,
    sm: Lrc<SourceMap>,
) -> Result<ParsedModule> {
    let imports: Vec<PathBuf> = items
        .iter()
        .map(|m| {
            let mut t = ImportsTraverser { imports: vec![] };
            m.visit_with(m, &mut t);
            t.imports
        })
        .flatten()
        .map(PathBuf::from)
        .collect::<HashSet<PathBuf>>()
        .iter()
        .map(PathBuf::from)
        .collect::<Vec<PathBuf>>();

    Ok(ParsedModule {
        id: Uuid::new_v4().to_hyphenated().to_string(),
        abs_path: path_to_module.clone(),
        imports,
        source_code: js_to_common_js(items.clone(), sm)?,
        deps_map: None,
    })
}

fn create_deps_tree(root: &PathBuf) -> Result<Vec<ParsedModule>> {
    let parsed_root = get_transformed_module(root)?;
    let node_modules = scan_node_modules();

    let mut transformed_modules: Vec<ParsedModule> = vec![parsed_root.clone()];
    let mut modules_to_traverse: Vec<ParsedModule> = vec![parsed_root];

    let mut cache: HashMap<String, ParsedModule> = HashMap::new();
    while !modules_to_traverse.is_empty() {
        let mut new_modules = vec![];
        for module in modules_to_traverse.iter_mut() {
            for dep in module.imports.iter() {
                let dependency_path = get_dependency_path(&dep, &module.abs_path, &node_modules)?;

                let dep_info =
                    if cache.contains_key(&dependency_path.clone().to_str().unwrap().to_string()) {
                        cache
                            .get(&dependency_path.clone().to_str().unwrap().to_string())
                            .unwrap()
                            .clone()
                    } else {
                        let dep_info = get_transformed_module(&dependency_path)?;
                        cache.insert(
                            dep_info.abs_path.clone().to_str().unwrap().to_string(),
                            dep_info.clone(),
                        );
                        new_modules.push(dep_info.clone());

                        dep_info
                    };

                let m = transformed_modules
                    .iter_mut()
                    .find(|item| item.id == module.id)
                    .expect("module is present");
                if m.deps_map.is_none() {
                    m.deps_map = Some(HashMap::new());
                }
                m.deps_map.as_mut().unwrap().insert(
                    dep.to_str().expect("converted").to_string(),
                    dep_info.id.clone(),
                );
            }
        }
        transformed_modules.append(&mut new_modules.clone().into_iter().collect::<Vec<_>>());
        modules_to_traverse = new_modules.into_iter().collect::<Vec<ParsedModule>>();
    }

    Ok(transformed_modules)
}

fn get_dependency_path(
    dep_relative_root_path: &PathBuf,
    parent_abs_path: &PathBuf,
    node_modules: &HashSet<PathBuf>,
) -> Result<PathBuf> {
    if dep_relative_root_path.is_absolute() {
        Err(anyhow::Error::msg(
            "doesn't work with absolute paths".to_string(),
        ))
    } else {
        if starts_from(&dep_relative_root_path, &node_modules) {
            Ok(PathBuf::from("node_modules").join(dep_relative_root_path))
        } else {
            let file_name = &dep_relative_root_path
                .to_str()
                .ok_or(format!(
                    "pathbuf: {:?} to str converted",
                    &dep_relative_root_path
                ))
                .map_err(|e| anyhow::Error::msg(e))?[2..];
            if parent_abs_path.is_dir() {
                Ok(parent_abs_path.join(&file_name))
            } else {
                let dir = get_path_to_file_parent_dir(&parent_abs_path);
                Ok(dir.join(&file_name))
            }
        }
    }
}

fn scan_node_modules() -> HashSet<PathBuf> {
    if let Ok(entries) = fs::read_dir("node_modules") {
        HashSet::from(
            entries
                .into_iter()
                .map(|e| {
                    PathBuf::from(
                        e.expect("entry")
                            .path()
                            .strip_prefix("node_modules")
                            .expect("node_modules prefix stripped"),
                    )
                })
                .collect(),
        )
    } else {
        HashSet::new()
    }
}

fn starts_from(name: &PathBuf, modules: &HashSet<PathBuf>) -> bool {
    modules.iter().any(|m| {
        name.to_str()
            .expect("pathbuf to str")
            .split("/")
            .collect::<Vec<&str>>()
            .get(0)
            .expect("root is present")
            == &m.to_str().unwrap()
    })
}

fn generate_bundle(modules_hierarchy: Vec<ParsedModule>) -> String {
    let modules_executables: String = modules_hierarchy
        .iter()
        .map(|module| {
            let module_deps = {
                if let Some(map) = &module.deps_map.as_ref() {
                    serde_json::to_string(map).expect("deps map into json serialized")
                } else {
                    "{}".to_owned()
                }
            };
            format!(
                "\"{module_id}\": {{
              wrapped_module: (module, exports, require) => {{
                {module_source_code}
              }},
              deps_map: {module_deps}
	    }}",
                module_id = &module.id,
                module_source_code = module.source_code,
                module_deps = module_deps
            )
        })
        .collect::<Vec<String>>()
        .join(",");

    let bundler_core = format!(
        "
(function(modules){{
    if (typeof window !== 'undefined') {{
    window.process = {{env: {{NODE_ENV: 'production'}}}};
}}
    const require = id => {{
      const {{wrapped_module, deps_map}} = modules[id];
      const localRequire = requiredModuleName => require(deps_map[requiredModuleName]);
      const module = {{exports: {{}}}};
      wrapped_module.call(module.exports, module, module.exports, localRequire);
      return module.exports;
    }}
    require(\"{root_module_id}\");
  }})({{ {modules} }})
",
        root_module_id = modules_hierarchy.get(0).expect("source root is present").id,
        modules = modules_executables
    );

    bundler_core
}

fn get_path_to_file_parent_dir(file: &PathBuf) -> PathBuf {
    let mut cp = file.clone();
    cp.pop();
    cp
}

struct Output<W>(Rc<Mutex<W>>);

impl<W: Write> Write for Output<W> {
    fn write(&mut self, buf: &[u8]) -> Result<usize, io::Error> {
        (*self.0.lock().unwrap()).write(buf)
    }

    fn flush(&mut self) -> Result<(), io::Error> {
        (*self.0.lock().unwrap()).flush()
    }
}

impl<W: Write> Clone for Output<W> {
    fn clone(&self) -> Self {
        Output(self.0.clone())
    }
}

fn js_to_common_js(module: Vec<ModuleItem>, sm: Lrc<SourceMap>) -> Result<String> {
    GLOBALS.set(&Globals::new(), || {
        let transpiled_module = transpile_module_to_common_js(module);

        let write_to = Output(Rc::new(Mutex::new(vec![])));
        emit_source_code(transpiled_module, sm, write_to.clone())?;

        let src = &*write_to.0.lock().expect("not poisoned");
        Ok(std::str::from_utf8(src)?.to_string())
    })
}

fn transpile_module_to_common_js(module: Vec<ModuleItem>) -> Vec<ModuleItem> {
    common_js(
        Mark::fresh(Mark::root()),
        swc_ecma_transforms::modules::util::Config {
            no_interop: true,
            ..Default::default()
        },
    )
    .fold_module_items(module)
}

fn emit_source_code<W: Write>(
    module: Vec<ModuleItem>,
    sm: Lrc<SourceMap>,
    writable: W,
) -> Result<()> {
    Emitter {
        cfg: Default::default(),
        cm: sm.clone(),
        wr: Box::new(JsWriter::new(sm.clone(), "\n", writable, None)),
        comments: None,
    }
    .emit_module(&Module {
        body: module,
        span: Default::default(),
        shebang: None,
    })?;
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::read_to_string;
    use std::fs::File;
    use std::io::Write;
    use std::process::Command;
    use std::thread::sleep;
    use std::time::Duration;
    use tempfile::{tempdir, TempDir};

    fn create_tmp_file() -> (File, PathBuf, TempDir, String) {
        let dir = tempdir().expect("temdir is created");
        let (file, file_path, name) = create_tmp_file_at_dir(&dir);

        (file, file_path, dir, name)
    }

    fn create_tmp_file_at_dir(dir: &TempDir) -> (File, PathBuf, String) {
        let name = format!("{}{}", Uuid::new_v4().to_hyphenated().to_string(), ".js");
        let file_path = dir.path().join(&name);
        let file = File::create(&file_path).unwrap();

        (file, file_path, name)
    }

    #[test]
    fn successful_parsing() {
        let (mut file, file_path, _dir, _) = create_tmp_file();
        writeln!(file, "import {{foo}} from './foo.js';").unwrap();

        let parsed_module = get_parsed_module(&file_path, &init_source_map());

        assert_eq!(parsed_module.unwrap().body.len(), 1);
    }

    #[test]
    fn transpiles_module() {
        let (mut file, file_path, _dir, _) = create_tmp_file();
        writeln!(file, "import {{foo}} from './foo.js';").unwrap();

        let sm = init_source_map();
        let src = js_to_common_js(get_parsed_module(&file_path, &sm).unwrap().body, sm);

        let u_src = src.unwrap();
        assert_eq!(
            u_src,
            r#""use strict";
var _fooJs = require("./foo.js");
"#
        )
    }

    #[test]
    fn transforms_module() {
        let (mut file, file_path, _dir, name) = create_tmp_file();
        let (mut file1, _file_path1, _dir1, _name1) = create_tmp_file();
        let s = format!(r"import {{foo}} from './{}';", &name); //&name1
        file.write_all(s.as_bytes()).unwrap();
        writeln!(file1, "export const foo = 5;").unwrap();

        let sm = init_source_map();
        let transformed_module = transform_module(
            get_parsed_module(&file_path, &sm).unwrap().body,
            &file_path,
            sm,
        );

        let transformed_u_module = transformed_module.unwrap();
        assert_eq!(transformed_u_module.abs_path, file_path);
        assert_eq!(transformed_u_module.imports.len(), 1);
        assert_eq!(
            &transformed_u_module.imports[0].to_str().unwrap()[2..],
            &name
        );
    }

    #[test]
    fn builds_deps_hierarchy() {
        let (mut file, file_path, dir, _name) = create_tmp_file();
        let (mut file1, file_path1, name1) = create_tmp_file_at_dir(&dir);
        let s = format!(r"import {{foo}} from './{}';", &name1); //&name1
        file.write_all(s.as_bytes()).unwrap();
        writeln!(file1, "export const foo = 5;").unwrap();

        let root_with_deps_top_down = create_deps_tree(&file_path);
        let u_root_with_deps_top_down = root_with_deps_top_down.unwrap();
        assert_eq!(u_root_with_deps_top_down.len(), 2);
        assert_eq!(u_root_with_deps_top_down[1].abs_path, file_path1);
    }

    #[test]
    fn get_file_dir() {
        let dir = get_path_to_file_parent_dir(&PathBuf::from("foo/index.js"));
        assert_eq!(dir.to_str().unwrap(), "foo");

        let dir = get_path_to_file_parent_dir(&PathBuf::from("foo/index"));
        assert_eq!(dir.to_str().unwrap(), "foo");
    }

    #[test]
    fn generates_bundle_code() {
        let (mut file, file_path, dir, _name) = create_tmp_file();
        let (mut file1, _file_path1, name1) = create_tmp_file_at_dir(&dir);
        let s = format!(
            r"import {{foo}} from './{}';
console.log(foo)",
            &name1
        );
        file.write_all(s.as_bytes()).unwrap();
        writeln!(file1, "export const foo = 5;").unwrap();

        let src = generate_bundle(create_deps_tree(&file_path).unwrap());

        let mut bundle = File::create(dir.path().join("bundle.js")).expect("bundle.js created");
        bundle
            .write_all(src.as_bytes())
            .expect("bundle has written");

        let stderr_path = dir.path().join("stderr");
        let mut cmd = Command::new("node");
        cmd.args(&["bundle.js"])
            .current_dir(&dir)
            .stdout(File::create(&stderr_path).unwrap())
            .spawn()
            .expect("bundle execution started");
        sleep(Duration::from_secs(1));

        let content = read_to_string(&stderr_path).expect("unable to read from stderr file");
        assert_eq!(content, "5\n");
    }

    #[test]
    fn generates_bundle_code1() {
        let (mut file, file_path, dir, _name) = create_tmp_file();
        let (mut file1, _, name1) = create_tmp_file_at_dir(&dir);
        let s = format!(
            r"import * as foo from './{}';
console.log(foo.foo)",
            &name1
        );
        file.write_all(s.as_bytes()).unwrap();
        writeln!(file1, "export const foo = 5;").unwrap();

        let src = generate_bundle(create_deps_tree(&file_path).unwrap());

        let mut bundle = File::create(dir.path().join("bundle.js")).expect("bundle.js created");
        bundle
            .write_all(src.as_bytes())
            .expect("bundle has written");

        let stderr_path = dir.path().join("stderr");
        let mut cmd = Command::new("node");
        cmd.args(&["bundle.js"])
            .current_dir(&dir)
            .stdout(File::create(&stderr_path).unwrap())
            .spawn()
            .expect("bundle execution started");
        sleep(Duration::from_secs(1));

        let content = read_to_string(&stderr_path).expect("unable to read from stderr file");
        assert_eq!(content, "5\n");
    }
}
