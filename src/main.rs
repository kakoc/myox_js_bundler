#![cfg_attr(debug_assertions, allow(dead_code, unused_imports, warnings))]

use anyhow::Result;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::io;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::Mutex;
use swc_common::sync::Lrc;
use swc_common::Mark;
use swc_common::SourceFile;
use swc_common::Spanned;
use swc_common::{
    errors::{ColorConfig, Handler},
    FileName, FilePathMapping, SourceMap,
};
use swc_ecma_ast::AssignExpr;
use swc_ecma_ast::CallExpr;
use swc_ecma_ast::Expr;
use swc_ecma_ast::ExprOrSpread;
use swc_ecma_ast::ExprOrSuper;
use swc_ecma_ast::ExprStmt;
use swc_ecma_ast::FnDecl;
use swc_ecma_ast::FnExpr;
use swc_ecma_ast::Function;
use swc_ecma_ast::Ident;
use swc_ecma_ast::ImportDecl;
use swc_ecma_ast::Lit;
use swc_ecma_ast::Module;
use swc_ecma_ast::ModuleDecl;
use swc_ecma_ast::ModuleItem;
use swc_ecma_ast::Stmt;
use swc_ecma_ast::Str;
use swc_ecma_codegen::{text_writer, Config, Emitter};
use swc_ecma_parser::Input;
use swc_ecma_parser::JscTarget;
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax};
use swc_ecma_transforms::modules::common_js::common_js;
use swc_ecma_visit::noop_fold_type;
use swc_ecma_visit::noop_visit_type;
use swc_ecma_visit::Node;
use swc_ecma_visit::{Fold, FoldWith, Visit, VisitWith};
use uuid::Uuid;
// use testing::string_errors;

// mod emitter;

fn main() {
    // emitter::emmit();
    // let sm = init_source_map();
    // let lf = load_file(&sm, &PathBuf::from("test/index.js"));
    // let lexer = init_lexer(&lf);
    // let mut parser = init_parser(lexer);

    let root_with_deps_top_down = create_deps_tree(&PathBuf::from("index.js"));
    let u_root_with_deps_top_down = root_with_deps_top_down.unwrap();
    // dbg!(&u_root_with_deps_top_down);
    let src = generate_bundle(u_root_with_deps_top_down);

    std::fs::write("out", src);
}

pub fn init_source_map() -> Lrc<SourceMap> {
    Default::default()
}

pub fn load_file(sm: &SourceMap, path: &PathBuf) -> Lrc<SourceFile> {
    // dbg!(
    //     &path,
    //     path.is_dir(),
    //     path.extension(),
    //     path.ends_with(".js")
    // 	    &path.with_extension("js")
    // );
    if path.is_dir() {
        sm.load_file(&path.join("index.js")).expect(&format!(
            "failed to load {}",
            &path.to_str().expect("correct path")
        ))
    } else {
        if !path.to_str().expect("pathbuf to str").ends_with(".js") {
            // dbg!(
            //     &path,
            //     path.is_dir(),
            //     path.extension(),
            //     path.ends_with(".js"),
            //     &path.with_extension("js"),
            //     &PathBuf::from(format!("{}.js", path.to_str().expect("path to str")))
            // );

            sm.load_file(&PathBuf::from(format!(
                "{}.js",
                path.to_str().expect("path to str")
            )))
            .expect(&format!(
                "failed to load {}",
                &path.to_str().expect("correct path")
            ))
        } else {
            sm.load_file(path).expect(&format!(
                "failed to load {}",
                &path.to_str().expect("correct path")
            ))
        }
    }
}

pub fn init_lexer<'a>(input_file: &'a Lrc<SourceFile>) -> Lexer<'a, StringInput<'a>> {
    Lexer::new(
        Syntax::Es(Default::default()),
        JscTarget::Es3,
        StringInput::from(&**input_file),
        None,
    )
}

fn get_parsed_module<'a>(file_path: &PathBuf) -> Result<ParsedModule> {
    let sm = init_source_map();
    let lf = load_file(&sm.clone(), &file_path.clone());
    let lexer = init_lexer(&lf);
    let mut parser = init_parser(lexer);
    let res = parser.parse_module();

    analyze_module(res.unwrap().body, &file_path, sm)
}

pub fn init_parser<'a>(lexer: Lexer<'a, StringInput<'a>>) -> Parser<Lexer<'a, StringInput<'a>>> {
    Parser::new_from(lexer)
}

struct Test {
    imports: Vec<String>,
}
impl Fold for Test {
    noop_fold_type!();
    // fn fold_with(&mut self, n: T) -> T {
    //     let span = n.span();
    //     n.fold_children(self)
    // }
}

impl Visit for Test {
    noop_visit_type!();
    fn visit_module(&mut self, n: &Module, _parent: &dyn Node) {
        n.visit_children_with(self)
    }

    // fn visit_expr_stmt(&mut self, n: &ExprStmt, _parent: &dyn Node) {
    //     dbg!(n);
    //     n.visit_children_with(self)
    // }
    fn visit_fn_expr(&mut self, n: &FnExpr, _parent: &dyn Node) {
        // dbg!(n);
    }

    fn visit_fn_decl(&mut self, n: &FnDecl, _parent: &dyn Node) {
        // dbg!(n);
    }

    fn visit_function(&mut self, n: &Function, _parent: &dyn Node) {
        // dbg!(n);
    }

    fn visit_call_expr(&mut self, n: &CallExpr, _parent: &dyn Node) {
        // dbg!(n);
        match n {
            CallExpr {
                span,
                callee,
                args,
                type_args,
            } => match callee {
                ExprOrSuper::Expr(i) => match &**i {
                    Expr::Ident(Ident { sym, .. }) => {
                        if sym.to_string() == "require" {
                            if args.len() != 1 {
                                ()
                            } else {
                                match &args[0] {
                                    ExprOrSpread { expr, .. } => match &**expr {
                                        Expr::Lit(Lit::Str(Str { value, .. })) => {
                                            // println!("{}", &value.to_string());
                                            self.imports.push(value.to_string());
                                            ()
                                        }
                                        _ => (),
                                    },
                                    _ => (),
                                }
                            }
                        } else {
                            ()
                        }
                    }
                    _ => (),
                },
                _ => (),
            },
            _ => (),
        }

        n.visit_children_with(self)
    }

    fn visit_assign_expr(&mut self, n: &AssignExpr, _parent: &dyn Node) {
        // dbg!(n);
        match n {
            AssignExpr { right, .. } => match &**right {
                Expr::Call(CallExpr {
                    span,
                    callee,
                    args,
                    type_args,
                }) => match callee {
                    ExprOrSuper::Expr(i) => match &**i {
                        Expr::Ident(Ident { sym, .. }) => {
                            if sym.to_string() == "require".to_owned() {
                                if args.len() != 1 {
                                    ()
                                } else {
                                    match &args[0] {
                                        ExprOrSpread { expr, .. } => match &**expr {
                                            Expr::Lit(Lit::Str(Str { value, .. })) => {
                                                // println!("{}", &value.to_string());
                                                self.imports.push(value.to_string());
                                                ()
                                            }
                                            _ => (),
                                        },
                                        _ => (),
                                    }
                                };
                            } else {
                                ()
                            };
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
    // fn fold_with(&mut self, n: T) -> T {
    //     let span = n.span();
    //     n.fold_children(self)
    // }
}

#[derive(Debug, Clone)]
struct ParsedModule {
    pub id: String,
    pub abs_path: PathBuf,
    pub imports: Vec<PathBuf>,
    // pub items: Vec<ModuleItem>,
    pub source_code: String,
    pub deps_map: Option<std::collections::HashMap<String, String>>,
}
fn analyze_module(
    mut items: Vec<ModuleItem>,
    path_to_module: &PathBuf,
    sm: Lrc<SourceMap>,
) -> Result<ParsedModule> {
    let deps_to_process: Vec<ParsedModule> = Default::default();
    // dbg!(&path_to_module);
    // dbg!(&items);
    let mut imports: Vec<PathBuf> = items
        .iter()
        .map(|m| {
            let mut t = Test { imports: vec![] };
            m.visit_with(m, &mut t);
            t.imports
        })
        .flatten()
        .map(PathBuf::from)
        .collect();

    // dbg!(imports);

    let mut imported_files = items
        .iter()
        .filter_map(|item| -> Option<String> {
            match item {
                ModuleItem::ModuleDecl(ModuleDecl::Import(decl)) => {
                    Some(decl.src.value.to_string())
                }
                ModuleItem::Stmt(Stmt::Expr(ExprStmt { span, expr })) => {
                    match &**expr {
                        Expr::Call(CallExpr {
                            span,
                            callee,
                            args,
                            type_args,
                        }) => match callee {
                            ExprOrSuper::Expr(i) => match &**i {
                                Expr::Ident(Ident { sym, .. }) => {
                                    if sym.to_string() == "require".to_owned() {
                                        if args.len() != 1 {
                                            return None;
                                        } else {
                                            match &args[0] {
                                                ExprOrSpread { expr, .. } => match &**expr {
                                                    Expr::Lit(Lit::Str(Str { value, .. })) => {
                                                        println!("{}", &value.to_string());
                                                        return Some(value.to_string());
                                                    }
                                                    _ => return None,
                                                },
                                                _ => return None,
                                            }
                                        };
                                    } else {
                                        return None;
                                    };
                                }
                                _ => return None,
                            },
                            _ => return None,
                        },
                        _ => return None,
                    }
                    None
                }
                _ => None,
            }
        })
        .map(PathBuf::from)
        .collect::<Vec<PathBuf>>();

    // swc_common::GLOBALS.set(&swc_common::Globals::new(), || {});
    imported_files.append(&mut imports);
    let v: HashSet<&str> = imported_files
        .iter()
        .map(|v| v.to_str().unwrap())
        .collect::<HashSet<&str>>();
    imported_files = v.iter().map(|v| PathBuf::from(v)).collect::<Vec<PathBuf>>();
    // dbg!(&path_to_module.clone());
    // dbg!(&imported_files);

    // if path_to_module.clone().to_str().unwrap() == "node_modules/react/react.js" {
    // dbg!(&items);
    // }
    // dbg!(&items);
    let transpiled_src = js_to_common_js(items.clone(), sm);

    Ok(ParsedModule {
        id: Uuid::new_v4().to_hyphenated().to_string(),
        abs_path: path_to_module.clone(),
        imports: imported_files,
        // items,
        source_code: transpiled_src,
        deps_map: None,
    })
}

fn create_deps_tree(root: &PathBuf) -> Result<Vec<ParsedModule>> {
    let parsed_root = get_parsed_module(root)?;
    let mut modules: Vec<ParsedModule> = vec![parsed_root.clone()];
    let node_modules = scan_node_modules();

    let mut q: VecDeque<ParsedModule> = VecDeque::new();
    q.push_back(parsed_root);

    let mut m: HashMap<PathBuf, i64> = HashMap::new();
    let mut cache: HashMap<String, ParsedModule> = HashMap::new();
    while !q.is_empty() {
        let mut new_modules = vec![];
        for mut module in q.iter_mut() {
            dbg!(&module);
            // if cache.contains(&module.abs_path.clone().to_str().unwrap().to_string()) {
            //     continue;
            // }
            // cache.insert(module.abs_path.clone().to_str().unwrap().to_string());
            // let mut deps_map = std::collections::HashMap::new();
            // if module.deps_map.is_none() {
            //     module.deps_map = Some(deps_map);
            // }
            // if module.abs_path.to_str().unwrap() == "node_modules/react/react.js" {}
            // dbg!(&module);
            m.entry(module.abs_path.clone())
                .and_modify(|&mut v| {
                    v + 1;
                })
                .or_insert(1);
            for dep in module.imports.iter() {
                // dbg!(&dep);

                // if cache.contains(&dep.to_str().unwrap().to_string()) {
                //     continue;
                // }
                let dependency_path = {
                    if dep.is_absolute() {
                        return Err(anyhow::Error::msg(
                            "doesn't work with absolute paths".to_string(),
                        ));
                    } else {
                        if dep.to_str().expect("correct path") == "react" {
                            // PathBuf::from("node_modules/react/react")
                            PathBuf::from("node_modules/react/index.js")
                        } else if dep.to_str().expect("correct path") == "react-dom" {
                            // PathBuf::from("node_modules/react/lib/ReactDOM")
                            PathBuf::from("node_modules/react-dom/index.js")
                        } else if starts_from(&dep, &node_modules) {
                            PathBuf::from("node_modules").join(dep)
                        } else {
                            dbg!(&dep);
                            // let dir = get_path_to_file_parent_dir(root);
                            if module.abs_path.is_dir() {
                                module
                                    .abs_path
                                    .join(&dep.to_str().expect("import path to str converted")[2..])
                            } else {
                                let dir = get_path_to_file_parent_dir(&module.abs_path);
                                dir.join(&dep.to_str().expect("import path to str converted")[2..])
                            }
                            // let dir = get_path_to_file_parent_dir(root);
                        }
                    }
                };
                dbg!(&dependency_path);

                // if dependency_path.to_str().unwrap() == "node_modules/react/lib/js/lib/warning"
                //     || dep.to_str().unwrap() == "fbjs/lib/warning"
                // {}
                // dbg!(
                //     &module,
                //     dep,
                //     &node_modules,
                //     starts_from(&dep, &node_modules)
                // );

                let dep_info =
                    if cache.contains_key(&dependency_path.clone().to_str().unwrap().to_string()) {
                        cache
                            .get(&dependency_path.clone().to_str().unwrap().to_string())
                            .unwrap()
                            .clone()
                    } else {
                        let dep_info = get_parsed_module(&dependency_path)?;
                        cache.insert(
                            dep_info.abs_path.clone().to_str().unwrap().to_string(),
                            dep_info.clone(),
                        );
                        new_modules.push(dep_info.clone());

                        dep_info
                    };
                // let dep_info = get_parsed_module(&dependency_path)?;

                // if dep.to_str().unwrap() == "./ReactDOM" {
                //     // dbg!(&module.abs_path, &cache, &dep);
                //     dbg!(&module.abs_path, &dep, &dep_info);
                // }
                // module.deps_map.as_mut().unwrap().insert(
                //     dep.to_str().expect("converted").to_string(),
                //     dep_info.id.clone(),
                // );
                if let Some(m) = modules.iter_mut().find(|item| item.id == module.id) {
                    if m.deps_map.is_none() {
                        m.deps_map = Some(std::collections::HashMap::new());
                    }
                    m.deps_map.as_mut().unwrap().insert(
                        dep.to_str().expect("converted").to_string(),
                        dep_info.id.clone(),
                    );
                }

                // let ms = modules
                //     .iter()
                //     .filter(|item| item.abs_path == module.abs_path)
                //     .collect::<Vec<&ParsedModule>>();
                // if ms.len() > 1 {
                //     dbg!();
                // }

                // dbg!(&module);
                // cache.insert(dep_info.id.clone());
                // cache.insert(dep_info.abs_path.clone().to_str().unwrap().to_string());
                // ----------
                // new_modules.push(dep_info);
            }
        }
        modules.append(
            &mut new_modules
                .clone()
                .into_iter()
                // .filter(|m| !cache.contains(&m.id))
                .collect::<Vec<_>>(),
        );
        q = new_modules
            .into_iter()
            // .filter(|m| !cache.contains(&m.id))
            .collect::<VecDeque<ParsedModule>>();
    }

    Ok(modules)
}

fn scan_node_modules() -> HashSet<PathBuf> {
    let entries = std::fs::read_dir("node_modules").expect("node_modules are present");
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
              factory: (module, exports, require) => {{
                {module_source_code}
              }},
              map: {module_deps}
	    }}",
                //tmp
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
    window.process = {{env: {{NODE_ENV: 'production'}}}};
    const require = id => {{
      const {{factory, map}} = modules[id];
      const localRequire = requireDeclarationName => require(map[requireDeclarationName]);
      const module = {{exports: {{}}}};
      factory.call(module.exports, module, module.exports, localRequire);
      return module.exports;
    }}
    require(\"{}\");
  }})({{ {} }})
",
        modules_hierarchy.get(0).expect("source root is present").id,
        modules_executables
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
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        (*self.0.lock().unwrap()).write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        (*self.0.lock().unwrap()).flush()
    }
}

impl<W: Write> Clone for Output<W> {
    fn clone(&self) -> Self {
        Output(self.0.clone())
    }
}

pub fn js_to_common_js(module: Vec<ModuleItem>, sm: Lrc<SourceMap>) -> String {
    swc_common::GLOBALS.set(&swc_common::Globals::new(), || {
        let mark = Mark::fresh(Mark::root());
        let new = common_js(
            mark,
            swc_ecma_transforms::modules::util::Config {
                no_interop: true,
                ..Default::default()
            },
        )
        .fold_module_items(module);

        let out = Output(Rc::new(Mutex::new(vec![])));
        let module_to_transpilq = Module {
            body: new,
            span: Default::default(),
            shebang: None,
        };
        Emitter {
            cfg: Default::default(),
            cm: sm.clone(),
            wr: Box::new(text_writer::JsWriter::new(
                sm.clone(),
                "\n",
                out.clone(),
                None,
            )),
            comments: None,
        }
        .emit_module(&module_to_transpilq)
        .expect("module transpiled");

        let src = &*out.0.lock().unwrap();
        std::str::from_utf8(src)
            .expect("transpiled code converted")
            .to_string()
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;
    use std::io::{self, Write};
    use tempfile::{tempdir, TempDir};

    fn create_tmp_file() -> (File, PathBuf, TempDir, String) {
        let dir = tempdir().unwrap();

        let name = format!("{}{}", Uuid::new_v4().to_hyphenated().to_string(), ".js");
        let file_path = dir.path().join(&name);
        let mut file = File::create(&file_path).unwrap();

        (file, file_path, dir, name)
    }

    fn create_tmp_file_with_dir(dir: &TempDir) -> (File, PathBuf, String) {
        let name = format!("{}{}", Uuid::new_v4().to_hyphenated().to_string(), ".js");
        let file_path = dir.path().join(&name);
        let mut file = File::create(&file_path).unwrap();

        (file, file_path, name)
    }

    // fn init<'a>(
    //     file_path: &PathBuf,
    // 	fn: dyn fn() -> ()
    // ) -> (
    //     Parser<Lexer<'a, StringInput<'a>>>,
    //     Lexer<'a, StringInput<'a>>,
    //     Lrc<SourceFile>,
    // ) {
    //     let sm = init_source_map();
    //     let lf = load_file(&sm.clone(), &file_path.clone());
    //     let lexer = init_lexer(&lf.clone());
    //     let parser = init_parser(lexer);

    // }

    #[test]
    fn successful_parsing() {
        let (mut file, file_path, dir, _) = create_tmp_file();
        writeln!(file, "import {{foo}} from './foo.js';").unwrap();

        let sm = init_source_map();
        let lf = load_file(&sm, &file_path);
        let lexer = init_lexer(&lf);
        let mut parser = init_parser(lexer);

        let res = parser.parse_module();
        assert!(res.is_ok());
    }

    #[test]
    fn analyzes_module() {
        let (mut file, file_path, dir, name) = create_tmp_file();
        let (mut file1, file_path1, dir1, name1) = create_tmp_file();
        let s = format!(r"import {{foo}} from './{}';", &name); //&name1
        file.write_all(s.as_bytes()).unwrap();
        writeln!(file1, "export const foo = 5;").unwrap();

        let sm = init_source_map();
        let lf = load_file(&sm, &file_path);
        let lexer = init_lexer(&lf);
        let mut parser = init_parser(lexer);

        let res = parser.parse_module();
        let module = analyze_module(res.unwrap().body, &file_path, sm);
        assert!(module.is_ok());

        let u_module = module.unwrap();
        // dbg!(&u_module);
        assert_eq!(u_module.abs_path, file_path);
        assert_eq!(u_module.imports.len(), 1);
        assert_eq!(&u_module.imports[0].to_str().unwrap()[2..], &name);
    }

    #[test]
    fn builds_deps_hierarchy() {
        let (mut file, file_path, dir, name) = create_tmp_file();
        let (mut file1, file_path1, name1) = create_tmp_file_with_dir(&dir);
        let s = format!(r"import {{foo}} from './{}';", &name1); //&name1
        file.write_all(s.as_bytes()).unwrap();
        writeln!(file1, "export const foo = 5;").unwrap();
        // dbg!(&dir, &dir1);

        let sm = init_source_map();
        let lf = load_file(&sm, &file_path);
        let lexer = init_lexer(&lf);
        let mut parser = init_parser(lexer);

        let root_with_deps_top_down = create_deps_tree(&file_path);
        assert!(root_with_deps_top_down.is_ok());
        let u_root_with_deps_top_down = root_with_deps_top_down.unwrap();
        dbg!(&u_root_with_deps_top_down);
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
        let (mut file, file_path, dir, name) = create_tmp_file();
        let (mut file1, file_path1, name1) = create_tmp_file_with_dir(&dir);
        let s = format!(r"import {{foo}} from './{}';", &name1); //&name1
        file.write_all(s.as_bytes()).unwrap();
        writeln!(file1, "export const foo = 5;").unwrap();
        // dbg!(&dir, &dir1);

        let sm = init_source_map();
        let lf = load_file(&sm, &file_path);
        let lexer = init_lexer(&lf);
        let mut parser = init_parser(lexer);

        let root_with_deps_top_down = create_deps_tree(&file_path);
        let u_root_with_deps_top_down = root_with_deps_top_down.unwrap();
        let src = generate_bundle(u_root_with_deps_top_down);
    }
}

// dbg!(std::fs::read_to_string(&file_path).unwrap());
