se anyhow::{Error, Result};
use std::path::Path;
use swc_common::sync::Lrc;
use swc_common::{
    errors::{ColorConfig, Handler},
    FileName, FilePathMapping, SourceFile, SourceMap,
};
use swc_ecma_ast::{ImportDecl, Module, ModuleDecl, ModuleItem, Program};
use swc_ecma_codegen::{text_writer, Config, Emitter};
use swc_ecma_parser::{lexer::Lexer, JscTarget, Parser, StringInput, Syntax};
// use testing::string_errors;

pub fn bundle() {
    let res = parse_js("test/index.js").unwrap();
    res.body
        .iter()
        .filter_map(|item| match item {
            ModuleItem::ModuleDecl(ModuleDecl::Import(decl)) => Some(decl),
            _ => None,
        })
        .collect::<Vec<&ImportDecl>>();

    println!("{:#?}", res.body);
}

pub fn parse_js(path: &str) -> Result<Module> {
    let cm: Lrc<SourceMap> = Default::default();
    let fm = cm
        .load_file(Path::new(path))
        .expect("failed to load index.js");
    let lexer = Lexer::new(
        Syntax::Es(Default::default()),
        Default::default(),
        StringInput::from(&*fm),
        None,
    );
    Parser::new_from(lexer)
        .parse_module()
        .map_err(|_| Error::msg("failed to parse module"))
}

struct ModuleData {
    pub fm: Lrc<SourceFile>,
    pub module: Module,
}

struct Compiler {}
impl Compiler {
    pub fn print() {}
}

struct Loader {}
impl Loader {
    pub fn new(compiler: Compiler) {}

    // fn load(&self, name: &String) -> Result<ModuleData, Error> {
    //     let fm = self
    //         .compiler
    //         .cm
    //         .load_file(name)
    //         .with_context(|| format!("failed to load file `{}`", name))?;

    //     let program = {
    //         let mut config = self.compiler.config_for_file(
    //             &swc::config::Options {
    //                 config: {
    //                     if let Some(c) = &self.options.config {
    //                         Some(swc::config::Config {
    //                             jsc: JscConfig {
    //                                 transform: {
    //                                     if let Some(c) = &c.jsc.transform {
    //                                         Some(TransformConfig {
    //                                             react: c.react.clone(),
    //                                             const_modules: c.const_modules.clone(),
    //                                             optimizer: None,
    //                                             legacy_decorator: c.legacy_decorator,
    //                                             decorator_metadata: c.decorator_metadata,
    //                                             hidden: Default::default(),
    //                                         })
    //                                     } else {
    //                                         None
    //                                     }
    //                                 },
    //                                 external_helpers: true,
    //                                 ..c.jsc
    //                             },
    //                             module: None,
    //                             minify: Some(false),
    //                             ..c.clone()
    //                         })
    //                     } else {
    //                         None
    //                     }
    //                 },
    //                 skip_helper_injection: true,
    //                 disable_hygiene: false,
    //                 disable_fixer: true,
    //                 global_mark: self.options.global_mark,
    //                 cwd: self.options.cwd.clone(),
    //                 caller: None,
    //                 filename: String::new(),
    //                 config_file: None,
    //                 root: None,
    //                 root_mode: Default::default(),
    //                 swcrc: true,
    //                 swcrc_roots: Default::default(),
    //                 env_name: {
    //                     let s = env::var("NODE_ENV").unwrap_or_else(|_| "development".into());
    //                     s
    //                 },
    //                 input_source_map: InputSourceMap::Bool(false),
    //                 source_maps: None,
    //                 source_file_name: None,
    //                 source_root: None,
    //                 is_module: true,
    //             },
    //             &fm.name,
    //         )?;

    //         log::trace!("JsLoader.load: loaded config");

    //         // We run transform at this phase to strip out unused dependencies.
    //         //
    //         // Note that we don't apply compat transform at loading phase.
    //         let program =
    //             self.compiler
    //                 .parse_js(fm.clone(), JscTarget::Es2020, config.syntax, true, true)?;

    //         log::trace!("JsLoader.load: parsed");

    //         // Fold module
    //         let program = helpers::HELPERS.set(&helpers, || {
    //             swc_ecma_utils::HANDLER.set(&self.compiler.handler, || {
    //                 let program =
    //                     program.fold_with(&mut inline_globals(env_map(), Default::default()));
    //                 let program = program.fold_with(&mut expr_simplifier());
    //                 let program = program.fold_with(&mut dead_branch_remover());

    //                 let program = program.fold_with(&mut config.pass);

    //                 program
    //             })
    //         });

    //         program
    //     };

    //     match program {
    //         Program::Module(module) => Ok(ModuleData {
    //             fm,
    //             module,
    //             helpers,
    //         }),
    //         _ => unreachable!(),
    //     }
    // }
}

struct Bundler {}
impl Bundler {
    pub fn new() {}

    pub fn bundle(&self, fname: String, fpath: String) {}

    pub fn load_transformed(&self, file_name: &String) {}

    pub fn analyze() {}
}
