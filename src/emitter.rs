use std::{path::Path, sync::Arc};
use swc_common::sync::Lrc;
use swc_common::{
    errors::{ColorConfig, Handler},
    FileName, FilePathMapping, Mark, SourceMap,
};
use swc_ecma_ast::Module;
use swc_ecma_codegen::{text_writer, Config, Emitter};
use swc_ecma_parser::{lexer::Lexer, JscTarget, Parser, StringInput, Syntax};
// use testing::string_errors;
use scoped_tls::scoped_thread_local;
use swc_ecma_transforms::{helpers::Helpers, modules::common_js::common_js};
use swc_ecma_visit::{Fold, FoldWith};

pub fn emmit() {
    // enable_helper
    let result = swc_common::GLOBALS.set(&swc_common::Globals::new(), || {
        // scoped_thread_local!(pub static HELPERS: Helpers);

        let cm: Lrc<SourceMap> = Default::default();
        let fm = cm
            .load_file(Path::new("./index.js"))
            .expect("failed to load sample.ts");

        let lexer = Lexer::new(
            Syntax::Es(Default::default()),
            JscTarget::Es3,
            StringInput::from(&*fm),
            None,
        );

        let mut parser = Parser::new_from(lexer);
        let res = parser.parse_module().unwrap();
        dbg!(&res);

        let used_mark = Mark::fresh(Mark::root());
        let new = common_js(
            used_mark,
            swc_ecma_transforms::modules::util::Config {
                no_interop: true,
                strict: true,
                ..Default::default()
            },
        )
        .fold_module_items(res.body);

        // res.fold_with(&mut common_js(Default::default(), Default::default()));
        let m = Mark::root();

        let new_m = Module {
            body: new,
            span: Default::default(),
            shebang: None, // ..Default::default()
        };

        // let out = parse_then_emit(from, Config { minify: true }, Syntax::default());

        let f = std::fs::File::create("./t.js").unwrap();
        let r = Emitter {
            cfg: Default::default(),
            cm: cm.clone(),
            wr: Box::new(text_writer::JsWriter::new(cm.clone(), "\n", f, None)),
            comments: None,
        }
        .emit_module(&new_m)
        .unwrap();
    });
}
