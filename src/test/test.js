 const fs = require("fs");
 const path = require("path");
 const parser = require("@babel/parser");
 const traverse = require("@babel/traverse").default;
 const babel = require("@babel/core");
 const resolve = require("resolve").sync;

let ID = 0;

function createModuleInfo(filePath) {
  const content = fs.readFileSync(filePath, "utf-8");
  const ast = parser.parse(content, {
    sourceType: "module"
  });
  const deps = [];
  traverse(ast, {
    ImportDeclaration: ({ node }) => {
	// console.log(node);
      deps.push(node.source.value);
    }
  });
  const id = ID++;
  const { code } = babel.transformFromAstSync(ast, null, {
    presets: ["@babel/preset-env"]
  });

    // console.log(code);

  //   console.log(
  // 	{
  //   id,
  //   filePath,
  //   deps,
  //   code
  // }
  //   );

  return {
    id,
    filePath,
    deps,
    code
  };
}

function createDependencyGraph(entry) {
  const entryInfo = createModuleInfo(entry);
  const graphArr = [];
  graphArr.push(entryInfo);
  for (const module of graphArr) {
    module.map = {};
    module.deps.forEach(depPath => {
      const baseDir = path.dirname(module.filePath);
      const moduleDepPath = resolve(depPath, { baseDir });
      const moduleInfo = createModuleInfo(moduleDepPath);
      graphArr.push(moduleInfo);
      module.map[depPath] = moduleInfo.id;
    });
  }
  return graphArr;
}

function pack(graph) {
  const moduleArgArr = graph.map(module => {
    return `${module.id}: {
      factory: (exports, require) => {
        ${module.code}
      },
      map: ${JSON.stringify(module.map)}
    }`;
  });
  const iifeBundler = `(function(modules){
    const require = id => {
      const {factory, map} = modules[id];
      const localRequire = requireDeclarationName => require(map[requireDeclarationName]);
      const module = {exports: {}};
      factory(module.exports, localRequire);
      return module.exports;
    }
    require(0);
  })({${moduleArgArr.join()}})
  `;
  return iifeBundler;
}

///////////////////////

const graph = createDependencyGraph("./index.js"); // wherever your entry is

/* create bundle based on dependency graph */
const bundle = pack(graph);

console.log(bundle);




////////////////////////
const entryInfo = createModuleInfo('./index.js');
