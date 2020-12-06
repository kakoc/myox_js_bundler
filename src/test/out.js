(function(modules){
    const require = id => {
      const {factory, map} = modules[id];
      const localRequire = requireDeclarationName => require(map[requireDeclarationName]);
      const module = {exports: {}};
      factory(module.exports, localRequire);
      return module.exports;
    }
    require(0);
  })({0: {
      factory: (exports, require) => {
        "use strict";

var _index = require("./index2.js");

var _index2 = require("./index3.js");
      },
      map: {"./index2.js":1,"./index3.js":2}
    },1: {
      factory: (exports, require) => {
        "use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.test = void 0;

var _index = require("./index4.js");

var test = 2;
exports.test = test;
      },
      map: {"./index4.js":3}
    },2: {
      factory: (exports, require) => {
        "use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.bar = void 0;
var bar = 5;
exports.bar = bar;
      },
      map: {}
    },3: {
      factory: (exports, require) => {
        "use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.foofoo = void 0;
var foofoo = 7;
exports.foofoo = foofoo;
      },
      map: {}
    }})
  
