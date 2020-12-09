// import { foo } from "./index2.js";
// import { bar } from "./index3.js";
// import baz from "./index4.js";

// console.log(foo + bar + baz);

// let a = require("./index2.js");
// console.log(a);

// // let fs = import('fs');
// // import * as fs from 'fs';
// const process = require('process');

// // let file = fs.readFileSync('./index2.js');
// console.log(process.getuid());

// ----------
import * as React from "react";
import * as ReactDOM from "react-dom";

class Hello extends React.Component {
  render() {
    return React.createElement("div", null, `Hello ${this.props.toWhat}`);
  }
}

ReactDOM.render(
  React.createElement(Hello, { toWhat: "World" }, null),
  document.getElementById("root")
);
// ---------

// require("react");
