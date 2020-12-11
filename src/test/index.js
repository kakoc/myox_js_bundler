// import { foo } from "./index2.js";
// import { bar } from "./index3.js";
// import baz from "./index4.js";

// console.log(foo + bar + baz);

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
