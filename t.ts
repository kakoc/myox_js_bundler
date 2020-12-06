"use strict";
var _index2Js = require("./index2.js");
var _index3Js = require("./index3.js");
var React = require("react");
var ReactDOM = require("react-dom");
class Hello extends React.Component {
    render() {
        return React.createElement('div', null, `Hello ${this.props.toWhat}`);
    }
}
ReactDOM.render(React.createElement(Hello, {
    toWhat: 'World'
}, null), document.getElementById('root'));
