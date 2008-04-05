// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License.  You may obtain a copy
// of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
// License for the specific language governing permissions and limitations under
// the License.

// JSON pretty printing

function prettyPrintJSON(val, indent, linesep, depth) {
  indent = indent != null ? indent : 4;
  linesep = linesep != null ? linesep : "\n";
  depth = depth != null ? depth : 1;
  var propsep = linesep.length ? "," + linesep : ", ";
  var tab = [];
  for (var i = 0; i < indent * depth; i++) tab.push("");
  tab = tab.join(" ");
  switch (typeof val) {
    case "boolean":
    case "number":
    case "string":
      return JSON.stringify(val);
    case "object": {
      if (val === null) return "null";
      if (val.constructor == Date) return JSON.stringify(val);
      var buf = [];
      if (val.constructor == Array) {
        buf.push("[");
        for (var index = 0; index < val.length; index++) {
          buf.push(index > 0 ? propsep : linesep);
          buf.push(
            tab, prettyPrintJSON(val[index], indent, linesep, depth + 1)
          );
        }
        if (index >= 0) buf.push(linesep, tab.substr(indent));
        buf.push("]");
      } else {
        buf.push("{");
        var index = 0;
        for (var key in val) {
          if (!val.hasOwnProperty(key)) continue;
          buf.push(index > 0 ? propsep : linesep);
          buf.push(
            tab, JSON.stringify(key), ": ",
            prettyPrintJSON(val[key], indent, linesep, depth + 1)
          );
          index++;
        }
        if (index >= 0) buf.push(linesep, tab.substr(indent));
        buf.push("}");
      }
      return buf.join("");
    }
  }
}

// File size pretty printing

function prettyPrintSize(size) {
  var jump = 512;
  if (size < jump) return size + " bytes";
  var units = ["KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"];
  var i = 0;
  while (size >= jump && i < units.length) {
    i += 1;
    size /= 1024
  }
  return size.toFixed(1) + ' ' + units[i - 1];
}
