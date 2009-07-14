// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

(function($) {
  $.futon = $.futon || {};
  $.extend($.futon, {

    // JSON pretty printing
    formatJSON: function(val, options) {
      options = $.extend({
        indent: 4,
        linesep: "\n",
        quoteKeys: true
      }, options || {});
      var itemsep = options.linesep.length ? "," + options.linesep : ", ";

      function escape(string) {
        return string.replace(/&/g, "&amp;")
                     .replace(/</g, "&lt;")
                     .replace(/>/g, "&gt;");
      }

      function format(val, depth) {
        var tab = [];
        for (var i = 0; i < options.indent * depth; i++) tab.push("");
        tab = tab.join(" ");

        var type = typeof val;
        switch (type) {
          case "boolean":
          case "number":
          case "string":
            var retval = JSON.stringify(val);
            if (options.html) {
              retval = "<code class='" + type + "'>" + escape(retval) + "</code>";
            }
            return retval;

          case "object": {
            if (val === null) {
              if (options.html) {
                return "<code class='null'>null</code>";
              }
              return "null";
            }
            if (val.constructor == Date) {
              return JSON.stringify(val);
            }

            var buf = [];

            if (val.constructor == Array) {
              buf.push("[");
              for (var index = 0; index < val.length; index++) {
                buf.push(index > 0 ? itemsep : options.linesep);
                buf.push(tab, format(val[index], depth + 1));
              }
              if (index >= 0) buf.push(options.linesep, tab.substr(options.indent));
              buf.push("]");

            } else {
              buf.push("{");
              var index = 0;
              for (var key in val) {
                buf.push(index > 0 ? itemsep : options.linesep);
                var keyDisplay = options.quoteKeys ? JSON.stringify(key) : key;
                if (options.html) {
                  if (options.quoteKeys) {
                    keyDisplay = keyDisplay.substr(1, keyDisplay.length - 2);
                  }
                  keyDisplay = "<code class='key'>" + escape(keyDisplay) + "</code>";
                  if (options.quoteKeys) {
                    keyDisplay = '"' + keyDisplay + '"';
                  }
                }
                buf.push(tab, keyDisplay,
                  ": ", format(val[key], depth + 1));
                index++;
              }
              if (index >= 0) buf.push(options.linesep, tab.substr(options.indent));
              buf.push("}");
            }

            return buf.join("");
          }
        }
      }

      return format(val, 1);
    },

    // File size pretty printing
    formatSize: function(size) {
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

  });
})(jQuery);
