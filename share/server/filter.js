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

var Filter = (function() {

  var view_emit = false;

  return {
      emit : function(key, value) {
        view_emit = true;
      },
      filter : function(fun, ddoc, args) {
        var results = [];
        var docs = args[0];
        var req = args[1];
        for (var i=0; i < docs.length; i++) {
          results.push((fun.apply(ddoc, [docs[i], req]) && true) || false);
        };
        respond([true, results]);
      },
      filter_view : function(fun, ddoc, args) {
        // recompile
        var source = fun.toSource();
        fun = evalcx(source, filter_sandbox);

        var results = [];
        var docs = args[0];
        for (var i=0; i < docs.length; i++) {
          view_emit = false;
          fun(docs[i]);
          results.push((view_emit && true) || false);
        };
        respond([true, results]);
      }
    }
})();
