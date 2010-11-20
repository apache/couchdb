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

var State = {
  reset : function(config) {
    // clear the globals and run gc
    State.funs = [];
    State.lib = null;
    State.query_config = config || {};
    init_sandbox();
    gc();
    print("true"); // indicates success
  },
  addFun : function(newFun) {
    // Compile to a function and add it to funs array
    State.funs.push(Couch.compileFunction(newFun, {views : {lib : State.lib}}));
    print("true");
  },
  addLib : function(lib) {
    State.lib = lib;
    print("true");
  }
};
