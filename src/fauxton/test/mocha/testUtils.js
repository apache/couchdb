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

define([
       "chai",
       "sinon-chai",
       "underscore"
],
function(chai, sinonChai) {
  chai.use(sinonChai);

  var ViewSandbox = function () {
    this.initialize();
  };
   
   _.extend(ViewSandbox.prototype, {
    initialize: function () {
      this.$el = $('<div style="display:none"></div>').appendTo('body');
      this.$ = this.$el.find;
    },
    views: [],
    renderView: function (view) {
      this.views.push(view);
      this.$el.append(view.el);
      view.render();
    },

    remove: function () {
      _.each(this.views, function (view) {
        view.remove();
      }, this);
    }
  });

  return {
    chai: chai,
    assert: chai.assert,
    ViewSandbox: ViewSandbox
  };
});

