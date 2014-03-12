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
      'api',
      'addons/config/resources',
      'testUtils'
], function (FauxtonAPI, Resources, testUtils) {
  var assert = testUtils.assert,
      ViewSandbox = testUtils.ViewSandbox;

  describe("ViewItem", function () {
    var tabMenu, optionModel;

    beforeEach(function () {
      optionModel = new Resources.OptionModel({
        section: "foo",
        name: "bar"
      });

      tabMenu = new Resources.ViewItem({
        model: optionModel
      });
    });

    describe("editing Items", function () {
      var viewSandbox;
      beforeEach(function () {
        viewSandbox = new ViewSandbox();
        viewSandbox.renderView(tabMenu);
      });

      afterEach(function () {
        viewSandbox.remove();
      });

      it("click on save should save the model and render", function () {
        var renderSpy = sinon.stub(tabMenu, 'render');
        var saveSpy = sinon.stub(optionModel, 'save');

        tabMenu.$('.js-edit-value').trigger('dblclick');
        tabMenu.$('.js-save-value').trigger('click');

        assert.ok(renderSpy.calledOnce);
        assert.ok(saveSpy.calledOnce);
      });
    });
  });
});
