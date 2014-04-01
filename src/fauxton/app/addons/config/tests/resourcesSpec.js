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
      'addons/config/views',
      'testUtils'
], function (FauxtonAPI, Resources, Views, testUtils) {
  var assert = testUtils.assert,
      ViewSandbox = testUtils.ViewSandbox;

  describe("Config: TableRow", function () {
    var tabMenu, optionModel;

    beforeEach(function () {
      optionModel = new Resources.OptionModel({
        section: "foo",
        name: "bar"
      });

      tabMenu = new Views.TableRow({
        model: optionModel,
        uniqueName: function () {
          return false;
        }
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

        var $fields = tabMenu.$('.js-edit-value').filter(function (el) {
          return $(this).find('[name="value"]').length;
        });

        $fields.find('.js-edit-value').trigger('dblclick');
        $fields.find('.js-save-value').trigger('click');

        assert.ok(renderSpy.calledOnce);
        assert.ok(saveSpy.calledOnce);
      });

      it("pressing enter should save the model and render", function () {
        var renderSpy = sinon.stub(tabMenu, 'render');
        var saveSpy = sinon.stub(optionModel, 'save');

        var e = $.Event("keyup");
        e.keyCode = 13;

        var $fields = tabMenu.$('.js-edit-value').filter(function (el) {
          return $(this).find('[name="value"]').length;
        });

        $fields.find('.js-value-input').trigger(e);

        assert.ok(renderSpy.calledOnce);
        assert.ok(saveSpy.calledOnce);
      });

      it("pressing Esc hides the field", function () {
        var e = $.Event("keyup");
        e.keyCode = 27;
        tabMenu.$('.js-value-input').trigger(e);

        assert.ok(tabMenu.$('.js-edit-value-form').hasClass('js-hidden'));
      });

      it("pressing Cancel hides the field", function () {
        tabMenu.$('.js-edit-value').trigger('dblclick');
        tabMenu.$('.js-cancel-value').trigger('click');

        assert.ok(tabMenu.$('.js-edit-value-form').hasClass('js-hidden'));
      });
    });
  });
});
