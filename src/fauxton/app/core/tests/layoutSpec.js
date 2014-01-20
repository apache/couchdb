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
  'testUtils'
], function (FauxtonAPI, testUtils) {
  var assert = testUtils.assert;

  describe("Faxuton Layout", function () {
    var layout;

    beforeEach(function () {
      layout = new FauxtonAPI.Layout();
    });

    describe('#setTemplate', function () {

      it("Should set template without prefix", function () {
        layout.setTemplate('myTemplate');

        assert.equal(layout.layout.template, 'templates/layouts/myTemplate');

      });

      it("Should set template with prefix", function () {
        layout.setTemplate({name: 'myTemplate', prefix: 'myPrefix/'});

        assert.equal(layout.layout.template, 'myPrefix/myTemplate');
      });

      it("Should remove old views", function () {
        var view = {
          remove: function () {}
        };

        layout.layoutViews = {
          'selector': view
        };

        var mockRemove = sinon.spy(view, 'remove');
        layout.setTemplate('myTemplate');
        assert.ok(mockRemove.calledOnce);

      });

      it("Should render", function () {
        var mockRender = sinon.spy(layout, 'render');

        layout.setTemplate('myTemplate');

        assert.ok(mockRender.calledOnce);

      });

    });

    describe('#renderView', function () {

      it('Should render existing view', function () {
        var view = new Backbone.View();
        var mockRender = sinon.spy(view, 'render');
        layout.layoutViews = {
          '#selector': view
        };

        var out = layout.renderView('#selector');

        assert.ok(mockRender.calledOnce);
      });

      it('Should return false for non-existing view', function () {
        var view = new Backbone.View();
        layout.layoutViews = {
          'selector': view
        };

        var out = layout.renderView('wrongSelector');
        assert.notOk(out, 'No view found');
      });
    });

  });
});
