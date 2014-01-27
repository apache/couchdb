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
  'testUtils',
  'api',
  'addons/fauxton/base',
  "backbone"
], function (testUtils, FauxtonAPI, Base) {
  var assert = testUtils.assert;


  describe('Fauxton RouteObject:beforeEstablish', function () {
    var TestRouteObject, testRouteObject, mockLayout, _layout;

    before(function () {
      Base.initialize();
      _layout = FauxtonAPI.masterLayout;
    });

    beforeEach(function () {
      TestRouteObject = FauxtonAPI.RouteObject.extend({
        crumbs: ['mycrumbs']
      });

      testRouteObject = new TestRouteObject();
      var apiBar = {};
      apiBar.hide = sinon.spy();
      var setViewSpy = sinon.stub();
      setViewSpy.returns({
        render: function () {}
      });

      // Need to find a better way of doing this
      mockLayout = {
        setTemplate: sinon.spy(),
        clearBreadcrumbs: sinon.spy(),
        setView: setViewSpy,
        renderView: sinon.spy(),
        removeView: sinon.spy(),
        hooks: [],
        setBreadcrumbs: sinon.spy(),
        apiBar: apiBar,
        layout: { 
          setView: function () {}
        }
      };


    });

    after(function () {
      FauxtonAPI.masterLayout = _layout;
    });

    it('Should clear breadcrumbs', function () {
      FauxtonAPI.masterLayout = mockLayout;
      testRouteObject.renderWith('the-route', mockLayout, 'args');
      assert.ok(mockLayout.removeView.calledWith('#breadcrumbs'), 'Clear Breadcrumbs called');
    });

    it('Should set breadcrumbs when breadcrumbs exist', function () {
      FauxtonAPI.masterLayout = mockLayout;
      testRouteObject.renderWith('the-route', mockLayout, 'args');
      assert.ok(mockLayout.setView.calledOnce, 'Set Breadcrumbs was called');
    });

  });


});
