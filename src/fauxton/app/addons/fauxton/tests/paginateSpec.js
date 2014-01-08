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
       'app',
       'addons/fauxton/components',
       'addons/documents/resources',
       'testUtils',
       'api'
], function (app, Views, Models, testUtils, FauxtonAPI) {
  var assert = testUtils.assert,
  ViewSandbox = testUtils.ViewSandbox;


  describe('IndexPaginate', function () {
    var viewSandbox, paginate, collection, navigateMock;
    beforeEach(function () {
      collection = new Models.IndexCollection([{
        id:'myId1',
        doc: 'num1'
      },
      {
        id:'myId2',
        doc: 'num2'
      }], {
        database: {id: 'databaseId'},
        design: '_design/myDoc'
      });

      paginate = new Views.IndexPagination({
        collection: collection,
        previousUrlfn: function () {},
        nextUrlfn: function () {},
        canShowPreviousfn: function () { return true; },
        canShowNextfn: function () { return true;}
      });
      viewSandbox = new ViewSandbox();
      viewSandbox.renderView(paginate); 
    });

    afterEach(function () {
      viewSandbox.remove();
    });

    describe('#next', function () {
      beforeEach(function () {
        //do this so it doesn't throw an error on other unwired up components
        FauxtonAPI.triggerRouteEvent = function () {};
        //FauxtonAPI.triggerRouteEvent.restore && FauxtonAPI.triggerRouteEvent.restore();
        //FauxtonAPI.navigate.restore && FauxtonAPI.navigate.restore(); 
      });

      it('Should navigate', function () {
        var navigateMock = sinon.spy(FauxtonAPI, 'navigate');

        paginate.$('a#next').click();

        assert.ok(navigateMock.calledOnce);
        FauxtonAPI.navigate.restore();
      });

      it('Should trigger routeEvent', function () {
        var navigateMock = sinon.spy(FauxtonAPI, 'triggerRouteEvent');

        paginate.$('a#next').click();

        assert.ok(navigateMock.calledOnce);
        FauxtonAPI.triggerRouteEvent.restore();
      });

    });


    describe('#previous', function () {

      it('Should navigate', function () {
        var navigateMock = sinon.spy(FauxtonAPI, 'navigate');

        paginate.$('a#previous').click();

        assert.ok(navigateMock.calledOnce);
        FauxtonAPI.navigate.restore();
      });

      it('Should trigger routeEvent', function () {
        var navigateMock = sinon.spy(FauxtonAPI, 'triggerRouteEvent');

        paginate.$('a#previous').click();

        assert.ok(navigateMock.calledOnce);
        FauxtonAPI.triggerRouteEvent.restore();
      });

    });

  });
});
