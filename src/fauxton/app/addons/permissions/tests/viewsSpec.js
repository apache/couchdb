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
       'addons/permissions/views',
       'addons/permissions/resources',
       'testUtils'
], function (FauxtonAPI, Views, Models, testUtils) {
  var assert = testUtils.assert,
      ViewSandbox = testUtils.ViewSandbox;

  describe('Permission View', function () {
    var security, section, viewSandbox;

    beforeEach(function () {
      security = new Models.Security({'admins': {
        'names': ['_user'],
        'roles': []
      }
      }, {database: {id: 'fakedb', safeID: function () { return this.id; }}});

      section = new Views.Permissions({
        database: 'fakedb',
        model: security
      });

      viewSandbox = new ViewSandbox();
      viewSandbox.renderView(section); 
    });

    afterEach(function () {
      viewSandbox.remove();
    });

    describe('itemRemoved', function () {

      it('Should set model', function () {
        var saveMock = sinon.spy(security, 'set');
        Views.events.trigger('itemRemoved');

        assert.ok(saveMock.calledOnce);
        var args = saveMock.args; 
        assert.deepEqual(args[0][0], {"admins":{"names":["_user"],"roles":[]},"members":{"names":[],"roles":[]}});
      });

      it('Should save model', function () {
        var saveMock = sinon.spy(security, 'save');
        Views.events.trigger('itemRemoved');

        assert.ok(saveMock.calledOnce);
      });
    });

  });

  describe('PermissionsSection', function () {
    var section,
        security,
        viewSandbox;

    beforeEach(function () {
      security = new Models.Security({'admins': {
        'names': ['_user'],
        'roles': []
      }
      }, {database: 'fakedb'});

      section = new Views.PermissionSection({
        section: 'admins',
        model: security
      });

      viewSandbox = new ViewSandbox();
      viewSandbox.renderView(section); 
    });

    afterEach(function () {
      viewSandbox.remove();
    });

    describe('#discardRemovedViews', function () {
      it('Should not filter out active views', function () {
        section.discardRemovedViews();

        assert.equal(section.nameViews.length, 1);

      });

      it('Should filter out removed views', function () {
        section.nameViews[0].removed = true;
        section.discardRemovedViews();

        assert.equal(section.nameViews.length, 0);

      });

    });

    describe('#getItemFromView', function () {

      it('Should return item list', function () {
        var items = section.getItemFromView(section.nameViews);

        assert.deepEqual(items, ['_user']);
      });

    });

    describe('#addItems', function () {

      it('Should add item to model', function () {
        //todo add a test here

      });

    });

  });

  describe('PermissionItem', function () {
    var item,
        viewSandbox;

    beforeEach(function () {
      item = new Views.PermissionItem({
        item: '_user'
      });

      viewSandbox = new ViewSandbox();
      viewSandbox.renderView(item); 
    });

    afterEach(function () {
      viewSandbox.remove();
    });

    it('should trigger event on remove item', function () {
      var eventSpy = sinon.spy();

      Views.events.on('itemRemoved', eventSpy);

      item.$('.close').click();
      
      assert.ok(eventSpy.calledOnce); 
    });

    it('should set removed to true', function () {
      item.$('.close').click();
      
      assert.ok(item.removed); 
    });
  });

});
