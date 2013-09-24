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
       'addons/permissions/resources',
      'testUtils'
], function (FauxtonAPI, Models, testUtils) {
  var assert = testUtils.assert;

  describe('Permissions', function () {

    describe('#addItem', function () {
      var security;
      
      beforeEach(function () {
        security = new Models.Security(null, {database: 'fakedb'});
      });

      it('Should add value to section', function () {

        security.addItem('_user', 'names', 'admins');
        assert.equal(security.get('admins').names[0], '_user');
      });

      it('Should handle incorrect type', function () {
        security.addItem('_user', 'asdasd', 'admins');
      });

      it('Should handle incorrect section', function () {
        security.addItem('_user', 'names', 'Asdasd');
      });

      it('Should reject duplicates', function () {
        security.addItem('_user', 'names', 'admins');
        security.addItem('_user', 'names', 'admins');
        assert.equal(security.get('admins').names.length, 1);
      });
    });

  });

});
