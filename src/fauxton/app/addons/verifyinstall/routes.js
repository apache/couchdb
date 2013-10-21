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
  "app",
  "api",
  "addons/verifyinstall/views"
],
function(app, FauxtonAPI, VerifyInstall) {

  var VerifyRouteObject = FauxtonAPI.RouteObject.extend({
    layout: 'one_pane',

    routes: {
      'verifyinstall': "verifyInstall"
    },
    selectedHeader: "Verify",

    verifyInstall: function () {
      this.setView('#dashboard-content', new VerifyInstall.Main({}));
    },

    crumbs: [{name: 'Verify Couchdb Installation', link: '#'}]
  });

  VerifyInstall.RouteObjects = [VerifyRouteObject];
  return VerifyInstall;
});
