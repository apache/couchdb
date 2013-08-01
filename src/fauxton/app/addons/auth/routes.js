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
       "addons/auth/resources"
],

function(app, FauxtonAPI, Auth) {

  var authRouteObject = FauxtonAPI.RouteObject.extend({
    layout: 'one_pane',

    routes: {
      'login': 'login',
      'logout': 'logout',
      'changePassword': {
        route: 'changePassword',
        roles: ['_admin', '_reader', '_replicator']
      },
      'createAdmin': 'createAdmin',
      'addAdmin': {
        roles: ['_admin'],
        route: 'addAdmin',
      },
      'noAccess': 'noAccess'
    },

    login: function () {
      this.crumbs = [{name: 'Login', link:"#"}];
      this.setView('#dashboard-content', new Auth.LoginView({model: FauxtonAPI.session}));
    },
    logout: function () {
      FauxtonAPI.addNotification({msg: 'You have been logged out.'});
      FauxtonAPI.session.logout().then(function () {
        FauxtonAPI.navigate('/');
      });
    },

    changePassword: function () {
      this.crumbs = [{name: 'Change Password', link:"#"}];
      this.setView('#dashboard-content', new Auth.ChangePassword({model: FauxtonAPI.session}));
    },

    createAdmin: function () {
      this.crumbs = [{name: 'Create Admin', link:"#"}];
      this.setView('#dashboard-content', new Auth.CreateAdminView({model: FauxtonAPI.session}));
    },

    addAdmin: function () {
      this.crumbs = [{name: 'Add Admin', link:"#"}];
      this.setView('#dashboard-content', new Auth.CreateAdminView({login_after: false, model: FauxtonAPI.session}));
    },

    noAccess: function () {
      this.crumbs = [{name: 'Access Denied', link:"#"}];
      this.setView('#dashboard-content', new Auth.NoAccessView());
    },

  });

  Auth.RouteObjects = [authRouteObject];
  
  return Auth;
});
