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
       "core/base",
       "core/layout",
       "core/router",
       "core/routeObject",
       "core/couchdbSession",
       "core/utils"
],

function(FauxtonAPI, Layout, Router, RouteObject, CouchdbSession, utils) {
  FauxtonAPI = _.extend(FauxtonAPI, {
    Layout: Layout,
    Router: Router,
    RouteObject: RouteObject,
    utils: utils
  });

  FauxtonAPI.navigate = function(url, _opts) {
    var options = _.extend({trigger: true}, _opts );
    FauxtonAPI.router.navigate(url,options);
  };

  FauxtonAPI.beforeUnload = function () {
    FauxtonAPI.router.beforeUnload.apply(FauxtonAPI.router, arguments);
  };

  FauxtonAPI.removeBeforeUnload = function () {
    FauxtonAPI.router.removeBeforeUnload.apply(FauxtonAPI.router, arguments);
  };

  FauxtonAPI.addRoute = function(route) {
    FauxtonAPI.router.route(route.route, route.name, route.callback);
  };

  FauxtonAPI.triggerRouteEvent = function (routeEvent, args) {
    FauxtonAPI.router.triggerRouteEvent("route:"+routeEvent, args);
  };

  
  return FauxtonAPI;
});

