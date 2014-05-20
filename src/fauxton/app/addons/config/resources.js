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
  "api"

],

function (app, FauxtonAPI) {

  var Config = FauxtonAPI.addon();


  Config.Model = Backbone.Model.extend({});
  Config.OptionModel = Backbone.Model.extend({
    documentation: "config",

    url: function () {
      return app.host + '/_config/' + this.get("section") + '/' + encodeURIComponent(this.get("name"));
    },

    isNew: function () { return false; },

    sync: function (method, model, options) {

      var params = {
        url: model.url(),
        contentType: 'application/json',
        dataType: 'json',
        data: JSON.stringify(model.get('value'))
      };

      if (method === 'delete') {
        params.type = 'DELETE';
      } else {
        params.type = 'PUT';
      }

      return $.ajax(params);
    }
  });

  Config.Collection = Backbone.Collection.extend({
    model: Config.Model,
    documentation: "config",
    comparator: function (OptionModel) {
      if (OptionModel.get("section")) {
        return OptionModel.get("section");
      }
    },
    url: function () {
      return app.host + '/_config';
    },

    parse: function (resp) {
      return _.map(resp, function (section, section_name) {
        return {
          section: section_name,
          options: _.map(section, function (option, option_name) {
            return {
              name: option_name,
              value: option
            };
          })
        };
      });
    }
  });



  return Config;
});
