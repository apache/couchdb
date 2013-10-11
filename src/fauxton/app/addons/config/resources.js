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
      return app.host + '/_config/' + this.get("section") + '/' + this.get("name");
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

  Config.ViewItem = FauxtonAPI.View.extend({
    tagName: "tr",
    className: "config-item",
    template: "addons/config/templates/item",

    events: {
      "click .edit-button": "editValue",
      "click #delete-value": "deleteValue",
      "click #cancel-value": "cancelEdit",
      "click #save-value": "saveValue"
    },

    deleteValue: function (event) {
      var result = confirm("Are you sure you want to delete this configuration value?");

      if (!result) { return; }

      this.model.destroy();
      this.remove();
    },

    editValue: function (event) {
      this.$("#show-value").hide();
      this.$("#edit-value-form").show();
    },

    saveValue: function (event) {
      this.model.save({value: this.$(".value-input").val()});
      this.render();
    },

    cancelEdit: function (event) {
      this.$("#edit-value-form").hide();
      this.$("#show-value").show();
    },

    serialize: function () {
      return {option: this.model.toJSON()};
    }

  });

  Config.View = FauxtonAPI.View.extend({
    template: "addons/config/templates/dashboard",

    events: {
      "click #add-section": "addSection",
      "submit #add-section-form": "submitForm"
    },

    submitForm: function (event) {
      event.preventDefault();
      var option = new Config.OptionModel({
        section: this.$('input[name="section"]').val(),
        name: this.$('input[name="name"]').val(),
        value: this.$('input[name="value"]').val()
      });

      option.save();

      var section = this.collection.find(function (section) {
        return section.get("section") === option.get("section");
      });

      if (section) {
        section.get("options").push(option.attributes);
      } else {
        this.collection.add({
          section: option.get("section"),
          options: [option.attributes]
        });
      }

      this.$("#add-section-modal").modal('hide');
      this.render();
    },

    addSection: function (event) {
      event.preventDefault();
      this.$("#add-section-modal").modal({show:true});
    },

    beforeRender: function() {
      this.collection.each(function(config) {
        _.each(config.get("options"), function (option, index) {
          this.insertView("table.config tbody", new Config.ViewItem({
            model: new Config.OptionModel({
              section: config.get("section"),
              name: option.name,
              value: option.value,
              index: index
            })
          }));
        }, this);
      }, this);
    },

    establish: function() {
      return [this.collection.fetch()];
    }
  });

  return Config;
});
