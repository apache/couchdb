define([
  "app",

  // Libs
  "backbone",

  // Modules
  "modules/fauxton"

],

function (app, backbone, Fauxton) {

  var Config = app.module();

  Config.Model = Backbone.Model.extend({});
  Config.OptionModel = Backbone.Model.extend({

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

  Config.ViewItem = Backbone.View.extend({
    tagName: "tr",
    className: "config-item",
    template: "config/item",

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

  Config.View = Backbone.View.extend({
    template: "config/dashboard",

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
    }
  });

  return Config;
});
