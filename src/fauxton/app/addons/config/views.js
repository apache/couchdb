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
  "addons/config/resources",
  "addons/fauxton/components"
],
function(app, FauxtonAPI, Config, Components) {
  var Views ={},
      Events = {};

  Views.Events = _.extend(Events, Backbone.Events);

  Views.TableRow = FauxtonAPI.View.extend({
    tagName: "tr",
    className: "config-item",
    template: "addons/config/templates/item",

    events: {
      "dblclick .js-edit-value": "editValue",
      "click .js-delete-value": "deleteValue",
      "click .js-cancel-value": "cancelEdit",
      "click .js-save-value": "saveAndRender",
      "keyup .js-value-input": "processKeyEvents"
    },

    deleteValue: function (event) {
      var result = confirm("Are you sure you want to delete this configuration value?");

      if (!result) { return; }

      this.model.destroy();
      this.remove();
    },

    editValue: function (event) {
      this.$(".js-show-value").addClass("js-hidden");
      this.$(".js-edit-value-form").removeClass("js-hidden");
      this.$(".js-value-input").focus();
    },

    processKeyEvents: function (event) {
      // Enter key
      if (event.keyCode === 13) {
        return this.saveAndRender();
      }
      // Esc key
      if (event.keyCode === 27) {
        return this.discardValue();
      }
    },

    discardValue: function (event) {
      this.$(".js-edit-value-form").addClass("js-hidden");
      this.$(".js-show-value").removeClass("js-hidden");
    },

    cancelEdit: function (event) {
      this.discardValue();
    },

    serialize: function () {
      return {option: this.model.toJSON()};
    },

    saveAndRender: function () {
      this.model.save({value: this.$(".js-value-input").val()});
      this.render();
    }

  });

  Views.Table = FauxtonAPI.View.extend({
    template: "addons/config/templates/dashboard",

    events: {
      "click #js-add-section": "addSection"
    },

    initialize: function(){
      this.listenTo(Views.Events, "newSection", this.render);
    },

    addSection: function (event) {
      event.preventDefault();
      this.modal.show();
    },

    beforeRender: function() {
      this.modal = this.insertView("#add-section-modal", new Views.Modal({
                      collection: this.collection
                    }));

      this.modal.render();

      this.collection.each(function(config) {
        _.each(config.get("options"), function (option, index) {
          this.insertView("table.config tbody", new Views.TableRow({
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

  Views.Modal = FauxtonAPI.View.extend({
    className: "modal hide fade",
    template:  "addons/config/templates/modal",
    events: {
      "submit #js-add-section-form": "validate"
    },
    initialize: function(){
      this.sourceArray = _.map(this.collection.toJSON(), function(item, key){ 
        return item.section; 
      });
    },
    afterRender: function(){
      this.sectionTypeAhead = new Components.Typeahead({
        source: this.sourceArray,
        el: 'input[name="section"]'
      });
      this.sectionTypeAhead.render();
    },
    submitForm: function (event) {
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

      this.hide();
      Views.Events.trigger("newSection");

    },
    isNew: function(collection){
      var sectionName = this.$('input[name="section"]').val(),
          name = this.$('input[name="name"]').val();
          var section = _.findWhere(collection.toJSON(), {"section":sectionName});
          var options = _.findWhere(section.options, {name: name});
          
          return options;
    },
    isSection: function(){
      var section = this.$('input[name="section"]').val();
      return _.find(this.sourceArray, function(item){ return item === section; });
    },
    validate: function (event){
      event.preventDefault();
      var section = this.$('input[name="section"]').val(),
          name = this.$('input[name="name"]').val(),
          value = this.$('input[name="value"]').val(),
          collection = this.collection;

      if(!this.isSection()){
         this.errorMessage("You need to use an existing section");
      } else if (!name) {
        this.errorMessage("Add a name");
      } else if (!value) {
        this.errorMessage("Add a value");
      } else if (this.isNew(collection)){
        this.errorMessage("Must have a unique name");
      } else {
        this.submitForm();
      }
    },
    errorMessage: function(msg){
      this.error = FauxtonAPI.addNotification({
          msg: msg,
          type: "error",
          clear: true,
          selector: ".js-form-error-config"
      });
    },
    show: function(){
      this.$el.modal({show:true});
    },
    hide: function(){
      this.$el.modal('hide');
    }

  });

  return Views;

});
