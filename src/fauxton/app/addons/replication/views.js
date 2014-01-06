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
       "addons/fauxton/components",
       "addons/replication/resources"
],
function(app, FauxtonAPI, Components, replication) {
  var View = {},
  Events ={},
  pollingInfo ={
    rate: 5,
    intervalId: null
  };

  _.extend(Events, Backbone.Events);

  // NOTES: http://wiki.apache.org/couchdb/Replication

  // Replication form view is huge
  // -----------------------------------
  // afterRender: autocomplete on the target input field
  // beforeRender:  add the status table
  // disableFields:  disable non active fields on submit 
  // enableFields:  enable field when radio btns are clicked
  // establish:  get the DB list for autocomplete
  // formValidation:  make sure fields aren't empty
  // showProgress:  make a call to active_tasks model and show only replication types.  Poll every 5 seconds. (make this it's own view)
  // startReplication:  saves to the model, starts replication
  // submit:  form submit handler
  // swapFields:  change to and from target
  // toggleAdvancedOptions:  toggle advanced

  View.ReplicationForm = FauxtonAPI.View.extend({
    template: "addons/replication/templates/form",
    events:  {
      "submit #replication": "validate",
      "click .btn-group .btn": "showFields",
      "click .swap": "swapFields",
      "click .options": "toggleAdvancedOptions"
    },
    initialize: function(options){
      this.status = options.status;
      this.selectedDB = options.selectedDB;
      this.newRepModel = new replication.Replicate({});
    },
    afterRender: function(){
      this.dbSearchTypeahead = new Components.DbSearchTypeahead({
        dbLimit: 30,
        el: "input#to_name"
      });

      this.dbSearchTypeahead.render();

    },

    beforeRender:  function(){
      this.insertView("#replicationStatus", new View.ReplicationList({
        collection: this.status
      }));
    },
    cleanup: function(){
      clearInterval(pollingInfo.intervalId);
    },
    enableFields: function(){
      this.$el.find('input','select').attr('disabled',false);
    },
    disableFields: function(){
      this.$el.find('input:hidden','select:hidden').attr('disabled',true);
    },
    showFields: function(e){
      var $currentTarget = this.$(e.currentTarget),
      targetVal = $currentTarget.val();

      if (targetVal === "local"){
        $currentTarget.parents('.form_set').addClass('local');
      }else{
        $currentTarget.parents('.form_set').removeClass('local');
      }
    },
    establish: function(){
      return [ this.collection.fetch(), this.status.fetch()];
    },
    validate: function(e){
      e.preventDefault();
      var notification;
      if (this.formValidation()){
        notification = FauxtonAPI.addNotification({
          msg: "Please enter every field.",
          type: "error",
          clear: true
        });
      }else if (this.$('input#to_name').is(':visible') && !this.$('input[name=create_target]').is(':checked')){
        var alreadyExists = this.collection.where({"name":this.$('input#to_name').val()});
        if (alreadyExists.length === 0){
          notification = FauxtonAPI.addNotification({
            msg: "This database doesn't exist. Check create target if you want to create it.",
            type: "error",
            clear: true
          });
        }
      }else{
        this.submit(e);
      }
    },
    formValidation: function(e){
      var $remote = this.$el.find('input:visible'),
      error = false;
      for(var i=0; i<$remote.length; i++){
        if ($remote[i].value =="http://" || $remote[i].value ===""){
          error = true;
        }
      }
      return error;
    },
    serialize: function(){
      return {
        databases:  this.collection.toJSON(),
        selectedDB: this.selectedDB
      };
    },
    startReplication: function(json){
      var that = this;
      this.newRepModel.save(json,{
        success: function(resp){
          var notification = FauxtonAPI.addNotification({
            msg: "Replication from "+resp.get('source')+" to "+ resp.get('target')+" has begun.",
            type: "success",
            clear: true
          });
          that.updateButtonText(false);
          Events.trigger('update:tasks');
        },
        error: function(model, xhr, options){
          var errorMessage = JSON.parse(xhr.responseText);
          var notification = FauxtonAPI.addNotification({
            msg: errorMessage.reason,
            type: "error",
            clear: true
          });
          that.updateButtonText(false);
        }
      });
      this.enableFields();
    },		
    updateButtonText: function(wait){
      var $button = this.$('#replication button[type=submit]');
      if(wait){
        $button.text('Starting replication...').attr('disabled', true);
      } else {
        $button.text('Replication').attr('disabled', false);
      }
    },
    submit: function(e){
      this.disableFields(); 
      var formJSON = {};
      _.map(this.$(e.currentTarget).serializeArray(), function(formData){
        if(formData.value !== ''){
          formJSON[formData.name] = (formData.value ==="true"? true: formData.value.replace(/\s/g, '').toLowerCase());
        }
      });

      this.updateButtonText(true);
      this.startReplication(formJSON);
    },	
    swapFields: function(e){
      e.preventDefault();
      //WALL O' VARIABLES
      var $fromSelect = this.$('#from_name'),
          $toSelect = this.$('#to_name'),
          $toInput = this.$('#to_url'),
          $fromInput = this.$('#from_url'),
          fromSelectVal = $fromSelect.val(),
          fromInputVal = $fromInput.val(),
          toSelectVal = $toSelect.val(),
          toInputVal = $toInput.val();

      $fromSelect.val(toSelectVal);
      $toSelect.val(fromSelectVal);

      $fromInput.val(toInputVal);
      $toInput.val(fromInputVal);
    }
  });


  View.ReplicationList = FauxtonAPI.View.extend({
    tagName: "ul",
    initialize:  function(){
      Events.bind('update:tasks', this.establish, this);
      this.listenTo(this.collection, "reset", this.render);
      this.$el.prepend("<li class='header'><h4>Active Replication Tasks</h4></li>");
    },
    establish: function(){
      return [this.collection.fetch({reset: true})];
    },
    setPolling: function(){
      var that = this;
      this.cleanup();
      pollingInfo.intervalId = setInterval(function() {
        that.establish();
      }, pollingInfo.rate*1000);
    },
    cleanup: function(){
      clearInterval(pollingInfo.intervalId);
    },
    beforeRender:  function(){
      this.collection.forEach(function(item) {
        this.insertView(new View.replicationItem({ 
          model: item
        }));
      }, this);
    },
    showHeader: function(){
      if (this.collection.length > 0){
        this.$el.parent().addClass('showHeader');
      } else {
        this.$el.parent().removeClass('showHeader');
      }
    },
    afterRender: function(){
      this.showHeader();
      this.setPolling();
    }
  });

  //make this a table row item.
  View.replicationItem = FauxtonAPI.View.extend({
    tagName: "li",
    className: "row",
    template: "addons/replication/templates/progress",
    events: {
      "click .cancel": "cancelReplication"
    },
    initialize: function(){
      this.newRepModel = new replication.Replicate({});
    },
    establish: function(){
      return [this.model.fetch()];
    },
    cancelReplication: function(e){
      //need to pass "cancel": true with source & target
      var $currentTarget = this.$(e.currentTarget),
      repID = $currentTarget.attr('data-rep-id');
      this.newRepModel.save({
        "replication_id": repID,
        "cancel": true
      },
      {
        success: function(model, xhr, options){
          var notification = FauxtonAPI.addNotification({
            msg: "Replication stopped.",
            type: "success",
            clear: true
          });
        },
        error: function(model, xhr, options){
          var errorMessage = JSON.parse(xhr.responseText);
          var notification = FauxtonAPI.addNotification({
            msg: errorMessage.reason,
            type: "error",
            clear: true
          });
        }
      });
    },
    afterRender: function(){
      if (this.model.get('continuous')){
        this.$el.addClass('continuous');
      }
    },
    serialize: function(){
      return {
        progress:  this.model.get('progress'),
        target: this.model.get('target'),
        source: this.model.get('source'),
        continuous: this.model.get('continuous'),
        repid: this.model.get('replication_id')
      };
    }
  });

  return View;
});
