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

       "addons/documents/resources",
       "addons/databases/resources"
],

function(app, FauxtonAPI, Components, Documents, Databases) {
  var Views = {};

  Views.Sidebar = FauxtonAPI.View.extend({
    template: "addons/documents/templates/sidebar",
    className: "sidenav",
    tagName: "nav",
    events: {
      "click button#delete-database": "showDeleteDatabaseModal"
    },

    initialize: function(options) {
      this.database = options.database;
      if (options.ddocInfo) {
        this.ddocID = options.ddocInfo.id;
        this.currView = options.ddocInfo.currView;
      }
    },
    showDeleteDatabaseModal: function(event){
      this.deleteDBModal.showModal();
    },

    serialize: function() {
      var docLinks = FauxtonAPI.getExtensions('docLinks'),
          newLinks = FauxtonAPI.getExtensions('sidebar:newLinks'),
          addLinks = FauxtonAPI.getExtensions('sidebar:links'),
          extensionList = FauxtonAPI.getExtensions('sidebar:list');
      return {
        changes_url: '#' + this.database.url('changes'),
        permissions_url: '#' + this.database.url('app') + '/permissions',
        db_url: '#' + this.database.url('index'),
        database: this.collection.database,
        database_url: '#' + this.database.url('app'),
        docLinks: docLinks,
        addLinks: addLinks,
        newLinks: newLinks,
        extensionList: extensionList > 0
      };
    },


    beforeRender: function(manage) {
      this.deleteDBModal = this.setView(
        '#delete-db-modal',
        new Views.DeleteDBModal({database: this.database})
      );

      var database = this.collection.database,
          newurl = "#" + database.url('app') + '/new';

      var newLinks = [{
        title: 'Add new',
        links: [{
          title: 'New Doc',
          url: newurl,
          icon: 'fonticon-circle-plus'
        },{
          title: 'New Design Doc',
          url: newurl,
          icon: 'fonticon-circle-plus'
        }]
      }];

      this.insertView("#new-all-docs-button", new Components.MenuDropDown({
        links: newLinks,
      }));

      this.insertView("#new-design-docs-button", new Components.MenuDropDown({
        links: newLinks,
      }));

      this.collection.each(function(design) {
        if (design.has('doc')){
          this.insertView(new Views.DdocSidenav({
            model: design,
            collection: this.collection
          }));
        }
      },this);
    },

    afterRender: function () {
      if (this.selectedTab) {
        this.setSelectedTab(this.selectedTab);
      }
    },

    setSelectedTab: function (selectedTab) {
      this.selectedTab = selectedTab;
      var $selectedTab = this.$('#' + selectedTab);

      this.$('li').removeClass('active');
      $selectedTab.parent().addClass('active');

      if ($selectedTab.parents(".accordion-body").length !== 0){
        $selectedTab
        .parents(".accordion-body")
        .addClass("in")
        .parents(".nav-header")
        .find(".js-collapse-toggle").addClass("down");
      }
    }
  });

  Views.DdocSidenav = FauxtonAPI.View.extend({
    tagName: "ul",
    className:  "nav nav-list",
    template: "addons/documents/templates/design_doc_menu",
    events: {
      "click button": "no",
      "click .js-collapse-toggle": "toggleArrow"
    },
    initialize: function(){

    },
    toggleArrow:  function(e){
      this.$(e.currentTarget).toggleClass("down");
    },
    no: function(event){
      event.preventDefault();
      alert("no");
    },
    buildIndexList: function(collection, selector, ddocType){
      var design = this.model.id.replace(/^_design\//,"");

      _.each(_.keys(collection[selector]), function(key){
        this.insertView(".accordion-body", new Views.IndexItem({
          selector: selector,
          ddoc: design,
          index: key,
          ddocType: ddocType,
          database: this.model.collection.database.id
        }));
      }, this);
    },

    serialize: function(){
      var ddocName = this.model.id.replace(/^_design\//,"");
      return{
        database: this.collection.database,
        designDoc: ddocName,
        ddoc_clean: app.utils.removeSpecialCharacters(ddocName),
        ddoc_encoded: app.utils.safeURLName(ddocName),
        database_encoded: app.utils.safeURLName(this.model.collection.database.id),
      };
    },
    beforeRender: function(manage) {
      var ddocDocs = this.model.get("doc"),
          ddocName = this.model.id.replace(/^_design\//,""),
          sideBarMenuLinks = [];

      var sidebarListTypes = FauxtonAPI.getExtensions('sidebar:list');
          if (ddocDocs){
            //Views
            this.buildIndexList(ddocDocs, "views", "view");
            //lists
            // this.buildIndexList(ddocDocs, "lists");
            // //show
            // this.buildIndexList(ddocDocs, "show");
            // //filters
            // this.buildIndexList(ddocDocs, "filters");
            //extensions
            _.each(sidebarListTypes, function (type) {
              this.buildIndexList(ddocDocs, type);
            },this);
          }
       
     var docSafe = app.utils.safeURLName(ddocName), 
         database = this.collection.database,
         links = _.reduce(FauxtonAPI.getExtensions('sidebar:links'), function (menuLinks, link) {

          menuLinks.push({
            title: link.title,
            url: "#" + database.url('app') + "/" + link.url + "/" + docSafe,
            icon: 'fonticon-circle-plus'
          });

         return menuLinks; 
     }, [{
      title: 'Secondary View',
      url: "#" + database.url('app') + "/new_view/" + docSafe,
      icon: 'fonticon-circle-plus'
     }]);

     sideBarMenuLinks.push({
       title: 'Add new',
       links: links
     });

      this.insertView(".new-button", new Components.MenuDropDown({
        links: sideBarMenuLinks,
      }));
    }
  });

  Views.IndexItem = FauxtonAPI.View.extend({
    template: "addons/documents/templates/index_menu_item",
    tagName: "li",

    initialize: function(options){
      this.index = options.index;
      this.ddoc = options.ddoc;
      this.database = options.database;
      this.selected = !! options.selected;
      this.selector = options.selector;
      this.ddocType = options.ddocType || this.selector;
    },

    serialize: function() {
      return {
        icon: this.ddocType,
        type:  this.ddocType,
        index: this.index,
        ddoc: this.ddoc,
        database: this.database,
        selected: this.selected
      };
    },

    afterRender: function() {
      if (this.selected) {
        $(".sidenav ul.nav-list li").removeClass("active");
        this.$el.addClass("active");
      }
    }
  });

  return Views;
});

