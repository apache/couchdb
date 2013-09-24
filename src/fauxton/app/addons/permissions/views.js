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
       "addons/permissions/resources"
],
function (app, FauxtonAPI, Permissions ) {
  var events = {};
  Permissions.events = _.extend(events, Backbone.Events);

  Permissions.Permissions = FauxtonAPI.View.extend({
    template: "addons/permissions/templates/permissions",

    initialize: function (options) {
      this.database = options.database;
      this.listenTo(Permissions.events, 'itemRemoved', this.itemRemoved);
    },

    itemRemoved: function (event) {
      this.model.set({
        admins: this.adminsView.items(),
        members: this.membersView.items()
      });

      this.model.save().then(function () {
        FauxtonAPI.addNotification({
          msg: 'Database permissions has been updated.'
        });
        }, function (xhr) {
        FauxtonAPI.addNotification({
          msg: 'Could not update permissions - reason: ' + xhr.responseText,
          type: 'error'
        });
      });
    },

    beforeRender: function () {
      this.adminsView = this.insertView('#sections', new Permissions.PermissionSection({
        model: this.model,
        section: 'admins',
        help: 'Database admins can update design documents and edit the admin and member lists.'
      }));

      this.membersView = this.insertView('#sections', new Permissions.PermissionSection({
        model: this.model,
        section: 'members',
        help: 'Database members can access the database. If no members are defined, the database is public.'
      }));
    },

    serialize: function () {
      return {
        databaseName: this.database.id,
      };
    }
  });

  Permissions.PermissionSection = FauxtonAPI.View.extend({
    template: "addons/permissions/templates/section",
    initialize: function (options) {
      this.section = options.section;
      this.help = options.help;
    },

    events: {
      "submit .permission-item-form": "addItem",
      'click .close': "removeItem"
    },

    beforeRender: function () {
      var section = this.model.get(this.section);
      
      this.nameViews = [];
      this.roleViews = [];

      _.each(section.names, function (name) {
        var nameView = this.insertView('#items-names', new Permissions.PermissionItem({
          item: name,
        }));
        this.nameViews.push(nameView);
      }, this);

      _.each(section.roles, function (role) {
        var roleView = this.insertView('#items-roles', new Permissions.PermissionItem({
          item: role,
        }));
        this.roleViews.push(roleView);
      }, this);
    },

    getItemFromView: function (viewList) {
      return _.map(viewList, function (view) {
        return view.item;
      });
    },

    discardRemovedViews: function () {
      this.nameViews = _.filter(this.nameViews, function (view) {
        return !view.removed; 
      });

      this.roleViews = _.filter(this.roleViews, function (view) {
        return !view.removed; 
      });
    },

    items: function () {
      this.discardRemovedViews();

      return  {
        names: this.getItemFromView(this.nameViews),
        roles: this.getItemFromView(this.roleViews)
      };
    },

    addItem: function (event) {
      event.preventDefault();
      var $item = this.$(event.currentTarget).find('.item'),
          value = $item.val(),
          section = $item.data('section'),
          type = $item.data('type'),
          that = this;

      var resp = this.model.addItem(value, type, section);

      if (resp && resp.error) {
        return FauxtonAPI.addNotification({
          msg: resp.msg,
          type: 'error'
        });
      }

      this.model.save().then(function () {
        that.render();
        FauxtonAPI.addNotification({
          msg: 'Database permissions has been updated.'
        });
      }, function (xhr) {
        FauxtonAPI.addNotification({
          msg: 'Could not update permissions - reason: ' + xhr.responseText,
          type: 'error'
        });
      });
    },

    serialize: function () {
      return {
        section: this.section,
        help: this.help
      };
    }

  });

  Permissions.PermissionItem = FauxtonAPI.View.extend({
    template: "addons/permissions/templates/item",
    initialize: function (options) {
      this.item = options.item;
      this.viewsList = options.viewsList;
    },

    events: {
      'click .close': "removeItem"
    },

    removeItem: function (event) {
      var that = this;
      event.preventDefault();
      
      this.removed = true;
      Permissions.events.trigger('itemRemoved');

      this.$el.hide('fast', function () {
        that.remove();
      });
    },


    serialize: function () {
      return {
        item: this.item
      };
    }

  });

  return Permissions;
});
