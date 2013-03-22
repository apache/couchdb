define([
       "app",
       "api",
       "addons/config/resources"
],

function (app, FauxtonAPI, Config) {
  var User = new FauxtonAPI.addon();

  User.Session = Backbone.Model.extend({
    url: '/_session',

    is_admin_party: function () {
      var userCtx = this.get('userCtx');
      if (!userCtx.name && userCtx.roles.indexOf("_admin") !== -1) {
        return true;
      }

      return false;
    },

    create_admin: function (username, password) {
      if (_.isEmpty(username) || _.isEmpty(password)) {
        var deferred = $.Deferred();
        
        deferred.reject('Username or password cannot be blank.');
        return deferred;
      }

      var admin = new Config.OptionModel({
        section: "admins",
        name: username,
        value: password
      });
      return admin.save();
    }
  });

  User.CreateAdminModal = FauxtonAPI.View.extend({
    template: 'addons/user/templates/create_admin_modal',

    events: {
      "click #create-admin": "create_admin"
    },

    create_admin: function (event) {
      event.preventDefault();
      var self = this,
          username = this.$('#username').val(),
          password = this.$('#password').val();

      var promise = this.model.create_admin(username, password);

      promise.done(function () {
        self.$('.modal').modal('hide');
        self.trigger('admin_created');
      });

      promise.fail(function (msg) {
        self.$('#modal-error').text(msg).removeClass('hide');
      });
    },

    show_modal: function () {
      this.$('.modal').modal();
    }
  });

  User.Info = FauxtonAPI.View.extend({
    template: 'addons/user/templates/info',

    initialize:function (options) {
      this.model.on('change', this.update_session, this);
    },

    serialize: function () {
      return {
        admin_party: this.model.is_admin_party()
      };
    },

    events: {
      "click #user-create-admin": 'show_admin_modal'
    },

    beforeRender: function () {
      this.create_admin_modal = this.setView('#user-create-admin-modal', new  User.CreateAdminModal({model: this.model}));
      this.create_admin_modal.on('admin_created', this.render);
    },

    afterRender: function () {
      if (this.model.is_admin_party()) {
        console.log('admin party');
        return;
      } else {
        console.log('not admin');
      }
    },

    show_admin_modal: function (event) {
      event.preventDefault();
      this.create_admin_modal.show_modal();
    },

    update_session: function () {
      console.log(this.model);
    }
  });

  return User;
});
