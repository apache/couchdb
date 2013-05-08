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

  var Auth = new FauxtonAPI.addon();

  var Admin = Backbone.Model.extend({

    url: function () {
      return app.host + '/_config/admins/' + this.get("name");
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

  Auth.Session = Backbone.Model.extend({
    url: '/_session',

    isAdminParty: function () {
      var userCtx = this.get('userCtx');

      if (!userCtx.name && userCtx.roles.indexOf("_admin") > -1) {
        return true;
      }

      return false;
    },

    user: function () {
      var userCtx = this.get('userCtx');

      if (!userCtx || !userCtx.name) { return null; }

      return {
        name: userCtx.name,
        roles: userCtx.roles
      };
    },

    userRoles: function () {
      var user = this.user();

      if (user && user.roles) { 
        return user.roles;
      }

      return [];
    },

    matchesRoles: function (roles) {
      if (roles.length === 0) {
        return true;
      }

      var numberMatchingRoles = _.intersection(this.userRoles(), roles).length;

      if (numberMatchingRoles > 0) {
        return true;
      }

      return false;
    },

    fetchOnce: function (opt) {
      var options = _.extend({}, opt);

      if (!this._deferred || this._deferred.isRejected() || options.forceFetch ) {
        this._deferred = this.fetch();
      }

      return this._deferred;
    },

    validateUser: function (username, password, msg) {
      if (_.isEmpty(username) || _.isEmpty(password)) {
        var deferred = $.Deferred();

        deferred.rejectWith(this, [msg]);
        return deferred;
      }
    },

    validatePasswords: function (password, password_confirm, msg) {
      if (_.isEmpty(password) || _.isEmpty(password_confirm) || (password !== password_confirm)) {
        var deferred = $.Deferred();

        deferred.rejectWith(this, [msg]);
        return deferred;
      }

    },

    createAdmin: function (username, password, login) {
      var self = this,
          error_promise =  this.validateUser(username, password, 'Authname or password cannot be blank.');

      if (error_promise) { return error_promise; }

      var admin = new Admin({
        name: username,
        value: password
      });

      return admin.save().then(function () {
        if (login) {
          return self.login(username, password);
        } else {
         return self.fetchOnce({forceFetch: true});
        }
      });
    },

    login: function (username, password) {
      var error_promise =  this.validateUser(username, password, 'Authname or password cannot be blank.');

      if (error_promise) { return error_promise; }

      var self = this;

      return $.ajax({
        type: "POST", 
        url: "/_session", 
        dataType: "json",
        data: {name: username, password: password}
      }).then(function () {
         return self.fetchOnce({forceFetch: true});
      });
    },

    logout: function () {
      var self = this;

      return $.ajax({
        type: "DELETE", 
        url: "/_session", 
        dataType: "json",
        username : "_", 
        password : "_"
      }).then(function () {
       return self.fetchOnce({forceFetch: true });
      });
    },

    changePassword: function (password, password_confirm) {
      var error_promise =  this.validatePasswords(password, password_confirm, 'Passwords do not match.');

      if (error_promise) { return error_promise; }

      var  self = this,
           info = this.get('info'),
           userCtx = this.get('userCtx');

       var admin = new Admin({
        name: userCtx.name,
        value: password
      });

      return admin.save().then(function () {
        return self.login(userCtx.name, password);
      });
    }
  });

  Auth.ModalView = FauxtonAPI.View.extend({

    show_modal: function () {
      this.clear_error_msg();
      this.$('.modal').modal();
      // hack to get modal visible 
      $('.modal-backdrop').css('z-index',1025);
    },

    hide_modal: function () {
      this.$('.modal').modal('hide');
    },

    set_error_msg: function (msg) {
      var text;
      if (typeof(msg) == 'string') {
        text = msg;
      } else {
        text = JSON.parse(msg.responseText).reason;
      }

      this.$('#modal-error').text(text).removeClass('hide');
    },

    clear_error_msg: function () {
      this.$('#modal-error').text(' ').addClass('hide');
    }

  });

  Auth.CreateAdminModal = Auth.ModalView.extend({
    template: 'addons/auth/templates/create_admin_modal',

    initialize: function (options) {
      this.login_after = options.login_after || true;
    },

    events: {
      "click #create-admin": "createAdmin"
    },

    createAdmin: function (event) {
      event.preventDefault();
      this.clear_error_msg();

      var self = this,
      username = this.$('#username').val(),
      password = this.$('#password').val();

      var promise = this.model.createAdmin(username, password, this.login_after);

      promise.then(function () {
        self.$('.modal').modal('hide');
        self.hide_modal();
      });

      promise.fail(function (rsp) {
        self.set_error_msg(rsp);
      });
    }

  });

  Auth.LoginModal = Auth.ModalView.extend({
    template: 'addons/auth/templates/login_modal',

    events: {
      "click #login": "login"
    },

    login: function () {
      event.preventDefault();
      this.clear_error_msg();

      var self = this,
      username = this.$('#username').val(),
      password = this.$('#password').val();

      var promise = this.model.login(username, password);

      promise.done(function () {
        self.hide_modal();
      });

      promise.fail(function (rsp) {
        self.set_error_msg(rsp);
      });
    }

  });

  Auth.ChangePasswordModal = Auth.ModalView.extend({
    template: 'addons/auth/templates/change_password_modal',

    events: {
      "click #change-password": "changePassword"
    },

    changePassword: function () {
      event.preventDefault();
      this.clear_error_msg();

      var self = this,
          new_password = this.$('#password').val(),
          password_confirm = this.$('#password-confirm').val();

      var promise = this.model.changePassword(new_password, password_confirm);

      promise.done(function () {
        self.hide_modal();
      });

      promise.fail(function (rsp) {
        self.set_error_msg(rsp);
      });
    }
  });

  Auth.NavLink = FauxtonAPI.View.extend({
    template: 'addons/auth/templates/nav_link',

    tagName: "li",
    className: "dropdown",

    initialize:function (options) {
    },

    serialize: function () {
      return {
        admin_party: this.model.isAdminParty(),
        user: this.model.user()
      };
    },

    events: {
      "click #user-create-admin": 'show_admin_modal',
      "click #user-create-more-admin": 'show_create_more_admin_modal',
      "click #user-login": 'show_login_modal',
      "click #user-change-password": 'show_change_password_modal',
      "click #user-logout": 'logout_user'
    },

    beforeRender: function () {
      this.listenTo(this.model, 'change', this.render);
      this.create_admin_modal = this.setView('#user-create-admin-modal', new Auth.CreateAdminModal({model: this.model}));
      this.login_modal = this.setView('#login-modal', new Auth.LoginModal({model: this.model}));
      this.change_password_modal = this.setView('#change-password-modal', new Auth.ChangePasswordModal({model: this.model}));
    },

    show_admin_modal: function (event) {
      event.preventDefault();
      this.create_admin_modal.show_modal();
    },

    show_create_more_admin_modal: function (event) {
      event.preventDefault();
      this.create_admin_modal.login_after = false;
      this.create_admin_modal.show_modal();
    },

    show_login_modal: function (event) {
      event.preventDefault();
      this.login_modal.show_modal();
    },

    show_change_password_modal: function (event) {
      event.preventDefault();
      this.change_password_modal.show_modal();
    },

    logout_user: function () {
      event.preventDefault();
      this.model.logout();
    }
  });

  Auth.NoAccessView = FauxtonAPI.View.extend({
    template: "addons/auth/templates/noAccess"

  });


  return Auth;
});
