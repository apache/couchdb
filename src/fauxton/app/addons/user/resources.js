define([
       "app",
       "api"
],

function (app, FauxtonAPI) {
  var User = new FauxtonAPI.addon();

  User.Session = Backbone.Model.extend({
    url: '/_session',
  });
  
  User.Info = FauxtonAPI.View.extend({

    initialize:function (options) {
      this.model.on('change', this.update_session, this);
    },

    update_session: function () {
      console.log('update session');
      console.log(this.model);
    }
  });

  var session = new User.Session();

  User.Layout = Backbone.Layout.extend({
    views: {
      'a[href="#user"]': new User.Info({model: session})
    }
  });

  var layout = new User.Layout();

  layout.render();
  session.fetch();
  return User;
});
