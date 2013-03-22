define([
       "app",
       "api",
       "addons/user/resources"
],

function(app, FauxtonAPI, User) {

  var userRoutes = function () {

    var session = new User.Session();
    return {
      layout: 'one_pane',
      crumbs: [{"name": "User","link": "_user"}],
      views: {
        "#dashboard-content": new User.Info({model: session})
      },
      establish: function () {
        return [session.fetch()];
      },
      apiUrl: 'boom'
    };
  };

  User.Routes = {
    "_user": userRoutes
  };

  return User;
});
