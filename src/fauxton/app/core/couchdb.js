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
  "core/base"
],
function (app, FauxtonAPI) {
  //this later needs to be dynamically loaded 

  FauxtonAPI.UUID = FauxtonAPI.Model.extend({
    initialize: function(options) {
      options = _.extend({count: 1}, options);
      this.count = options.count;
    },

    url: function() {
      return app.host + "/_uuids?count=" + this.count;
    },

    next: function() {
      return this.get("uuids").pop();
    }
  });

  FauxtonAPI.Session = FauxtonAPI.Model.extend({
    url: app.host + '/_session',

    user: function () {
      var userCtx = this.get('userCtx');

      if (!userCtx || !userCtx.name) { return null; }

      return {
        name: userCtx.name,
        roles: userCtx.roles
      };
    },

    fetchUser: function (opt) {
      var that = this,
      currentUser = this.user();

      return this.fetchOnce(opt).then(function () {
        var user = that.user();

        // Notify anyone listening on these events that either a user has changed
        // or current user is the same
        if (currentUser !== user) {
          that.trigger('session:userChanged');
        } else {
          that.trigger('session:userFetched');
        }

        // this will return the user as a value to all function that calls done on this
        // eg. session.fetchUser().done(user) { .. do something with user ..}
        return user; 
      });
    }
  });

  FauxtonAPI.setSession = function (newSession) {
    app.session = FauxtonAPI.session = newSession;
    return FauxtonAPI.session.fetchUser();
  };

  //set default session
  FauxtonAPI.setSession(new FauxtonAPI.Session());

  return FauxtonAPI;
});
