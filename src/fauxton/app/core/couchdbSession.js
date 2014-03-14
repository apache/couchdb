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
  "core/base"
],
function (FauxtonAPI) {
  var CouchdbSession = {
    Session: FauxtonAPI.Model.extend({
      url: '/_session',

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
            options = opt || {},
            currentUser = this.user(),
            fetch = _.bind(this.fetchOnce, this);

        if (options.forceFetch) {
          fetch = _.bind(this.fetch, this);
        }

        return fetch(opt).then(function () {
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
    })
  };

  return CouchdbSession;
});
