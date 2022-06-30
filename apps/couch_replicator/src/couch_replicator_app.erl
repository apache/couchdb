%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_replicator_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, []) ->
    couch_replicator_sup:start_link().

stop([]) ->
    ok.
