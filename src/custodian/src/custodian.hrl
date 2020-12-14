% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-define(CUSTODIAN_ID, <<"_design/custodian">>).

-define(CUSTODIAN_VALIDATION,
<<"function(newDoc, oldDoc) {
  var i, range, node;
  if(newDoc['_id'].substring(0, 8) === \"_design/\") return;
  if(newDoc['_deleted'] === true) return;
  if (!newDoc.by_node) {
    throw({forbidden: \"by_node is mandatory\"});
  }
  if (!newDoc.by_range) {
    throw({forbidden: \"by_range is mandatory\"});
  }
  for (node in newDoc.by_node) {
    for (i in newDoc.by_node[node]) {
      range = newDoc.by_node[node][i];
      if(!newDoc.by_range[range]) {
        throw({forbidden: \"by_range for \" + range + \" is missing\"});
      }
      if(newDoc.by_range[range].indexOf(node) === -1) {
        throw({forbidden : \"by_range for \" + range + \" is missing \" + node});
      }
    }
  }
  for (range in newDoc.by_range) {
    for (i in newDoc.by_range[range]) {
      node = newDoc.by_range[range][i];
      if(!newDoc.by_node[node]) {
        throw({forbidden: \"by_node for \" + node + \" is missing\"});
      }
      if (newDoc.by_node[node].indexOf(range) === -1) {
        throw({forbidden: \"by_node for \" + node + \" is missing \" + range});
      }
    }
  }
}
">>).
