# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

import mango
import unittest

DOCS = [
    {"_id": "_design/my-design-doc"},
    {"_id": "54af50626de419f5109c962f", "user_id": 0, "age": 10, "name": "Jimi"},
    {"_id": "54af50622071121b25402dc3", "user_id": 1, "age": 11, "name": "Eddie"},
]

# [{erlfdb_nif,erlfdb_future_get,[#Ref<0.1264327726.2786983941.139980>],[]},{erlfdb,fold_range_int,4,[{file,"src/erlfdb.erl"},{line,675}]},{fabric2_fdb,get_winning_revs_wait,2,[{file,"src/fabric2_fdb.erl"},{line,474}]},{fabric2_db,'-open_doc/3-fun-1-',5,[{file,"src/fabric2_db.erl"},{line,503}]},{mango_idx,ddoc_fold_cb,2,[{file,"src/mango_idx.erl"},{line,80}]},{fabric2_db,'-fold_docs/4-fun-0-',6,[{file,"src/fabric2_db.erl"},{line,795}]},{fabric2_fdb,fold_range_cb,2,[{file,"src/fabric2_fdb.erl"},{line,1379}]},{lists,foldl,3,[{file,"lists.erl"},{line,1263}]}]


class IgnoreDesignDocsForAllDocsIndexTests(mango.DbPerClass):
    def test_should_not_return_design_docs(self):
        self.db.save_docs(DOCS)
        docs = self.db.find({"_id": {"$gte": None}})
        assert len(docs) == 2
