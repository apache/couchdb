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

import copy
import mango

DOCS1 = [
    {
        "_id": "54af50626de419f5109c962f",
        "user_id": 0,
        "age": 10,
        "name": "Jimi",
        "location": "UK",
        "number": 4
    },
    {
        "_id": "54af50622071121b25402dc3",
        "user_id": 1,
        "age": 12,
        "name": "Eddie",
        "location": "ZAR",
        "number": 2
    },
]

class SupportStableAndUpdate(mango.DbPerClass):
    def setUp(self):
        self.db.recreate()
        self.db.create_index(["name"])
        self.db.save_docs(copy.deepcopy(DOCS1))

    def test_update_updates_view_when_specified(self):
        docs = self.db.find({"name": "Eddie"}, update=False)
        assert len(docs) == 0
        docs = self.db.find({"name": "Eddie"}, update=True)
        assert len(docs) == 1
