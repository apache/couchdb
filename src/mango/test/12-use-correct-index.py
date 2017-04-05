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
    {
        "_id": "_design/my-design-doc",
    },
    {
        "_id": "54af50626de419f5109c962f",
        "user_id": 0,
        "age": 10,
        "name": "Jimi",
        "location": "UK"
    },
    {
        "_id": "54af50622071121b25402dc3",
        "user_id": 1,
        "age": 12,
        "name": "Eddie",
        "location": "ZAR"
    },
    {
        "_id": "54af50622071121b25402dc6",
        "user_id": 1,
        "age": 6,
        "name": "Harry",
        "location": "US"
    },
    {
        "_id": "54af50622071121b25402dc9",
        "name": "Eddie"
    },
]

class ChooseCorrectIndexForDocs(mango.DbPerClass):
    def test_choose_index_with_one_field_in_index(self):
        self.db.save_docs(DOCS)
        self.db.create_index(["name", "age", "user_id"], ddoc="aaa")
        self.db.create_index(["name"], ddoc="zzz")
        explain = self.db.find({"name": "Eddie"}, explain=True)
        assert explain["index"]["ddoc"] == '_design/zzz'

    def test_choose_index_with_two(self):
        self.db.save_docs(DOCS)
        self.db.create_index(["name", "age", "user_id"], ddoc="aaa")
        self.db.create_index(["name", "age"], ddoc="bbb")
        self.db.create_index(["name"], ddoc="zzz")
        explain = self.db.find({"name": "Eddie", "age":{"$gte": 12}}, explain=True)
        assert explain["index"]["ddoc"] == '_design/bbb'

    def test_choose_index_alphabetically(self):
        self.db.save_docs(DOCS)
        self.db.create_index(["name", "age", "user_id"], ddoc="aaa")
        self.db.create_index(["name", "age", "location"], ddoc="bbb")
        self.db.create_index(["name"], ddoc="zzz")
        explain = self.db.find({"name": "Eddie", "age": {"$gt": 12}}, explain=True)
        assert explain["index"]["ddoc"] == '_design/aaa'
