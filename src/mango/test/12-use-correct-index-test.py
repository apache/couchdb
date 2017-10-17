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
import copy

DOCS = [
    {
        "_id": "_design/my-design-doc",
    },
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
    {
        "_id": "54af50622071121b25402dc6",
        "user_id": 1,
        "age": 6,
        "name": "Harry",
        "location": "US",
        "number":8
    },
    {
        "_id": "54af50622071121b25402dc9",
        "name": "Eddie",
        "occupation": "engineer",
        "number":7
    },
]

class ChooseCorrectIndexForDocs(mango.DbPerClass):
    def setUp(self):
        self.db.recreate()
        self.db.save_docs(copy.deepcopy(DOCS))

    def test_choose_index_with_one_field_in_index(self):
        self.db.create_index(["name", "age", "user_id"], ddoc="aaa")
        self.db.create_index(["name"], ddoc="zzz")
        explain = self.db.find({"name": "Eddie"}, explain=True)
        self.assertEqual(explain["index"]["ddoc"], '_design/zzz')

    def test_choose_index_with_two(self):
        self.db.create_index(["name", "age", "user_id"], ddoc="aaa")
        self.db.create_index(["name", "age"], ddoc="bbb")
        self.db.create_index(["name"], ddoc="zzz")
        explain = self.db.find({"name": "Eddie", "age":{"$gte": 12}}, explain=True)
        self.assertEqual(explain["index"]["ddoc"], '_design/bbb')

    def test_choose_index_alphabetically(self):
        self.db.create_index(["name"], ddoc="aaa")
        self.db.create_index(["name"], ddoc="bbb")
        self.db.create_index(["name"], ddoc="zzz")
        explain = self.db.find({"name": "Eddie", "age": {"$gte": 12}}, explain=True)
        self.assertEqual(explain["index"]["ddoc"], '_design/aaa')

    def test_choose_index_most_accurate(self):
        self.db.create_index(["name", "age", "user_id"], ddoc="aaa")
        self.db.create_index(["name", "age"], ddoc="bbb")
        self.db.create_index(["name"], ddoc="zzz")
        explain = self.db.find({"name": "Eddie", "age": {"$gte": 12}}, explain=True)
        self.assertEqual(explain["index"]["ddoc"], '_design/bbb')
    
    def test_choose_index_most_accurate_in_memory_selector(self):
        self.db.create_index(["name", "location", "user_id"], ddoc="aaa")
        self.db.create_index(["name", "age", "user_id"], ddoc="bbb")
        self.db.create_index(["name"], ddoc="zzz")
        explain = self.db.find({"name": "Eddie", "number": {"$lte": 12}}, explain=True)
        self.assertEqual(explain["index"]["ddoc"], '_design/zzz')

    def test_warn_on_full_db_scan(self):
        selector = {"not_indexed":"foo"}
        explain_resp = self.db.find(selector, explain=True, return_raw=True)
        self.assertEqual(explain_resp["index"]["type"], "special")
        resp = self.db.find(selector, return_raw=True)
        self.assertEqual(resp["warning"], "no matching index found, create an index to optimize query time")

    def test_chooses_idxA(self):
        DOCS2 = [
            {"a":1, "b":1, "c":1},
            {"a":1000, "d" : 1000, "e": 1000}
        ]
        self.db.save_docs(copy.deepcopy(DOCS2))
        self.db.create_index(["a", "b", "c"])
        self.db.create_index(["a", "d", "e"])
        explain = self.db.find({"a": {"$gt": 0}, "b": {"$gt": 0}, "c": {"$gt": 0}}, explain=True)
        self.assertEqual(explain["index"]["def"]["fields"], [{'a': 'asc'}, {'b': 'asc'}, {'c': 'asc'}])

    def test_can_query_with_range_on_secondary_column(self):
        self.db.create_index(["age", "name"], ddoc="bbb")
        selector = {"age": 10, "name": {"$gte": 0}}
        docs = self.db.find(selector)
        self.assertEqual(len(docs), 1)
        explain = self.db.find(selector, explain=True)
        self.assertEqual(explain["index"]["ddoc"], "_design/bbb")
        self.assertEqual(explain["mrargs"]["end_key"], [10, '<MAX>'])
