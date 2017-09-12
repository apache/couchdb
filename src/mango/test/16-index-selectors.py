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
import unittest

DOCS = [
    {
        "_id": "100",
        "name": "Jimi",
        "location": "AUS",
        "user_id": 1,
        "same": "value"
    },
    {
        "_id": "200",
        "name": "Eddie",
        "location": "BRA",
        "user_id": 2,
        "same": "value"
    },
    {
        "_id": "300",
        "name": "Harry",
        "location": "CAN",
        "user_id":3,
        "same": "value"
    },
    {
        "_id": "400",
        "name": "Eddie",
        "location": "DEN",
        "user_id":4,
        "same": "value"
    },
    {
        "_id": "500",
        "name": "Jones",
        "location": "ETH",
        "user_id":5,
        "same": "value"
    },
    {
        "_id": "600",
        "name": "Winnifried",
        "location": "FRA",
        "user_id":6,
        "same": "value"
    },
    {
        "_id": "700",
        "name": "Marilyn",
        "location": "GHA",
        "user_id":7,
        "same": "value"
    },
    {
        "_id": "800",
        "name": "Sandra",
        "location": "ZAR",
        "user_id":8,
        "same": "value"
    },
]

class IndexSelectorJson(mango.DbPerClass):
    def setUp(self):
        self.db.recreate()
        self.db.save_docs(copy.deepcopy(DOCS))

    def test_saves_selector_in_index(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_index(["location"], selector=selector)
        indexes = self.db.list_indexes()
        self.assertEqual(indexes[1]["def"]["selector"], selector)

    def test_uses_partial_index_for_query_selector(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_index(["location"], selector=selector, ddoc="Selected", name="Selected")
        resp = self.db.find(selector, explain=True, use_index='Selected')
        self.assertEqual(resp["index"]["name"], "Selected")
        docs = self.db.find(selector)
        self.assertEqual(len(docs), 3)

    def test_uses_partial_index_with_different_selector(self):
        selector = {"location": {"$gte": "FRA"}}
        selector2 = {"location": {"$gte": "A"}}
        self.db.create_index(["location"], selector=selector, ddoc="Selected", name="Selected")
        resp = self.db.find(selector2, explain=True, use_index='Selected')
        self.assertEqual(resp["index"]["name"], "Selected")
        docs = self.db.find(selector2, use_index='Selected')
        self.assertEqual(len(docs), 3)

    def test_doesnot_use_selector_when_not_specified(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_index(["location"], selector=selector, ddoc="Selected", name="Selected")
        resp = self.db.find(selector, explain=True)
        self.assertEqual(resp["index"]["name"], "_all_docs")

    def test_doesnot_use_selector_when_not_specified_with_index(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_index(["location"], selector=selector, ddoc="Selected", name="Selected")
        self.db.create_index(["location"], name="NotSelected")
        resp = self.db.find(selector, explain=True)
        self.assertEqual(resp["index"]["name"], "NotSelected")
