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
        assert indexes[1]["def"]["selector"] == selector

    def test_uses_selector_for_index(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_index(["location"], selector=selector, name="Selected")
        resp = self.db.find({"location":{"$gte": "FRA"}}, explain=True)
        assert resp["index"]["name"] == "Selected"

    def test_uses_selector_for_index_multiple_fields(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_index(["location"], selector=selector, name="Selected")
        resp = self.db.find({"location":{"$gte": "FRA"}, "user_id": 7}, explain=True)
        assert resp["index"]["name"] == "Selected"

    def test_uses_selector_for_index_multiple_fields_on_both(self):
        selector = {"location": {"$gte": "FRA"}, "user_id": 7}
        self.db.create_index(["location"], selector=selector, name="Selected")
        resp = self.db.find({"location":{"$gte": "FRA"}, "user_id": 7}, explain=True)
        assert resp["index"]["name"] == "Selected"

    def test_with_or_selector(self):
        selector = {"location": {"$gte": "FRA"}, "user_id": 7}
        self.db.create_index(["location"], selector=selector, name="Selected")
        self.db.create_index(["location"], name="NotSelected")
        resp = self.db.find({"$or":[{"location":{"$gte": "FRA"}, "user_id": 7}, {"name": "Sandra"}]}, explain=True)
        assert resp["index"]["name"] != "Selected"

    def test_does_not_use_selector_index(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_index(["location"], selector=selector, name="Selected")
        resp = self.db.find({"location":{"$eq": "ZAR"}}, explain=True)
        assert resp["index"]["name"] != "Selected"

    def test_two_indexes_use_index_with_selector(self):
        selector = {"location": {"$gte": "ZAR"}}
        self.db.create_index(["location"], selector=selector, name="Selected")
        self.db.create_index(["location"], name="NotSelected")
        resp = self.db.find({"location":{"$gte": "ZAR"}}, explain=True)
        print resp
        assert resp["index"]["name"] == "Selected"
