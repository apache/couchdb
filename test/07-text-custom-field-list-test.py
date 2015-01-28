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


@unittest.skipUnless(mango.has_text_service(), "requires text service")
class CustomFieldsTest(mango.UserDocsTextTests):

    FIELDS = [
        {"name": "favorites.[]", "type": "string"},
        {"name": "manager", "type": "boolean"},
        {"name": "age", "type": "number"},
        # These two are to test the default analyzer for
        # each field.
        {"name": "location.state", "type": "string"},
        {
            "name": "location.address.street",
            "type": "string"
        },
        {"name": "name\\.first", "type": "string"}
    ]

    def test_basic(self):
        docs = self.db.find({"age": 22})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 9

    def test_multi_field(self):
        docs = self.db.find({"age": 22, "manager": True})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 9

        docs = self.db.find({"age": 22, "manager": False})
        assert len(docs) == 0

    def test_element_acess(self):
        docs = self.db.find({"favorites.0": "Ruby"})
        assert len(docs) == 3
        for d in docs:
            assert "Ruby" in d["favorites"]

    # This should throw an exception because we only index the array
    # favorites.[], and not the string field favorites
    def test_index_selection(self):
        try:
            self.db.find({"selector": {"$or": [{"favorites": "Ruby"},
                {"favorites.0":"Ruby"}]}})
        except Exception, e:
                assert e.response.status_code == 400

    def test_in_with_array(self):
        vals = ["Lisp", "Python"]
        docs = self.db.find({"favorites": {"$in": vals}})
        assert len(docs) == 10

    # This should also throw an error because we only indexed
    # favorites.[] of type string. For the following query to work, the
    # user has to index favorites.[] of type number, and also
    # favorites.[].Versions.Alpha of type string.
    def test_in_different_types(self):
        vals = ["Random Garbage", 52, {"Versions": {"Alpha": "Beta"}}]
        try:
            self.db.find({"favorites": {"$in": vals}})
        except Exception, e:
                assert e.response.status_code == 400

    # This test differs from the situation where we index everything.
    # When we index everything the actual number of docs that gets
    # returned is 5. That's because of the special situation where we
    # have an array of an array, i.e: [["Lisp"]], because we're indexing
    # specifically favorites.[] of type string. So it does not count
    # the example and we only get 4 back.
    def test_nin_with_array(self):
        vals = ["Lisp", "Python"]
        docs = self.db.find({"favorites": {"$nin": vals}})
        assert len(docs) == 4

    def test_missing(self):
        self.db.find({"location.state": "Nevada"})

    def test_missing_type(self):
        # Raises an exception
        try:
            self.db.find({"age": "foo"})
            raise Exception("Should have thrown an HTTPError")
        except:
            return

    def test_field_analyzer_is_keyword(self):
        docs = self.db.find({"location.state": "New"})
        assert len(docs) == 0

        docs = self.db.find({"location.state": "New Hampshire"})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 10

    # Since our FIELDS list only includes "name\\.first", we should
    # get an error when we try to search for "name.first", since the index
    # for that field does not exist.
    def test_escaped_field(self):
        docs = self.db.find({"name\\.first": "name dot first"})
        assert len(docs) == 1
        assert docs[0]["name.first"] == "name dot first"

        try:
            self.db.find({"name.first": "name dot first"})
            raise Exception("Should have thrown an HTTPError")
        except:
            return

    def test_filtered_search_fields(self):
        docs = self.db.find({"age": 22}, fields = ["age", "location.state"])
        assert len(docs) == 1
        assert docs == [{"age": 22, "location": {"state": "Missouri"}}]

        docs = self.db.find({"age": 22}, fields = ["age", "Random Garbage"])
        assert len(docs) == 1
        assert docs == [{"age": 22}]

        docs = self.db.find({"age": 22}, fields = ["favorites"])
        assert len(docs) == 1
        assert docs == [{"favorites": ["Lisp", "Erlang", "Python"]}]

        docs = self.db.find({"age": 22}, fields = ["favorites.[]"])
        assert len(docs) == 1
        assert docs == [{}]

        docs = self.db.find({"age": 22}, fields = ["all_fields"])
        assert len(docs) == 1
        assert docs == [{}]
