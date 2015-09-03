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


class CustomFieldsTest(mango.UserDocsTextTests):

    @classmethod
    def setUpClass(klass):
        raise unittest.SkipTest('text index service not available')

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
