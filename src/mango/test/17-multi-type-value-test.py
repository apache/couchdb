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
    {"_id": "1", "name": "Jimi", "age": 10},
    {"_id": "2", "name": {"forename": "Eddie"}, "age": 20},
    {"_id": "3", "name": None, "age": 30},
    {"_id": "4", "name": 1, "age": 40},
    {"_id": "5", "forename": "Sam", "age": 50},
]


class MultiValueFieldTests:
    def test_can_query_with_name(self):
        docs = self.db.find({"name": {"$exists": True}})
        self.assertEqual(len(docs), 4)
        for d in docs:
            self.assertIn("name", d)

    def test_can_query_with_name_subfield(self):
        docs = self.db.find({"name.forename": {"$exists": True}})
        self.assertEqual(len(docs), 1)
        self.assertEqual(docs[0]["_id"], "2")

    def test_can_query_with_name_range(self):
        docs = self.db.find({"name": {"$gte": 0}})
        # expect to include "Jimi", 1 and {"forename":"Eddie"}
        self.assertEqual(len(docs), 3)
        for d in docs:
            self.assertIn("name", d)

    def test_can_query_with_age_and_name_range(self):
        docs = self.db.find({"age": {"$gte": 0, "$lt": 40}, "name": {"$gte": 0}})
        # expect to include "Jimi", 1 and {"forename":"Eddie"}
        self.assertEqual(len(docs), 2)
        for d in docs:
            self.assertIn("name", d)


class MultiValueFieldJSONTests(mango.DbPerClass, MultiValueFieldTests):
    def setUp(self):
        self.db.recreate()
        self.db.create_index(["name"], wait_for_built_index=False)
        self.db.create_index(["age", "name"], wait_for_built_index=True)
        self.db.save_docs(copy.deepcopy(DOCS))


# @unittest.skipUnless(mango.has_text_service(), "requires text service")
# class MultiValueFieldTextTests(MultiValueFieldDocsNoIndexes, OperatorTests):
#     pass


class MultiValueFieldAllDocsTests(mango.DbPerClass, MultiValueFieldTests):
    def setUp(self):
        self.db.recreate()
        self.db.save_docs(copy.deepcopy(DOCS))
