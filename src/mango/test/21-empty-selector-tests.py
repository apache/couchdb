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

import json
import mango
import unittest
import user_docs
import math


def make_empty_selector_suite(klass):
    class EmptySelectorTestCase(klass):
        def test_empty(self):
            resp = self.db.find({}, explain=True)
            self.assertEqual(resp["index"]["type"], "special")

        def test_empty_array_or(self):
            resp = self.db.find({"$or": []}, explain=True)
            self.assertEqual(resp["index"]["type"], klass.INDEX_TYPE)
            docs = self.db.find({"$or": []})
            assert len(docs) == 0

        def test_empty_array_or_with_age(self):
            resp = self.db.find({"age": 22, "$or": []}, explain=True)
            self.assertEqual(resp["index"]["type"], klass.INDEX_TYPE)
            docs = self.db.find({"age": 22, "$or": []})
            assert len(docs) == 1

        def test_empty_array_in_with_age(self):
            resp = self.db.find({"age": 22, "company": {"$in": []}}, explain=True)
            self.assertEqual(resp["index"]["type"], klass.INDEX_TYPE)
            docs = self.db.find({"age": 22, "company": {"$in": []}})
            assert len(docs) == 0

        def test_empty_array_and_with_age(self):
            resp = self.db.find({"age": 22, "$and": []}, explain=True)
            self.assertEqual(resp["index"]["type"], klass.INDEX_TYPE)
            docs = self.db.find({"age": 22, "$and": []})
            assert len(docs) == 1

        def test_empty_array_all_age(self):
            resp = self.db.find({"age": 22, "company": {"$all": []}}, explain=True)
            self.assertEqual(resp["index"]["type"], klass.INDEX_TYPE)
            docs = self.db.find({"age": 22, "company": {"$all": []}})
            assert len(docs) == 0

        def test_empty_array_nested_all_with_age(self):
            resp = self.db.find(
                {"age": 22, "$and": [{"company": {"$all": []}}]}, explain=True
            )
            self.assertEqual(resp["index"]["type"], klass.INDEX_TYPE)
            docs = self.db.find({"age": 22, "$and": [{"company": {"$all": []}}]})
            assert len(docs) == 0

        def test_empty_arrays_complex(self):
            resp = self.db.find({"$or": [], "a": {"$in": []}}, explain=True)
            self.assertEqual(resp["index"]["type"], klass.INDEX_TYPE)
            docs = self.db.find({"$or": [], "a": {"$in": []}})
            assert len(docs) == 0

        def test_empty_nin(self):
            resp = self.db.find({"favorites": {"$nin": []}}, explain=True)
            self.assertEqual(resp["index"]["type"], klass.INDEX_TYPE)
            docs = self.db.find({"favorites": {"$nin": []}})
            assert len(docs) == len(user_docs.DOCS)

    return EmptySelectorTestCase


class EmptySelectorNoIndexTests(
    make_empty_selector_suite(mango.UserDocsTestsNoIndexes)
):
    pass


@unittest.skipUnless(mango.has_text_service(), "requires text service")
class EmptySelectorTextTests(make_empty_selector_suite(mango.UserDocsTextTests)):
    pass


class EmptySelectorUserDocTests(make_empty_selector_suite(mango.UserDocsTests)):
    pass
