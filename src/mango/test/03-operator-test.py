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


class OperatorTests:
    def assertUserIds(self, user_ids, docs):
        user_ids_returned = list(d["user_id"] for d in docs)
        user_ids.sort()
        user_ids_returned.sort()
        self.assertEqual(user_ids, user_ids_returned)

    def test_all(self):
        docs = self.db.find(
            {"manager": True, "favorites": {"$all": ["Lisp", "Python"]}}
        )
        self.assertEqual(len(docs), 3)
        user_ids = [2, 12, 9]
        self.assertUserIds(user_ids, docs)

    def test_all_non_array(self):
        docs = self.db.find({"manager": True, "location": {"$all": ["Ohai"]}})
        self.assertEqual(len(docs), 0)

    def test_elem_match(self):
        emdocs = [
            {"user_id": "a", "bang": [{"foo": 1, "bar": 2}]},
            {"user_id": "b", "bang": [{"foo": 2, "bam": True}]},
        ]
        self.db.save_docs(emdocs, w=3)
        docs = self.db.find(
            {
                "_id": {"$gt": None},
                "bang": {"$elemMatch": {"foo": {"$gte": 1}, "bam": True}},
            }
        )
        self.assertEqual(len(docs), 1)
        self.assertEqual(docs[0]["user_id"], "b")

    def test_all_match(self):
        amdocs = [
            {"user_id": "a", "bang": [{"foo": 1, "bar": 2}, {"foo": 3, "bar": 4}]},
            {"user_id": "b", "bang": [{"foo": 1, "bar": 2}, {"foo": 4, "bar": 4}]},
        ]
        self.db.save_docs(amdocs, w=3)
        docs = self.db.find(
            {"bang": {"$allMatch": {"foo": {"$mod": [2, 1]}, "bar": {"$mod": [2, 0]}}}}
        )
        self.assertEqual(len(docs), 1)
        self.assertEqual(docs[0]["user_id"], "a")

    def test_empty_all_match(self):
        amdocs = [{"bad_doc": "a", "emptybang": []}]
        self.db.save_docs(amdocs, w=3)
        docs = self.db.find({"emptybang": {"$allMatch": {"foo": {"$eq": 2}}}})
        self.assertEqual(len(docs), 0)

    def test_in_operator_array(self):
        docs = self.db.find({"manager": True, "favorites": {"$in": ["Ruby", "Python"]}})
        self.assertUserIds([2, 6, 7, 9, 11, 12], docs)

    def test_nin_operator_array(self):
        docs = self.db.find(
            {"manager": True, "favorites": {"$nin": ["Erlang", "Python"]}}
        )
        self.assertEqual(len(docs), 4)
        for doc in docs:
            if isinstance(doc["favorites"], list):
                self.assertNotIn("Erlang", doc["favorites"])
                self.assertNotIn("Python", doc["favorites"])

    def test_regex(self):
        docs = self.db.find(
            {"age": {"$gt": 40}, "location.state": {"$regex": "(?i)new.*"}}
        )
        self.assertEqual(len(docs), 2)
        self.assertUserIds([2, 10], docs)

    def test_exists_false(self):
        docs = self.db.find({"age": {"$gt": 0}, "twitter": {"$exists": False}})
        user_ids = [2, 3, 5, 6, 7, 8, 10, 11, 12, 14]
        self.assertUserIds(user_ids, docs)
        for d in docs:
            self.assertNotIn("twitter", d)

    def test_eq_null_does_not_include_missing(self):
        docs = self.db.find({"age": {"$gt": 0}, "twitter": None})
        user_ids = [9]
        self.assertUserIds(user_ids, docs)
        for d in docs:
            self.assertEqual(d["twitter"], None)

    def test_ne_includes_null_but_not_missing(self):
        docs = self.db.find({"twitter": {"$ne": "notamatch"}})
        user_ids = [0, 1, 4, 9, 13]
        self.assertUserIds(user_ids, docs)
        for d in docs:
            self.assertIn("twitter", d)

    # ideally this work be consistent across index types but, alas, it is not
    @unittest.skipUnless(
        not mango.has_text_service(),
        "text indexes do not support range queries across type boundaries",
    )
    def test_lt_includes_null_but_not_missing(self):
        docs = self.db.find({"twitter": {"$lt": 1}})
        user_ids = [9]
        self.assertUserIds(user_ids, docs)
        for d in docs:
            self.assertEqual(d["twitter"], None)

    @unittest.skipUnless(
        not mango.has_text_service(),
        "text indexes do not support range queries across type boundaries",
    )
    def test_lte_includes_null_but_not_missing(self):
        docs = self.db.find({"twitter": {"$lt": 1}})
        user_ids = [9]
        self.assertUserIds(user_ids, docs)
        for d in docs:
            self.assertEqual(d["twitter"], None)

    def test_lte_null_includes_null_but_not_missing(self):
        docs = self.db.find({"twitter": {"$lte": None}})
        user_ids = [9]
        self.assertUserIds(user_ids, docs)
        for d in docs:
            self.assertEqual(d["twitter"], None)

    def test_lte_at_z_except_null_excludes_null_and_missing(self):
        docs = self.db.find({"twitter": {"$and": [{"$lte": "@z"}, {"$ne": None}]}})
        user_ids = [0, 1, 4, 13]
        self.assertUserIds(user_ids, docs)
        for d in docs:
            self.assertNotEqual(d["twitter"], None)

    def test_range_gte_null_includes_null_but_not_missing(self):
        docs = self.db.find({"twitter": {"$gte": None}})
        self.assertGreater(len(docs), 0)
        for d in docs:
            self.assertIn("twitter", d)

    def test_exists_false_returns_missing_but_not_null(self):
        docs = self.db.find({"twitter": {"$exists": False}})
        self.assertGreater(len(docs), 0)
        for d in docs:
            self.assertNotIn("twitter", d)

    @unittest.skipUnless(
        not mango.has_text_service(),
        "text indexes do not support range queries across type boundaries",
    )
    def test_lte_respsects_unicode_collation(self):
        docs = self.db.find({"ordered": {"$lte": "a"}})
        user_ids = [7, 8, 9, 10, 11, 12]
        self.assertUserIds(user_ids, docs)

    @unittest.skipUnless(
        not mango.has_text_service(),
        "text indexes do not support range queries across type boundaries",
    )
    def test_gte_respsects_unicode_collation(self):
        docs = self.db.find({"ordered": {"$gte": "a"}})
        user_ids = [12, 13, 14]
        self.assertUserIds(user_ids, docs)


class OperatorJSONTests(mango.UserDocsTests, OperatorTests):
    pass


@unittest.skipUnless(mango.has_text_service(), "requires text service")
class OperatorTextTests(mango.UserDocsTextTests, OperatorTests):
    pass


class OperatorAllDocsTests(mango.UserDocsTestsNoIndexes, OperatorTests):
    def test_range_id_eq(self):
        doc_id = "8e1c90c0-ac18-4832-8081-40d14325bde0"
        r = self.db.find({"_id": doc_id}, explain=True, return_raw=True)

        self.assertEqual(r["args"]["end_key"], doc_id)
        self.assertEqual(r["args"]["start_key"], doc_id)
