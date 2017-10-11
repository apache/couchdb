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
import user_docs
import unittest


class IndexSelectionTests:

    def test_basic(self):
        resp = self.db.find({"age": 123}, explain=True)
        self.assertEqual(resp["index"]["type"], "json")

    def test_with_and(self):
        resp = self.db.find({
                "name.first": "Stephanie",
                "name.last": "This doesn't have to match anything."
            }, explain=True)
        self.assertEqual(resp["index"]["type"], "json")

    def test_use_most_columns(self):
        # ddoc id for the age index
        ddocid = "_design/ad3d537c03cd7c6a43cf8dff66ef70ea54c2b40f"
        resp = self.db.find({
                "name.first": "Stephanie",
                "name.last": "Something or other",
                "age": {"$gt": 1}
            }, explain=True)
        self.assertNotEqual(resp["index"]["ddoc"], "_design/" + ddocid)

        resp = self.db.find({
                "name.first": "Stephanie",
                "name.last": "Something or other",
                "age": {"$gt": 1}
            }, use_index=ddocid, explain=True)
        self.assertEqual(resp["index"]["ddoc"], ddocid)

    def test_no_valid_sort_index(self):
        try:
            self.db.find({"_id": {"$gt": None}}, sort=["name"], return_raw=True)
        except Exception as e:
            self.assertEqual(e.response.status_code, 400)
        else:
            raise AssertionError("bad find")

    def test_invalid_use_index(self):
        # ddoc id for the age index
        ddocid = "_design/ad3d537c03cd7c6a43cf8dff66ef70ea54c2b40f"
        try:
            self.db.find({}, use_index=ddocid)
        except Exception as e:
            self.assertEqual(e.response.status_code, 400)
        else:
            raise AssertionError("bad find")

    def test_uses_index_when_no_range_or_equals(self):
        # index on ["manager"] should be valid because
        # selector requires "manager" to exist. The
        # selector doesn't narrow the keyrange so it's
        # a full index scan
        selector = {
            "manager": {"$exists": True}
        }
        docs = self.db.find(selector)
        self.assertEqual(len(docs), 14)

        resp_explain = self.db.find(selector, explain=True)
        self.assertEqual(resp_explain["index"]["type"], "json")


    def test_reject_use_index_invalid_fields(self):
        # index on ["company","manager"] which should not be valid
        ddocid = "_design/a0c425a60cf3c3c09e3c537c9ef20059dcef9198"
        selector = {
            "company": "Pharmex"
        }
        try:
            self.db.find(selector, use_index=ddocid)
        except Exception as e:
            self.assertEqual(e.response.status_code, 400)
        else:
            raise AssertionError("did not reject bad use_index")

    def test_reject_use_index_sort_order(self):
        # index on ["company","manager"] which should not be valid
        ddocid = "_design/a0c425a60cf3c3c09e3c537c9ef20059dcef9198"
        selector = {
            "company": {"$gt": None},
            "manager": {"$gt": None}
        }
        try:
            self.db.find(selector, use_index=ddocid, sort=[{"manager":"desc"}])
        except Exception as e:
            self.assertEqual(e.response.status_code, 400)
        else:
            raise AssertionError("did not reject bad use_index")

    # This doc will not be saved given the new ddoc validation code
    # in couch_mrview
    def test_manual_bad_view_idx01(self):
        design_doc = {
            "_id": "_design/bad_view_index",
            "language": "query",
            "views": {
                "queryidx1": {
                    "map": {
                        "fields": {
                            "age": "asc"
                        }
                    },
                    "reduce": "_count",
                    "options": {
                        "def": {
                            "fields": [
                                {
                                    "age": "asc"
                                }
                            ]
                        },
                        "w": 2
                    }
                }
            },
            "views" : {
                "views001" : {
                "map" : "function(employee){if(employee.training)"
                    + "{emit(employee.number, employee.training);}}"
                }
            }
        }
        with self.assertRaises(KeyError):
            self.db.save_doc(design_doc)


class JSONIndexSelectionTests(mango.UserDocsTests, IndexSelectionTests):

    @classmethod
    def setUpClass(klass):
        super(JSONIndexSelectionTests, klass).setUpClass()

    def test_uses_all_docs_when_fields_do_not_match_selector(self):
        # index exists on ["company", "manager"] but not ["company"]
        # so we should fall back to all docs (so we include docs
        # with no "manager" field)
        selector = {
            "company": "Pharmex"
        }
        docs = self.db.find(selector)
        self.assertEqual(len(docs), 1)
        self.assertEqual(docs[0]["company"], "Pharmex")
        self.assertNotIn("manager", docs[0])
        
        resp_explain = self.db.find(selector, explain=True)

        self.assertEqual(resp_explain["index"]["type"], "special")

    def test_uses_all_docs_when_selector_doesnt_require_fields_to_exist(self):
        # as in test above, use a selector that doesn't overlap with the index
        # due to an explicit exists clause
        selector = {
            "company": "Pharmex",
            "manager": {"$exists": False}
        }
        docs = self.db.find(selector)
        self.assertEqual(len(docs), 1)
        self.assertEqual(docs[0]["company"], "Pharmex")
        self.assertNotIn("manager", docs[0])

        resp_explain = self.db.find(selector, explain=True)
        self.assertEqual(resp_explain["index"]["type"], "special")


@unittest.skipUnless(mango.has_text_service(), "requires text service")
class TextIndexSelectionTests(mango.UserDocsTests, IndexSelectionTests):

    @classmethod
    def setUpClass(klass):
        super(TextIndexSelectionTests, klass).setUpClass()

    def setUp(self):
        self.db.recreate()
        user_docs.add_text_indexes(self.db, {})

    def test_with_text(self):
        resp = self.db.find({
                "$text" : "Stephanie",
                "name.first": "Stephanie",
                "name.last": "This doesn't have to match anything."
            }, explain=True)
        self.assertEqual(resp["index"]["type"], "text")

    def test_no_view_index(self):
        resp = self.db.find({"name.first": "Ohai!"}, explain=True)
        self.assertEqual(resp["index"]["type"], "text")

    def test_with_or(self):
        resp = self.db.find({
                "$or": [
                    {"name.first": "Stephanie"},
                    {"name.last": "This doesn't have to match anything."}
                ]
            }, explain=True)
        self.assertEqual(resp["index"]["type"], "text")
    
    def test_manual_bad_text_idx(self):
        design_doc = {
            "_id": "_design/bad_text_index",
            "language": "query",
            "indexes": {
                    "text_index": {
                        "default_analyzer": "keyword",
                        "default_field": {},
                        "selector": {},
                        "fields": "all_fields",
                        "analyzer": {
                        "name": "perfield",
                        "default": "keyword",
                        "fields": {
                            "$default": "standard"
                        }
                    }
                }
            },
            "indexes": {
                "st_index": {
                    "analyzer": "standard",
                    "index": "function(doc){\n index(\"st_index\", doc.geometry);\n}"
                }
            }
        }
        self.db.save_doc(design_doc)
        docs= self.db.find({"age" : 48})
        self.assertEqual(len(docs), 1)
        self.assertEqual(docs[0]["name"]["first"], "Stephanie")
        self.assertEqual(docs[0]["age"], 48)


@unittest.skipUnless(mango.has_text_service(), "requires text service")
class MultiTextIndexSelectionTests(mango.UserDocsTests):
    @classmethod
    def setUpClass(klass):
        super(MultiTextIndexSelectionTests, klass).setUpClass()
        if mango.has_text_service():
            klass.db.create_text_index(ddoc="foo", analyzer="keyword")
            klass.db.create_text_index(ddoc="bar", analyzer="email")

    def test_fallback_to_json_with_multi_text(self):
        resp = self.db.find({"name.first": "A first name", "name.last": "A last name"}, explain=True)
        self.assertEqual(resp["index"]["type"], "json")

    def test_multi_text_index_is_error(self):
        try:
            self.db.find({"$text": "a query"}, explain=True)
        except Exception as e:
            self.assertEqual(e.response.status_code, 400)

    def test_use_index_works(self):
        resp = self.db.find({"$text": "a query"}, use_index="foo", explain=True)
        self.assertEqual(resp["index"]["ddoc"], "_design/foo")
