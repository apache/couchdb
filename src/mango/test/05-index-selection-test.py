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

import requests


class IndexSelectionTests:
    def test_basic(self):
        resp = self.db.find({"age": 123}, explain=True)
        self.assertEqual(resp["index"]["type"], "json")

    def test_with_and(self):
        resp = self.db.find(
            {
                "name.first": "Stephanie",
                "name.last": "This doesn't have to match anything.",
            },
            explain=True,
        )
        self.assertEqual(resp["index"]["type"], "json")

    def test_with_nested_and(self):
        resp = self.db.find(
            {"name.first": {"$gt": "a", "$lt": "z"}, "name.last": "Foo"}, explain=True
        )
        self.assertEqual(resp["index"]["type"], "json")

    def test_with_or(self):
        ddocid = "_design/company_and_manager"

        resp = self.db.find(
            {
                "company": {"$gt": "a", "$lt": "z"},
                "$or": [{"manager": "Foo"}, {"manager": "Bar"}],
            },
            explain=True,
        )
        self.assertEqual(resp["index"]["ddoc"], ddocid)

    def test_use_most_columns(self):
        ddocid = "_design/age"
        resp = self.db.find(
            {
                "name.first": "Stephanie",
                "name.last": "Something or other",
                "age": {"$gt": 1},
            },
            explain=True,
        )
        self.assertNotEqual(resp["index"]["ddoc"], ddocid)

        resp = self.db.find(
            {
                "name.first": "Stephanie",
                "name.last": "Something or other",
                "age": {"$gt": 1},
            },
            use_index=ddocid,
            explain=True,
        )
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
        ddocid = "_design/age"
        r = self.db.find({}, use_index=ddocid, return_raw=True)
        self.assertEqual(
            r["warning"].split("\n")[0].lower(),
            "{0} was not used because it does not contain a valid index for this query.".format(
                ddocid
            ),
        )

    def test_uses_index_when_no_range_or_equals(self):
        # index on ["manager"] should be valid because
        # selector requires "manager" to exist. The
        # selector doesn't narrow the keyrange so it's
        # a full index scan
        selector = {"manager": {"$exists": True}}
        docs = self.db.find(selector)
        self.assertEqual(len(docs), 14)

        resp_explain = self.db.find(selector, explain=True)
        self.assertEqual(resp_explain["index"]["type"], "json")

    def test_reject_use_index_invalid_fields(self):
        ddocid = "_design/company_and_manager"
        selector = {"company": "Pharmex"}
        r = self.db.find(selector, use_index=ddocid, return_raw=True)
        self.assertEqual(
            r["warning"].split("\n")[0].lower(),
            "{0} was not used because it does not contain a valid index for this query.".format(
                ddocid
            ),
        )

        # should still return a correct result
        for d in r["docs"]:
            self.assertEqual(d["company"], "Pharmex")

    def test_reject_use_index_ddoc_and_name_invalid_fields(self):
        ddocid = "_design/company_and_manager"
        name = "company_and_manager"
        selector = {"company": "Pharmex"}

        resp = self.db.find(selector, use_index=[ddocid, name], return_raw=True)
        self.assertEqual(
            resp["warning"].split("\n")[0].lower(),
            "{0}, {1} was not used because it is not a valid index for this query.".format(
                ddocid, name
            ),
        )

        # should still return a correct result
        for d in resp["docs"]:
            self.assertEqual(d["company"], "Pharmex")

    def test_reject_use_index_sort_order(self):
        # index on ["company","manager"] which should not be valid
        # and there is no valid fallback (i.e. an index on ["company"])
        ddocid = "_design/company_and_manager"
        selector = {"company": {"$gt": None}}
        try:
            self.db.find(selector, use_index=ddocid, sort=[{"company": "desc"}])
        except Exception as e:
            self.assertEqual(e.response.status_code, 400)
        else:
            raise AssertionError("did not reject bad use_index")

    def test_use_index_fallback_if_valid_sort(self):
        ddocid_valid = "_design/fallbackfoo"
        ddocid_invalid = "_design/fallbackfoobar"
        self.db.create_index(fields=["foo"], ddoc=ddocid_invalid)
        self.db.create_index(fields=["foo", "bar"], ddoc=ddocid_valid)
        selector = {"foo": {"$gt": None}}

        resp_explain = self.db.find(
            selector, sort=["foo", "bar"], use_index=ddocid_invalid, explain=True
        )
        self.assertEqual(resp_explain["index"]["ddoc"], ddocid_valid)

        resp = self.db.find(
            selector, sort=["foo", "bar"], use_index=ddocid_invalid, return_raw=True
        )
        self.assertEqual(
            resp["warning"].split("\n")[0].lower(),
            "{0} was not used because it does not contain a valid index for this query.".format(
                ddocid_invalid
            ),
        )
        self.assertEqual(len(resp["docs"]), 0)

    def test_prefer_use_index_over_optimal_index(self):
        # index on ["company"] even though index on ["company", "manager"] is better
        ddocid_preferred = "_design/testsuboptimal"
        self.db.create_index(fields=["baz"], ddoc=ddocid_preferred)
        self.db.create_index(fields=["baz", "bar"])
        selector = {"baz": {"$gt": None}, "bar": {"$gt": None}}
        resp = self.db.find(selector, use_index=ddocid_preferred, return_raw=True)
        self.assertTrue("warning" not in resp)

        resp_explain = self.db.find(selector, use_index=ddocid_preferred, explain=True)
        self.assertEqual(resp_explain["index"]["ddoc"], ddocid_preferred)

    # This doc will not be saved given the new ddoc validation code
    # in couch_mrview
    def test_manual_bad_view_idx01(self):
        design_doc = {
            "_id": "_design/bad_view_index",
            "language": "query",
            "views": {
                "queryidx1": {
                    "map": {"fields": {"age": "asc"}},
                    "reduce": "_count",
                    "options": {"def": {"fields": [{"age": "asc"}]}, "w": 2},
                }
            },
            "views": {
                "views001": {
                    "map": "function(employee){if(employee.training)"
                    + "{emit(employee.number, employee.training);}}"
                }
            },
        }
        try:
            self.db.save_doc(design_doc)
            assert False, "Should not get here."
        except requests.exceptions.HTTPError as e:
            self.assertEqual(e.response.json()["error"], "invalid_design_doc")

    def test_explain_sort_reverse(self):
        selector = {"manager": {"$gt": None}}
        resp_explain = self.db.find(
            selector, fields=["manager"], sort=[{"manager": "desc"}], explain=True
        )
        self.assertEqual(resp_explain["index"]["type"], "json")


class JSONIndexSelectionTests(mango.UserDocsTests, IndexSelectionTests):
    @classmethod
    def setUpClass(klass):
        super(JSONIndexSelectionTests, klass).setUpClass()

    def test_uses_all_docs_when_fields_do_not_match_selector(self):
        # index exists on ["company", "manager"] but not ["company"]
        # so we should fall back to all docs (so we include docs
        # with no "manager" field)
        selector = {"company": "Pharmex"}
        docs = self.db.find(selector)
        self.assertEqual(len(docs), 1)
        self.assertEqual(docs[0]["company"], "Pharmex")
        self.assertNotIn("manager", docs[0])

        resp_explain = self.db.find(selector, explain=True)

        self.assertEqual(resp_explain["index"]["type"], "special")

    def test_uses_all_docs_when_selector_doesnt_require_fields_to_exist(self):
        # as in test above, use a selector that doesn't overlap with the index
        # due to an explicit exists clause
        selector = {"company": "Pharmex", "manager": {"$exists": False}}
        docs = self.db.find(selector)
        self.assertEqual(len(docs), 1)
        self.assertEqual(docs[0]["company"], "Pharmex")
        self.assertNotIn("manager", docs[0])

        resp_explain = self.db.find(selector, explain=True)
        self.assertEqual(resp_explain["index"]["type"], "special")


@unittest.skipUnless(mango.has_text_service(), "requires text service")
class TextIndexSelectionTests(mango.UserDocsTests):
    @classmethod
    def setUpClass(klass):
        super(TextIndexSelectionTests, klass).setUpClass()
        if mango.has_text_service():
            user_docs.add_text_indexes(klass.db, {})

    def test_with_text(self):
        resp = self.db.find(
            {
                "$text": "Stephanie",
                "name.first": "Stephanie",
                "name.last": "This doesn't have to match anything.",
            },
            explain=True,
        )
        self.assertEqual(resp["index"]["type"], "text")

    def test_no_view_index(self):
        resp = self.db.find({"name.first": "Ohai!"}, explain=True)
        self.assertEqual(resp["index"]["type"], "text")

    def test_with_or(self):
        resp = self.db.find(
            {
                "$or": [
                    {"name.first": "Stephanie"},
                    {"name.last": "This doesn't have to match anything."},
                ]
            },
            explain=True,
        )
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
                        "fields": {"$default": "standard"},
                    },
                }
            },
            "indexes": {
                "st_index": {
                    "analyzer": "standard",
                    "index": 'function(doc){\n index("st_index", doc.geometry);\n}',
                }
            },
        }
        self.db.save_doc(design_doc)
        docs = self.db.find({"age": 48})
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
        resp = self.db.find(
            {"name.first": "A first name", "name.last": "A last name"}, explain=True
        )
        self.assertEqual(resp["index"]["type"], "json")

    def test_multi_text_index_is_error(self):
        try:
            self.db.find({"$text": "a query"}, explain=True)
        except Exception as e:
            self.assertEqual(e.response.status_code, 400)

    def test_use_index_works(self):
        resp = self.db.find({"$text": "a query"}, use_index="foo", explain=True)
        self.assertEqual(resp["index"]["ddoc"], "_design/foo")
