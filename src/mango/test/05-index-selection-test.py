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


class IndexSelectionTests(mango.UserDocsTests):
    @classmethod
    def setUpClass(klass):
        super(IndexSelectionTests, klass).setUpClass()
        if mango.has_text_service():
            user_docs.add_text_indexes(klass.db, {})

    def test_basic(self):
        resp = self.db.find({"manager": "A manager"}, explain=True)
        assert resp["index"]["type"] == "json"

    def test_with_and(self):
        resp = self.db.find({
                "name.first": "Stephanie",
                "name.last": "This doesn't have to match anything."
            }, explain=True)
        assert resp["index"]["type"] == "json"

    @unittest.skipUnless(mango.has_text_service(), "requires text service")
    def test_with_text(self):
        resp = self.db.find({
                "$text" : "Stephanie",
                "name.first": "Stephanie",
                "name.last": "This doesn't have to match anything."
            }, explain=True)
        assert resp["index"]["type"] == "text"

    @unittest.skipUnless(mango.has_text_service(), "requires text service")
    def test_no_view_index(self):
        resp = self.db.find({"name.first": "Ohai!"}, explain=True)
        assert resp["index"]["type"] == "text"

    @unittest.skipUnless(mango.has_text_service(), "requires text service")
    def test_with_or(self):
        resp = self.db.find({
                "$or": [
                    {"name.first": "Stephanie"},
                    {"name.last": "This doesn't have to match anything."}
                ]
            }, explain=True)
        assert resp["index"]["type"] == "text"

    def test_use_most_columns(self):
        # ddoc id for the age index
        ddocid = "_design/ad3d537c03cd7c6a43cf8dff66ef70ea54c2b40f"
        resp = self.db.find({
                "name.first": "Stephanie",
                "name.last": "Something or other",
                "age": {"$gt": 1}
            }, explain=True)
        assert resp["index"]["ddoc"] != "_design/" + ddocid

        resp = self.db.find({
                "name.first": "Stephanie",
                "name.last": "Something or other",
                "age": {"$gt": 1}
            }, use_index=ddocid, explain=True)
        assert resp["index"]["ddoc"] == ddocid

    def test_use_most_columns(self):
        # ddoc id for the age index
        ddocid = "_design/ad3d537c03cd7c6a43cf8dff66ef70ea54c2b40f"
        try:
            self.db.find({}, use_index=ddocid)
        except Exception, e:
            assert e.response.status_code == 400
        else:
            raise AssertionError("bad find")

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

    @unittest.skipUnless(mango.has_text_service(), "requires text service")
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
        assert len(docs) == 1
        assert docs[0]["name"]["first"] == "Stephanie"
        assert docs[0]["age"] == 48


@unittest.skipUnless(mango.has_text_service(), "requires text service")
class MultiTextIndexSelectionTests(mango.UserDocsTests):
    @classmethod
    def setUpClass(klass):
        super(MultiTextIndexSelectionTests, klass).setUpClass()
        if mango.has_text_service():
            klass.db.create_text_index(ddoc="foo", analyzer="keyword")
            klass.db.create_text_index(ddoc="bar", analyzer="email")

    def test_view_ok_with_multi_text(self):
        resp = self.db.find({"name.last": "A last name"}, explain=True)
        assert resp["index"]["type"] == "json"

    def test_multi_text_index_is_error(self):
        try:
            self.db.find({"$text": "a query"}, explain=True)
        except Exception, e:
            assert e.response.status_code == 400

    def test_use_index_works(self):
        resp = self.db.find({"$text": "a query"}, use_index="foo", explain=True)
        assert resp["index"]["ddoc"] == "_design/foo"
