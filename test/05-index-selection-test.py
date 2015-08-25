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
        # user_docs.add_text_indexes(klass.db, {})

    def test_basic(self):
        resp = self.db.find({"name.last": "A last name"}, explain=True)
        assert resp["index"]["type"] == "json"

    def test_with_and(self):
        resp = self.db.find({
                "name.first": "Stephanie",
                "name.last": "This doesn't have to match anything."
            }, explain=True)
        assert resp["index"]["type"] == "json"

    def test_with_text(self):
        resp = self.db.find({
                "$text" : "Stephanie",
                "name.first": "Stephanie",
                "name.last": "This doesn't have to match anything."
            }, explain=True)
        assert resp["index"]["type"] == "text"

    @unittest.skip
    def test_no_view_index(self):
        resp = self.db.find({"name.first": "Ohai!"}, explain=True)
        assert resp["index"]["type"] == "text"

    @unittest.skip
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


class MultiTextIndexSelectionTests(mango.UserDocsTests):
    @classmethod
    def setUpClass(klass):
        raise unittest.SkipTest('text index is not supported yet')
        super(MultiTextIndexSelectionTests, klass).setUpClass()
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
