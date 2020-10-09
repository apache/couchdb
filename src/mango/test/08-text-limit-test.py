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
import limit_docs
import unittest


@unittest.skipUnless(mango.has_text_service(), "requires text service")
class LimitTests(mango.LimitDocsTextTests):
    def test_limit_field(self):
        q = {"$or": [{"user_id": {"$lt": 10}}, {"filtered_array.[]": 1}]}
        docs = self.db.find(q, limit=10)
        assert len(docs) == 8
        for d in docs:
            assert d["user_id"] < 10

    def test_limit_field2(self):
        q = {"$or": [{"user_id": {"$lt": 20}}, {"filtered_array.[]": 1}]}
        docs = self.db.find(q, limit=10)
        assert len(docs) == 10
        for d in docs:
            assert d["user_id"] < 20

    def test_limit_field3(self):
        q = {"$or": [{"user_id": {"$lt": 100}}, {"filtered_array.[]": 1}]}
        docs = self.db.find(q, limit=1)
        assert len(docs) == 1
        for d in docs:
            assert d["user_id"] < 100

    def test_limit_field4(self):
        q = {"$or": [{"user_id": {"$lt": 0}}, {"filtered_array.[]": 1}]}
        docs = self.db.find(q, limit=35)
        assert len(docs) == 0

    # We reach our cap here of 50
    def test_limit_field5(self):
        q = {"age": {"$exists": True}}
        docs = self.db.find(q, limit=250)
        assert len(docs) == 75
        for d in docs:
            assert d["age"] < 100

    def test_limit_skip_field1(self):
        q = {"$or": [{"user_id": {"$lt": 100}}, {"filtered_array.[]": 1}]}
        docs = self.db.find(q, limit=10, skip=20)
        assert len(docs) == 10
        for d in docs:
            assert d["user_id"] > 20

    def test_limit_skip_field2(self):
        q = {"$or": [{"user_id": {"$lt": 100}}, {"filtered_array.[]": 1}]}
        docs = self.db.find(q, limit=100, skip=100)
        assert len(docs) == 0

    def test_limit_skip_field3(self):
        q = {"$or": [{"user_id": {"$lt": 20}}, {"filtered_array.[]": 1}]}
        docs = self.db.find(q, limit=1, skip=30)
        assert len(docs) == 0

    def test_limit_skip_field4(self):
        q = {"$or": [{"user_id": {"$lt": 100}}, {"filtered_array.[]": 1}]}
        docs = self.db.find(q, limit=0, skip=0)
        assert len(docs) == 0

    def test_limit_skip_field5(self):
        q = {"$or": [{"user_id": {"$lt": 100}}, {"filtered_array.[]": 1}]}
        try:
            self.db.find(q, limit=-1)
        except Exception as e:
            assert e.response.status_code == 400
        else:
            raise AssertionError("Should have thrown error for negative limit")

    def test_limit_skip_field6(self):
        q = {"$or": [{"user_id": {"$lt": 100}}, {"filtered_array.[]": 1}]}
        try:
            self.db.find(q, skip=-1)
        except Exception as e:
            assert e.response.status_code == 400
        else:
            raise AssertionError("Should have thrown error for negative skip")

    # Basic test to ensure we can iterate through documents with a bookmark
    def test_limit_bookmark(self):
        for i in range(1, len(limit_docs.DOCS), 5):
            self.run_bookmark_check(i)

        for i in range(1, len(limit_docs.DOCS), 5):
            self.run_bookmark_sort_check(i)

    def run_bookmark_check(self, size):
        q = {"age": {"$gt": 0}}
        seen_docs = set()
        bm = None
        while True:
            json = self.db.find(q, limit=size, bookmark=bm, return_raw=True)
            for doc in json["docs"]:
                assert doc["_id"] not in seen_docs
                seen_docs.add(doc["_id"])
            if not len(json["docs"]):
                break
            assert json["bookmark"] != bm
            bm = json["bookmark"]
        assert len(seen_docs) == len(limit_docs.DOCS)

    def run_bookmark_sort_check(self, size):
        q = {"age": {"$gt": 0}}
        seen_docs = set()
        bm = None
        age = 0
        while True:
            json = self.db.find(
                q, limit=size, bookmark=bm, sort=["age"], return_raw=True
            )
            for doc in json["docs"]:
                assert doc["_id"] not in seen_docs
                assert doc["age"] >= age
                age = doc["age"]
                seen_docs.add(doc["_id"])
            if not len(json["docs"]):
                break
            assert json["bookmark"] != bm
            bm = json["bookmark"]
        assert len(seen_docs) == len(limit_docs.DOCS)

    def run_explain_check(self, size):
        q = {"age": {"$gt": 0}}
        seen_docs = set()
        bm = None
        results1 = self.db.find(q, limit=size, bookmark=bm, return_raw=True)
        assert results1["bookmark"] != bm
        bm = results1["bookmark"]
        results2 = self.db.find(q, limit=size, bookmark=bm, explain=True)
        assert results2["bookmark"] == bm
