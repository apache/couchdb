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
class SortTests(mango.UserDocsTextTests):

    def test_number_sort(self):
        q = {"age": {"$gt": 0}}
        docs = self.db.find(q, sort=["age:number"])
        assert len(docs) == 15
        assert docs[0]["age"] == 22

    def test_number_sort_desc(self):
        q = {"age": {"$gt": 0}}
        docs = self.db.find(q, sort=[{"age": "desc"}])
        assert len(docs) == 15
        assert docs[0]["age"] == 79

        q = {"manager": True}
        docs = self.db.find(q, sort=[{"age:number": "desc"}])
        assert len(docs) == 11
        assert docs[0]["age"] == 79

    def test_string_sort(self):
        q = {"email": {"$gt": None}}
        docs = self.db.find(q, sort=["email:string"])
        assert len(docs) == 15
        assert docs[0]["email"] == "abbottwatson@talkola.com"

    def test_notype_sort(self):
        q = {"email": {"$gt": None}}
        try:
            self.db.find(q, sort=["email"])
        except Exception as e:
            assert e.response.status_code == 400
        else:
            raise AssertionError("Should have thrown error for sort")

    def test_array_sort(self):
        q = {"favorites": {"$exists": True}}
        docs = self.db.find(q, sort=["favorites.[]:string"])
        assert len(docs) == 15
        assert docs[0]["user_id"] == 8

    def test_multi_sort(self):
        q = {"name": {"$exists": True}}
        docs = self.db.find(q, sort=["name.last:string", "age:number"])
        assert len(docs) == 15
        assert docs[0]["name"] == {"last":"Ewing","first":"Shelly"}
        assert docs[1]["age"] == 22

    def test_guess_type_sort(self):
        q = {"$or": [{"age":{"$gt": 0}}, {"email": {"$gt": None}}]}
        docs = self.db.find(q, sort=["age"])
        assert len(docs) == 15
        assert docs[0]["age"] == 22

    def test_guess_dup_type_sort(self):
        q = {"$and": [{"age":{"$gt": 0}}, {"email": {"$gt": None}},
            {"age":{"$lte": 100}}]}
        docs = self.db.find(q, sort=["age"])
        assert len(docs) == 15
        assert docs[0]["age"] == 22

    def test_ambiguous_type_sort(self):
        q = {"$or": [{"age":{"$gt": 0}}, {"email": {"$gt": None}},
            {"age": "34"}]}
        try:
            self.db.find(q, sort=["age"])
        except Exception as e:
            assert e.response.status_code == 400
        else:
            raise AssertionError("Should have thrown error for sort")

    def test_guess_multi_sort(self):
        q = {"$or": [{"age":{"$gt": 0}}, {"email": {"$gt": None}},
            {"name.last": "Harvey"}]}
        docs = self.db.find(q, sort=["name.last", "age"])
        assert len(docs) == 15
        assert docs[0]["name"] == {"last":"Ewing","first":"Shelly"}
        assert docs[1]["age"] == 22

    def test_guess_mix_sort(self):
        q = {"$or": [{"age":{"$gt": 0}}, {"email": {"$gt": None}},
            {"name.last": "Harvey"}]}
        docs = self.db.find(q, sort=["name.last:string", "age"])
        assert len(docs) == 15
        assert docs[0]["name"] == {"last":"Ewing","first":"Shelly"}
        assert docs[1]["age"] == 22
