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
import copy

DOCS = [
    {
        "_id": "100",
        "name": "Jimi",
        "location": "AUS",
        "user_id": 1,
        "same": "value"
    },
    {
        "_id": "200",
        "name": "Eddie",
        "location": "BRA",
        "user_id": 2,
        "same": "value"
    },
    {
        "_id": "300",
        "name": "Harry",
        "location": "CAN",
        "user_id":3,
        "same": "value"
    },
    {
        "_id": "400",
        "name": "Eddie",
        "location": "DEN",
        "user_id":4,
        "same": "value"
    },
    {
        "_id": "500",
        "name": "Jones",
        "location": "ETH",
        "user_id":5,
        "same": "value"
    },
    {
        "_id": "600",
        "name": "Winnifried",
        "location": "FRA",
        "user_id":6,
        "same": "value"
    },
    {
        "_id": "700",
        "name": "Marilyn",
        "location": "GHA",
        "user_id":7,
        "same": "value"
    },
    {
        "_id": "800",
        "name": "Sandra",
        "location": "ZAR",
        "user_id":8,
        "same": "value"
    },
]

class PaginateJsonDocs(mango.DbPerClass):
    def setUp(self):
        self.db.recreate()
        self.db.save_docs(copy.deepcopy(DOCS))

    def test_all_docs_paginate_to_end(self):
        selector = {"_id": {"$gt": 0}}  
        # Page 1
        resp = self.db.find(selector, fields=["_id"], limit=5, return_raw=True)
        bookmark = resp['bookmark']
        docs = resp['docs']
        assert docs[0]['_id'] == '100'
        assert len(docs) == 5

        # Page 2
        resp = self.db.find(selector, fields=["_id"], bookmark= bookmark, limit=5, return_raw=True)
        bookmark = resp['bookmark']
        docs = resp['docs']
        assert docs[0]['_id'] == '600'
        assert len(docs) == 3

        # Page 3 
        resp = self.db.find(selector, bookmark= bookmark, limit=5, return_raw=True)
        bookmark = resp['bookmark']
        docs = resp['docs']
        assert len(docs) == 0

    def test_return_previous_bookmark_for_empty(self):
        selector = {"_id": {"$gt": 0}}  
        # Page 1
        resp = self.db.find(selector, fields=["_id"], return_raw=True)
        bookmark1 = resp['bookmark']
        docs = resp['docs']
        assert len(docs) == 8

        resp = self.db.find(selector, fields=["_id"], return_raw=True, bookmark=bookmark1)
        bookmark2 = resp['bookmark']
        docs = resp['docs']
        assert len(docs) == 0

        resp = self.db.find(selector, fields=["_id"], return_raw=True, bookmark=bookmark2)
        bookmark3 = resp['bookmark']
        docs = resp['docs']
        assert bookmark3 == bookmark2
        assert len(docs) == 0

    def test_all_docs_with_skip(self):
        selector = {"_id": {"$gt": 0}}  
        # Page 1
        resp = self.db.find(selector, fields=["_id"], skip=2, limit=5, return_raw=True)
        bookmark = resp['bookmark']
        docs = resp['docs']
        assert docs[0]['_id'] == '300'
        assert len(docs) == 5

        # Page 2
        resp = self.db.find(selector, fields=["_id"], bookmark= bookmark, limit=5, return_raw=True)
        bookmark = resp['bookmark']
        docs = resp['docs']
        assert docs[0]['_id'] == '800'
        assert len(docs) == 1
        resp = self.db.find(selector, bookmark= bookmark, limit=5, return_raw=True)
        bookmark = resp['bookmark']
        docs = resp['docs']
        assert len(docs) == 0

    def test_all_docs_reverse(self):
        selector = {"_id": {"$gt": 0}} 
        resp = self.db.find(selector, fields=["_id"], sort=[{"_id": "desc"}], limit=5, return_raw=True)
        docs = resp['docs']
        bookmark1 = resp["bookmark"]
        assert len(docs) == 5
        assert docs[0]['_id'] == '800'

        resp = self.db.find(selector, fields=["_id"], sort=[{"_id": "desc"}], limit=5, return_raw=True, bookmark=bookmark1)
        docs = resp['docs']
        bookmark2 = resp["bookmark"]
        assert len(docs) == 3
        assert docs[0]['_id'] == '300'

        resp = self.db.find(selector, fields=["_id"], sort=[{"_id": "desc"}], limit=5, return_raw=True, bookmark=bookmark2)
        docs = resp['docs']
        assert len(docs) == 0

    def test_bad_bookmark(self):
        try:
            self.db.find({"_id": {"$gt": 0}}, bookmark="bad-bookmark")
        except Exception as e:
            resp = e.response.json()
            assert resp["error"] == "invalid_bookmark"
            assert resp["reason"] == "Invalid bookmark value: \"bad-bookmark\""
            assert e.response.status_code == 400
        else:
            raise AssertionError("Should have thrown error for bad bookmark")
    
    def test_throws_error_on_text_bookmark(self):
        bookmark = 'g2wAAAABaANkABFub2RlMUBjb3VjaGRiLm5ldGwAAAACYQBiP____2poAkY_8AAAAAAAAGEHag'
        try:
            self.db.find({"_id": {"$gt": 0}}, bookmark=bookmark)
        except Exception as e:
            resp = e.response.json()
            assert resp["error"] == "invalid_bookmark"
            assert e.response.status_code == 400
        else:
            raise AssertionError("Should have thrown error for bad bookmark")
    
    def test_index_pagination(self):
        self.db.create_index(["location"])
        selector = {"location": {"$gt": "A"}} 
        resp = self.db.find(selector, fields=["_id"], limit=5, return_raw=True)
        docs = resp['docs']
        bookmark1 = resp["bookmark"]
        assert len(docs) == 5
        assert docs[0]['_id'] == '100'

        resp = self.db.find(selector, fields=["_id"], limit=5, return_raw=True, bookmark=bookmark1)
        docs = resp['docs']
        bookmark2 = resp["bookmark"]
        assert len(docs) == 3
        assert docs[0]['_id'] == '600'

        resp = self.db.find(selector, fields=["_id"], limit=5, return_raw=True, bookmark=bookmark2)
        docs = resp['docs']
        assert len(docs) == 0

    def test_index_pagination_two_keys(self):
        self.db.create_index(["location", "user_id"])
        selector = {"location": {"$gt": "A"}, "user_id": {"$gte": 1}} 
        resp = self.db.find(selector, fields=["_id"], limit=5, return_raw=True)
        docs = resp['docs']
        bookmark1 = resp["bookmark"]
        assert len(docs) == 5
        assert docs[0]['_id'] == '100'

        resp = self.db.find(selector, fields=["_id"], limit=5, return_raw=True, bookmark=bookmark1)
        docs = resp['docs']
        bookmark2 = resp["bookmark"]
        assert len(docs) == 3
        assert docs[0]['_id'] == '600'

        resp = self.db.find(selector, fields=["_id"], limit=5, return_raw=True, bookmark=bookmark2)
        docs = resp['docs']
        assert len(docs) == 0

    def test_index_pagination_reverse(self):
        self.db.create_index(["location", "user_id"])
        selector = {"location": {"$gt": "A"}, "user_id": {"$gte": 1}} 
        sort = [{"location": "desc"}, {"user_id": "desc"}]
        resp = self.db.find(selector, fields=["_id"], sort=sort, limit=5, return_raw=True)
        docs = resp['docs']
        bookmark1 = resp["bookmark"]
        assert len(docs) == 5
        assert docs[0]['_id'] == '800'

        resp = self.db.find(selector, fields=["_id"], limit=5, sort=sort, return_raw=True, bookmark=bookmark1)
        docs = resp['docs']
        bookmark2 = resp["bookmark"]
        assert len(docs) == 3
        assert docs[0]['_id'] == '300'

        resp = self.db.find(selector, fields=["_id"], limit=5, sort=sort, return_raw=True, bookmark=bookmark2)
        docs = resp['docs']
        assert len(docs) == 0

    def test_index_pagination_same_emitted_key(self):
        self.db.create_index(["same"])
        selector = {"same": {"$gt": ""}} 
        resp = self.db.find(selector, fields=["_id"], limit=5, return_raw=True)
        docs = resp['docs']
        bookmark1 = resp["bookmark"]
        assert len(docs) == 5
        assert docs[0]['_id'] == '100'

        resp = self.db.find(selector, fields=["_id"], limit=5, return_raw=True, bookmark=bookmark1)
        docs = resp['docs']
        bookmark2 = resp["bookmark"]
        assert len(docs) == 3
        assert docs[0]['_id'] == '600'

        resp = self.db.find(selector, fields=["_id"], limit=5, return_raw=True, bookmark=bookmark2)
        docs = resp['docs']
        assert len(docs) == 0
