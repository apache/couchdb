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


class OperatorTests(mango.UserDocsTests):

    def test_all(self):
        docs = self.db.find({
                "manager": True,
                "favorites": {"$all": ["Lisp", "Python"]}
            })
        print docs
        assert len(docs) == 4
        assert docs[0]["user_id"] == 2
        assert docs[1]["user_id"] == 12
        assert docs[2]["user_id"] == 9
        assert docs[3]["user_id"] == 14

    def test_all_non_array(self):
        docs = self.db.find({
                "manager": True,
                "location": {"$all": ["Ohai"]}
            })
        assert len(docs) == 0

    def test_elem_match(self):
        emdocs = [
            {
                "user_id": "a",
                "bang": [{
                    "foo": 1,
                    "bar": 2
                }]
            },
            {
                "user_id": "b",
                "bang": [{
                    "foo": 2,
                    "bam": True
                }]
            }
        ]
        self.db.save_docs(emdocs, w=3)
        docs = self.db.find({
            "_id": {"$gt": None},
            "bang": {"$elemMatch": {
                "foo": {"$gte": 1},
                "bam": True
            }}
        })
        print docs
        assert len(docs) == 1
        assert docs[0]["user_id"] == "b"

    def test_all_match(self):
        amdocs = [
            {
                "user_id": "a",
                "bang": [
                    {
                        "foo": 1,
                        "bar": 2
                    },
                    {
                        "foo": 3,
                        "bar": 4
                    }
                ]
            },
            {
                "user_id": "b",
                "bang": [
                    {
                        "foo": 1,
                        "bar": 2
                    },
                    {
                        "foo": 4,
                        "bar": 4
                    }
                ]
            }
        ]
        self.db.save_docs(amdocs, w=3)
        docs = self.db.find({
            "_id": {"$gt": None},
            "bang": {"$allMatch": {
                "foo": {"$mod": [2,1]},
                "bar": {"$mod": [2,0]}
            }}
        })
        assert len(docs) == 1
        assert docs[0]["user_id"] == "a"

    def test_in_operator_array(self):
        docs = self.db.find({
                "manager": True,
                "favorites": {"$in": ["Ruby", "Python"]}
            })
        assert len(docs) == 7
        assert docs[0]["user_id"] == 2
        assert docs[1]["user_id"] == 12

    def test_nin_operator_array(self):
        docs = self.db.find({
                "manager": True,
                "favorites": {"$nin": ["Erlang", "Python"]}
            })
        assert len(docs) == 4
        for doc in docs:
            if isinstance(doc["favorites"], list):
                assert "Erlang" not in doc["favorites"]
                assert "Python" not in doc["favorites"]

    def test_regex(self):
        docs = self.db.find({
                "age": {"$gt": 40},
                "location.state": {"$regex": "(?i)new.*"}
            })
        assert len(docs) == 2
        assert docs[0]["user_id"] == 2
        assert docs[1]["user_id"] == 10

    def test_exists_false(self):
        docs = self.db.find({
                "age": {"$gt": 0},
                "twitter": {"$exists": False}
            })
        user_ids = [2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 14]
        assert len(docs) == len(user_ids)
        for doc in docs:
            assert doc["user_id"] in user_ids
