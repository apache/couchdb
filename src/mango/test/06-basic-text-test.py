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
from hypothesis import given, assume, example
import hypothesis.strategies as st

@unittest.skipIf(mango.has_text_service(), "text service exists")
class TextIndexCheckTests(mango.DbPerClass):

    def test_create_text_index(self):
        body = json.dumps({
            'index': {
            },
            'type': 'text'
        })
        resp = self.db.sess.post(self.db.path("_index"), data=body)
        assert resp.status_code == 503, resp

@unittest.skipUnless(mango.has_text_service(), "requires text service")
class BasicTextTests(mango.UserDocsTextTests):

    def test_simple(self):
        docs = self.db.find({"$text": "Stephanie"})
        assert len(docs) == 1
        assert docs[0]["name"]["first"] == "Stephanie"

    def test_with_integer(self):
        docs = self.db.find({"name.first": "Stephanie", "age": 48})
        assert len(docs) == 1
        assert docs[0]["name"]["first"] == "Stephanie"
        assert docs[0]["age"] == 48

    def test_with_boolean(self):
        docs = self.db.find({"name.first": "Stephanie", "manager": False})
        assert len(docs) == 1
        assert docs[0]["name"]["first"] == "Stephanie"
        assert docs[0]["manager"] == False

    def test_with_array(self):
        faves = ["Ruby", "C", "Python"]
        docs = self.db.find({"name.first": "Stephanie", "favorites": faves})
        assert docs[0]["name"]["first"] == "Stephanie"
        assert docs[0]["favorites"] == faves

    def test_array_ref(self):
        docs = self.db.find({"favorites.1": "Python"})
        assert len(docs) == 4
        for d in docs:
            assert "Python" in d["favorites"]

        # Nested Level
        docs = self.db.find({"favorites.0.2": "Python"})
        assert len(docs) == 1
        for d in docs:
            assert "Python" in d["favorites"][0][2]

    def test_number_ref(self):
        docs = self.db.find({"11111": "number_field"})
        assert len(docs) == 1
        assert docs[0]["11111"] == "number_field"

        docs = self.db.find({"22222.33333": "nested_number_field"})
        assert len(docs) == 1
        assert docs[0]["22222"]["33333"] == "nested_number_field"

    def test_lt(self):
        docs = self.db.find({"age": {"$lt": 22}})
        assert len(docs) == 0

        docs = self.db.find({"age": {"$lt": 23}})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 9

        docs = self.db.find({"age": {"$lt": 33}})
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (1, 9)

        docs = self.db.find({"age": {"$lt": 34}})
        assert len(docs) == 3
        for d in docs:
            assert d["user_id"] in (1, 7, 9)

        docs = self.db.find({"company": {"$lt": "Dreamia"}})
        assert len(docs) == 1
        assert docs[0]["company"] == "Affluex"

    def test_lte(self):
        docs = self.db.find({"age": {"$lte": 21}})
        assert len(docs) == 0

        docs = self.db.find({"age": {"$lte": 22}})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 9

        docs = self.db.find({"age": {"$lte": 33}})
        assert len(docs) == 3
        for d in docs:
            assert d["user_id"] in (1, 7, 9)

        docs = self.db.find({"company": {"$lte": "Dreamia"}})
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (0, 11)

    def test_eq(self):
        docs = self.db.find({"age": 21})
        assert len(docs) == 0

        docs = self.db.find({"age": 22})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 9

        docs = self.db.find({"age": {"$eq": 22}})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 9

        docs = self.db.find({"age": 33})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 7

    def test_ne(self):
        docs = self.db.find({"age": {"$ne": 22}})
        assert len(docs) == len(user_docs.DOCS) - 1
        for d in docs:
            assert d["age"] != 22

        docs = self.db.find({"$not": {"age": 22}})
        assert len(docs) == len(user_docs.DOCS) - 1
        for d in docs:
            assert d["age"] != 22

    def test_gt(self):
        docs = self.db.find({"age": {"$gt": 77}})
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (3, 13)

        docs = self.db.find({"age": {"$gt": 78}})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 3

        docs = self.db.find({"age": {"$gt": 79}})
        assert len(docs) == 0

        docs = self.db.find({"company": {"$gt": "Zialactic"}})
        assert len(docs) == 0

    def test_gte(self):
        docs = self.db.find({"age": {"$gte": 77}})
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (3, 13)

        docs = self.db.find({"age": {"$gte": 78}})
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (3, 13)

        docs = self.db.find({"age": {"$gte": 79}})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 3

        docs = self.db.find({"age": {"$gte": 80}})
        assert len(docs) == 0

        docs = self.db.find({"company": {"$gte": "Zialactic"}})
        assert len(docs) == 1
        assert docs[0]["company"] == "Zialactic"

    def test_and(self):
        docs = self.db.find({"age": 22, "manager": True})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 9

        docs = self.db.find({"age": 22, "manager": False})
        assert len(docs) == 0

        docs = self.db.find({"$and": [{"age": 22}, {"manager": True}]})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 9

        docs = self.db.find({"$and": [{"age": 22}, {"manager": False}]})
        assert len(docs) == 0

        docs = self.db.find({"$text": "Ramona", "age": 22})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 9

        docs = self.db.find({"$and": [{"$text": "Ramona"}, {"age": 22}]})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 9

        docs = self.db.find({"$and": [{"$text": "Ramona"}, {"$text": "Floyd"}]})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 9

    def test_or(self):
        docs = self.db.find({"$or": [{"age": 22}, {"age": 33}]})
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (7, 9)

        q = {"$or": [{"$text": "Ramona"}, {"$text": "Stephanie"}]}
        docs = self.db.find(q)
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (0, 9)

        q = {"$or": [{"$text": "Ramona"}, {"age": 22}]}
        docs = self.db.find(q)
        assert len(docs) == 1
        assert docs[0]["user_id"] == 9

    def test_and_or(self):
        q = {
            "age": 22,
            "$or": [
                {"manager": False},
                {"location.state": "Missouri"}
            ]
        }
        docs = self.db.find(q)
        assert len(docs) == 1
        assert docs[0]["user_id"] == 9

        q = {
            "$or": [
                {"age": 22},
                {"age": 43, "manager": True}
            ]
        }
        docs = self.db.find(q)
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (9, 10)

        q = {
            "$or": [
                {"$text": "Ramona"},
                {"age": 43, "manager": True}
            ]
        }
        docs = self.db.find(q)
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (9, 10)

    def test_nor(self):
        docs = self.db.find({"$nor": [{"age": 22}, {"age": 33}]})
        assert len(docs) == 13
        for d in docs:
            assert d["user_id"] not in (7, 9)

    def test_in_with_value(self):
        docs = self.db.find({"age": {"$in": [1, 5]}})
        assert len(docs) == 0

        docs = self.db.find({"age": {"$in": [1, 5, 22]}})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 9

        docs = self.db.find({"age": {"$in": [1, 5, 22, 31]}})
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (1, 9)

        docs = self.db.find({"age": {"$in": [22, 31]}})
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (1, 9)

        # Limits on boolean clauses?
        docs = self.db.find({"age": {"$in": range(1000)}})
        assert len(docs) == 15

    def test_in_with_array(self):
        vals = ["Random Garbage", 52, {"Versions": {"Alpha": "Beta"}}]
        docs = self.db.find({"favorites": {"$in": vals}})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 1

        vals = ["Lisp", "Python"]
        docs = self.db.find({"favorites": {"$in": vals}})
        assert len(docs) == 10

        vals = [{"val1": 1, "val2": "val2"}]
        docs = self.db.find({"test_in": {"$in": vals}})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 2

    def test_nin_with_value(self):
        docs = self.db.find({"age": {"$nin": [1, 5]}})
        assert len(docs) == len(user_docs.DOCS)

        docs = self.db.find({"age": {"$nin": [1, 5, 22]}})
        assert len(docs) == len(user_docs.DOCS) - 1
        for d in docs:
            assert d["user_id"] != 9

        docs = self.db.find({"age": {"$nin": [1, 5, 22, 31]}})
        assert len(docs) == len(user_docs.DOCS) - 2
        for d in docs:
            assert d["user_id"] not in (1, 9)

        docs = self.db.find({"age": {"$nin": [22, 31]}})
        assert len(docs) == len(user_docs.DOCS) - 2
        for d in docs:
            assert d["user_id"] not in (1, 9)

        # Limits on boolean clauses?
        docs = self.db.find({"age": {"$nin": range(1000)}})
        assert len(docs) == 0

    def test_nin_with_array(self):
        vals = ["Random Garbage", 52, {"Versions": {"Alpha": "Beta"}}]
        docs = self.db.find({"favorites": {"$nin": vals}})
        assert len(docs) == len(user_docs.DOCS) - 1
        for d in docs:
            assert d["user_id"] != 1

        vals = ["Lisp", "Python"]
        docs = self.db.find({"favorites": {"$nin": vals}})
        assert len(docs) == 5

        vals = [{"val1": 1, "val2": "val2"}]
        docs = self.db.find({"test_in": {"$nin": vals}})
        assert len(docs) == 0

    def test_all(self):
        vals = ["Ruby", "C", "Python", {"Versions": {"Alpha": "Beta"}}]
        docs = self.db.find({"favorites": {"$all": vals}})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 1

        # This matches where favorites either contains
        # the nested array, or is the nested array. This is
        # notably different than the non-nested array in that
        # it does not match a re-ordered version of the array.
        # The fact that user_id 14 isn't included demonstrates
        # this behavior.
        vals = [["Lisp", "Erlang", "Python"]]
        docs = self.db.find({"favorites": {"$all": vals}})
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (3, 9)

    def test_exists_field(self):
        docs = self.db.find({"exists_field": {"$exists": True}})
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (7, 8)

        docs = self.db.find({"exists_field": {"$exists": False}})
        assert len(docs) == len(user_docs.DOCS) - 2
        for d in docs:
            assert d["user_id"] not in (7, 8)

    def test_exists_array(self):
        docs = self.db.find({"exists_array": {"$exists": True}})
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (9, 10)

        docs = self.db.find({"exists_array": {"$exists": False}})
        assert len(docs) == len(user_docs.DOCS) - 2
        for d in docs:
            assert d["user_id"] not in (9, 10)

    def test_exists_object(self):
        docs = self.db.find({"exists_object": {"$exists": True}})
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (11, 12)

        docs = self.db.find({"exists_object": {"$exists": False}})
        assert len(docs) == len(user_docs.DOCS) - 2
        for d in docs:
            assert d["user_id"] not in (11, 12)

    def test_exists_object_member(self):
        docs = self.db.find({"exists_object.should": {"$exists": True}})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 11

        docs = self.db.find({"exists_object.should": {"$exists": False}})
        assert len(docs) == len(user_docs.DOCS) - 1
        for d in docs:
            assert d["user_id"] != 11

    def test_exists_and(self):
        q = {"$and": [
            {"manager": {"$exists": True}},
            {"exists_object.should": {"$exists": True}}
        ]}
        docs = self.db.find(q)
        assert len(docs) == 1
        assert docs[0]["user_id"] == 11

        q = {"$and": [
            {"manager": {"$exists": False}},
            {"exists_object.should": {"$exists": True}}
        ]}
        docs = self.db.find(q)
        assert len(docs) == 0

        # Translates to manager exists or exists_object.should doesn't
        # exist, which will match all docs
        q = {"$not": q}
        docs = self.db.find(q)
        assert len(docs) == len(user_docs.DOCS)

    def test_value_chars(self):
        q = {"complex_field_value": "+-(){}[]^~&&*||\"\\/?:!"}
        docs = self.db.find(q)
        assert len(docs) == 1

    def test_regex(self):
        docs = self.db.find({
                "age": {"$gt": 40},
                "location.state": {"$regex": "(?i)new.*"}
            })
        assert len(docs) == 2
        assert docs[0]["user_id"] == 2
        assert docs[1]["user_id"] == 10

    # test lucene syntax in $text

@unittest.skipUnless(mango.has_text_service(), "requires text service")
class ElemMatchTests(mango.FriendDocsTextTests):

    def test_elem_match_non_object(self):
        q = {"bestfriends":{
                "$elemMatch":
                    {"$eq":"Wolverine", "$eq":"Cyclops"}
            }
        }
        docs = self.db.find(q)
        assert len(docs) == 1
        assert docs[0]["bestfriends"] == ["Wolverine", "Cyclops"]

        q = {"results": {"$elemMatch": {"$gte": 80, "$lt": 85}}}

        docs = self.db.find(q)
        assert len(docs) == 1
        assert docs[0]["results"] == [82, 85, 88]

    def test_elem_match(self):
        q = {"friends": {
                "$elemMatch":
                    {"name.first": "Vargas"}
            }
        }
        docs = self.db.find(q)
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (0, 1)

        q = {
            "friends": {
                "$elemMatch": {
                    "name.first": "Ochoa",
                    "name.last": "Burch"
                }
            }
        }
        docs = self.db.find(q)
        assert len(docs) == 1
        assert docs[0]["user_id"] == 4


        # Check that we can do logic in elemMatch
        q = {
            "friends": {"$elemMatch": {
                "name.first": "Ochoa", "type": "work"
            }}
        }
        docs = self.db.find(q)
        assert len(docs) == 1
        assert docs[0]["user_id"] == 1

        q = {
            "friends": {
                "$elemMatch": {
                    "name.first": "Ochoa",
                    "$or": [
                        {"type": "work"},
                        {"type": "personal"}
                    ]
                }
            }
        }
        docs = self.db.find(q)
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (1, 4)

        # Same as last, but using $in
        q = {
            "friends": {
                "$elemMatch": {
                    "name.first": "Ochoa",
                    "type": {"$in": ["work", "personal"]}
                }
            }
        }
        docs = self.db.find(q)
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (1, 4)

        q = {
            "$and": [{
                "friends": {
                    "$elemMatch": {
                        "id": 0,
                        "name": {
                            "$exists": True
                            }
                        }
                    }
                },
                {
                "friends": {
                    "$elemMatch": {
                        "$or": [
                            {
                            "name": {
                                "first": "Campos",
                                "last": "Freeman"
                                }
                            },
                            {
                            "name": {
                                "$in": [{
                                    "first": "Gibbs",
                                    "last": "Mccarty"
                                    },
                                    {
                                    "first": "Wilkins",
                                    "last": "Chang"
                                     }
                                    ]
                                    }
                                }
                            ]
                        }
                    }
                }
            ]
        }
        docs = self.db.find(q)
        assert len(docs) == 3
        for d in docs:
            assert d["user_id"] in (10, 11,12)

@unittest.skipUnless(mango.has_text_service(), "requires text service")
class AllMatchTests(mango.FriendDocsTextTests):

    def test_all_match(self):
        q = {"friends": {
                "$allMatch":
                    {"type": "personal"}
            }
        }
        docs = self.db.find(q)
        assert len(docs) == 2
        for d in docs:
            assert d["user_id"] in (8, 5)

        # Check that we can do logic in allMatch
        q = {
            "friends": {
                "$allMatch": {
                    "name.first": "Ochoa",
                    "$or": [
                        {"type": "work"},
                        {"type": "personal"}
                    ]
                }
            }
        }
        docs = self.db.find(q)
        assert len(docs) == 1
        assert docs[0]["user_id"] == 15

        # Same as last, but using $in
        q = {
            "friends": {
                "$allMatch": {
                    "name.first": "Ochoa",
                    "type": {"$in": ["work", "personal"]}
                }
            }
        }
        docs = self.db.find(q)
        assert len(docs) == 1
        assert docs[0]["user_id"] == 15


# Test numeric strings for $text
@unittest.skipUnless(mango.has_text_service(), "requires text service")
class NumStringTests(mango.DbPerClass):

    @classmethod
    def setUpClass(klass):
        super(NumStringTests, klass).setUpClass()
        klass.db.recreate()
        if mango.has_text_service():
            klass.db.create_text_index()

    # not available for python 2.7.x
    def isFinite(num):
        not (math.isinf(num) or math.isnan(num))

    @given(f=st.floats().filter(isFinite).map(str)
        | st.floats().map(lambda f: f.hex()))
    @example('NaN')
    @example('Infinity')
    def test_floating_point_val(self,f):
        doc = {"number_string": f}
        self.db.save_doc(doc)
        q = {"$text": f}
        docs = self.db.find(q)
        if len(docs) == 1:
            assert docs[0]["number_string"] == f
        if len(docs) == 2:
            if docs[0]["number_string"] != f:
                assert docs[1]["number_string"] == f
        q = {"number_string": f}
        docs = self.db.find(q)
        if len(docs) == 1:
            assert docs[0]["number_string"] == f
        if len(docs) == 2:
            if docs[0]["number_string"] != f:
                assert docs[1]["number_string"] == f
