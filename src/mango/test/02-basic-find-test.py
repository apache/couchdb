# -*- coding: latin-1 -*-
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

class BasicFindTests(mango.UserDocsTests):

    def test_bad_selector(self):
        bad_selectors = [
            None,
            True,
            False,
            1.0,
            "foobarbaz",
            {"foo":{"$not_an_op": 2}},
            {"$gt":2},
            [None, "bing"]
        ]
        for bs in bad_selectors:
            try:
                self.db.find(bs)
            except Exception, e:
                assert e.response.status_code == 400
            else:
                raise AssertionError("bad find")

    def test_bad_limit(self):
        bad_limits = [
            None,
            True,
            False,
            -1,
            1.2,
            "no limit!",
            {"foo": "bar"},
            [2]
        ],
        for bl in bad_limits:
            try:
                self.db.find({"int":{"$gt":2}}, limit=bl)
            except Exception, e:
                assert e.response.status_code == 400
            else:
                raise AssertionError("bad find")

    def test_bad_skip(self):
        bad_skips = [
            None,
            True,
            False,
            -3,
            1.2,
            "no limit!",
            {"foo": "bar"},
            [2]
        ],
        for bs in bad_skips:
            try:
                self.db.find({"int":{"$gt":2}}, skip=bs)
            except Exception, e:
                assert e.response.status_code == 400
            else:
                raise AssertionError("bad find")

    def test_bad_sort(self):
        bad_sorts = [
            None,
            True,
            False,
            1.2,
            "no limit!",
            {"foo": "bar"},
            [2],
            [{"foo":"asc", "bar": "asc"}],
            [{"foo":"asc"}, {"bar":"desc"}],
        ],
        for bs in bad_sorts:
            try:
                self.db.find({"int":{"$gt":2}}, sort=bs)
            except Exception, e:
                assert e.response.status_code == 400
            else:
                raise AssertionError("bad find")

    def test_bad_fields(self):
        bad_fields = [
            None,
            True,
            False,
            1.2,
            "no limit!",
            {"foo": "bar"},
            [2],
            [[]],
            ["foo", 2.0],
        ],
        for bf in bad_fields:
            try:
                self.db.find({"int":{"$gt":2}}, fields=bf)
            except Exception, e:
                assert e.response.status_code == 400
            else:
                raise AssertionError("bad find")

    def test_bad_r(self):
        bad_rs = [
            None,
            True,
            False,
            1.2,
            "no limit!",
            {"foo": "bar"},
            [2],
        ],
        for br in bad_rs:
            try:
                self.db.find({"int":{"$gt":2}}, r=br)
            except Exception, e:
                assert e.response.status_code == 400
            else:
                raise AssertionError("bad find")

    def test_bad_conflicts(self):
        bad_conflicts = [
            None,
            1.2,
            "no limit!",
            {"foo": "bar"},
            [2],
        ],
        for bc in bad_conflicts:
            try:
                self.db.find({"int":{"$gt":2}}, conflicts=bc)
            except Exception, e:
                assert e.response.status_code == 400
            else:
                raise AssertionError("bad find")

    def test_simple_find(self):
        docs = self.db.find({"age": {"$lt": 35}})
        assert len(docs) == 3
        assert docs[0]["user_id"] == 9
        assert docs[1]["user_id"] == 1
        assert docs[2]["user_id"] == 7

    def test_multi_cond_and(self):
        docs = self.db.find({"manager": True, "location.city": "Longbranch"})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 7

    def test_multi_cond_or(self):
        docs = self.db.find({
                "$and":[
                    {"age":{"$gte": 75}},
                    {"$or": [
                        {"name.first": "Mathis"},
                        {"name.first": "Whitley"}
                    ]}
                ]
            })
        assert len(docs) == 2
        assert docs[0]["user_id"] == 11
        assert docs[1]["user_id"] == 13

    def test_multi_col_idx(self):
        docs = self.db.find({
            "location.state": {"$and": [
                {"$gt": "Hawaii"},
                {"$lt": "Maine"}
            ]},
            "location.city": {"$lt": "Longbranch"}
        })
        assert len(docs) == 1
        assert docs[0]["user_id"] == 6

    def test_missing_not_indexed(self):
        docs = self.db.find({"favorites.3": "C"})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 6

        docs = self.db.find({"favorites.3": None})
        assert len(docs) == 0

        docs = self.db.find({"twitter": {"$gt": None}})
        assert len(docs) == 4
        assert docs[0]["user_id"] == 1
        assert docs[1]["user_id"] == 4
        assert docs[2]["user_id"] == 0
        assert docs[3]["user_id"] == 13

    def test_limit(self):
        docs = self.db.find({"age": {"$gt": 0}})
        assert len(docs) == 15
        for l in [0, 1, 5, 14]:
            docs = self.db.find({"age": {"$gt": 0}}, limit=l)
            assert len(docs) == l

    def test_skip(self):
        docs = self.db.find({"age": {"$gt": 0}})
        assert len(docs) == 15
        for s in [0, 1, 5, 14]:
            docs = self.db.find({"age": {"$gt": 0}}, skip=s)
            assert len(docs) == (15 - s)

    def test_sort(self):
        docs1 = self.db.find({"age": {"$gt": 0}}, sort=[{"age":"asc"}])
        docs2 = list(sorted(docs1, key=lambda d: d["age"]))
        assert docs1 is not docs2 and docs1 == docs2

        docs1 = self.db.find({"age": {"$gt": 0}}, sort=[{"age":"desc"}])
        docs2 = list(reversed(sorted(docs1, key=lambda d: d["age"])))
        assert docs1 is not docs2 and docs1 == docs2

    def test_fields(self):
        selector = {"age": {"$gt": 0}}
        docs = self.db.find(selector, fields=["user_id", "location.address"])
        for d in docs:
            assert sorted(d.keys()) == ["location", "user_id"]
            assert sorted(d["location"].keys()) == ["address"]

    def test_r(self):
        for r in [1, 2, 3]:
            docs = self.db.find({"age": {"$gt": 0}}, r=r)
            assert len(docs) == 15

    def test_empty(self):
        docs = self.db.find({})
        # 15 users 
        assert len(docs) == 15

    def test_empty_subsel(self):
        docs = self.db.find({
                "_id": {"$gt": None},
                "location": {}
            })
        assert len(docs) == 0

    def test_empty_subsel_match(self):
        self.db.save_docs([{"user_id": "eo", "empty_obj": {}}])
        docs = self.db.find({
                "_id": {"$gt": None},
                "empty_obj": {}
            })
        assert len(docs) == 1
        assert docs[0]["user_id"] == "eo"

    def test_unsatisfiable_range(self):
        docs = self.db.find({
                "$and":[
                    {"age":{"$gt": 0}},
                    {"age":{"$lt": 0}}
                ]
            })
        assert len(docs) == 0

    def test_explain_view_args(self):
        explain = self.db.find({
               "age":{"$gt": 0}
            }, fields=["manager"],
            explain=True)
        assert explain["mrargs"]["stable"] == False
        assert explain["mrargs"]["update"] == True
        assert explain["mrargs"]["reduce"] == False
        assert explain["mrargs"]["start_key"] == [0]
        assert explain["mrargs"]["end_key"] == [{}]
        assert explain["mrargs"]["include_docs"] == True
