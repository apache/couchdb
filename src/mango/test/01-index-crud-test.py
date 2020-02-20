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

import random

import mango
import copy
import unittest

DOCS = [
    {"_id": "1", "name": "Jimi", "age": 10, "cars": 1},
    {"_id": "2", "name": "kate", "age": 8, "cars": 0},
]


class IndexCrudTests(mango.DbPerClass):
    def setUp(self):
        self.db.recreate()

    def test_bad_fields(self):
        bad_fields = [
            None,
            True,
            False,
            "bing",
            2.0,
            {"foo": "bar"},
            [{"foo": 2}],
            [{"foo": "asc", "bar": "desc"}],
            [{"foo": "asc"}, {"bar": "desc"}],
            [""],
        ]
        for fields in bad_fields:
            try:
                self.db.create_index(fields)
            except Exception as e:
                self.assertEqual(e.response.status_code, 400)
            else:
                raise AssertionError("bad create index")

    def test_bad_types(self):
        bad_types = [
            None,
            True,
            False,
            1.5,
            "foo",  # Future support
            "geo",  # Future support
            {"foo": "bar"},
            ["baz", 3.0],
        ]
        for bt in bad_types:
            try:
                self.db.create_index(["foo"], idx_type=bt)
            except Exception as e:
                self.assertEqual(
                    e.response.status_code, 400, (bt, e.response.status_code)
                )
            else:
                raise AssertionError("bad create index")

    def test_bad_names(self):
        bad_names = [True, False, 1.5, {"foo": "bar"}, [None, False]]
        for bn in bad_names:
            try:
                self.db.create_index(["foo"], name=bn)
            except Exception as e:
                self.assertEqual(e.response.status_code, 400)
            else:
                raise AssertionError("bad create index")
            try:
                self.db.create_index(["foo"], ddoc=bn)
            except Exception as e:
                self.assertEqual(e.response.status_code, 400)
            else:
                raise AssertionError("bad create index")

    def test_create_idx_01(self):
        fields = ["foo", "bar"]
        ret = self.db.create_index(fields, name="idx_01")
        assert ret is True
        for idx in self.db.list_indexes():
            if idx["name"] != "idx_01":
                continue
            self.assertEqual(idx["def"]["fields"], [{"foo": "asc"}, {"bar": "asc"}])
            return
        raise AssertionError("index not created")

    def test_create_idx_01_exists(self):
        fields = ["foo", "bar"]
        ret = self.db.create_index(fields, name="idx_01")
        assert ret is True
        ret = self.db.create_index(fields, name="idx_01")
        assert ret is False

    def test_create_idx_02(self):
        fields = ["baz", "foo"]
        ret = self.db.create_index(fields, name="idx_02")
        assert ret is True
        for idx in self.db.list_indexes():
            if idx["name"] != "idx_02":
                continue
            self.assertEqual(idx["def"]["fields"], [{"baz": "asc"}, {"foo": "asc"}])
            return
        raise AssertionError("index not created")

    @unittest.skip("need spidermonkey 60")
    def test_ignore_design_docs(self):
        fields = ["baz", "foo"]
        ret = self.db.create_index(fields, name="idx_02")
        assert ret is True
        self.db.save_doc({
            "_id": "_design/ignore",
            "views": {
                "view1": {
                    "map": "function (doc) { emit(doc._id, 1)}"
                }
            }
        })
        Indexes = self.db.list_indexes()
        self.assertEqual(len(Indexes), 2)

    def test_read_idx_doc(self):
        self.db.create_index(["foo", "bar"], name="idx_01")
        self.db.create_index(["hello", "bar"])
        for idx in self.db.list_indexes():
            if idx["type"] == "special":
                continue
            ddocid = idx["ddoc"]
            doc = self.db.open_doc(ddocid)
            self.assertEqual(doc["_id"], ddocid)
            # info = self.db.ddoc_info(ddocid)
            # self.assertEqual(info["name"], ddocid.split("_design/")[-1])

    def test_delete_idx_escaped(self):
        self.db.create_index(["foo", "bar"], name="idx_01")
        pre_indexes = self.db.list_indexes()
        ret = self.db.create_index(["bing"], name="idx_del_1")
        assert ret is True
        for idx in self.db.list_indexes():
            if idx["name"] != "idx_del_1":
                continue
            self.assertEqual(idx["def"]["fields"], [{"bing": "asc"}])
            self.db.delete_index(idx["ddoc"].replace("/", "%2F"), idx["name"])
        post_indexes = self.db.list_indexes()
        self.assertEqual(pre_indexes, post_indexes)

    def test_delete_idx_unescaped(self):
        pre_indexes = self.db.list_indexes()
        ret = self.db.create_index(["bing"], name="idx_del_2")
        assert ret is True
        for idx in self.db.list_indexes():
            if idx["name"] != "idx_del_2":
                continue
            self.assertEqual(idx["def"]["fields"], [{"bing": "asc"}])
            self.db.delete_index(idx["ddoc"], idx["name"])
        post_indexes = self.db.list_indexes()
        self.assertEqual(pre_indexes, post_indexes)

    def test_delete_idx_no_design(self):
        pre_indexes = self.db.list_indexes()
        ret = self.db.create_index(["bing"], name="idx_del_3")
        assert ret is True
        for idx in self.db.list_indexes():
            if idx["name"] != "idx_del_3":
                continue
            self.assertEqual(idx["def"]["fields"], [{"bing": "asc"}])
            self.db.delete_index(idx["ddoc"].split("/")[-1], idx["name"])
        post_indexes = self.db.list_indexes()
        self.assertEqual(pre_indexes, post_indexes)

    def test_bulk_delete(self):
        fields = ["field1"]
        ret = self.db.create_index(fields, name="idx_01")
        assert ret is True

        fields = ["field2"]
        ret = self.db.create_index(fields, name="idx_02")
        assert ret is True

        fields = ["field3"]
        ret = self.db.create_index(fields, name="idx_03")
        assert ret is True

        docids = []

        for idx in self.db.list_indexes():
            if idx["ddoc"] is not None:
                docids.append(idx["ddoc"])

        docids.append("_design/this_is_not_an_index_name")

        ret = self.db.bulk_delete(docids)

        self.assertEqual(ret["fail"][0]["id"], "_design/this_is_not_an_index_name")
        self.assertEqual(len(ret["success"]), 3)

        for idx in self.db.list_indexes():
            assert idx["type"] != "json"
            assert idx["type"] != "text"

    def test_recreate_index(self):
        pre_indexes = self.db.list_indexes()
        for i in range(5):
            ret = self.db.create_index(["bing"], name="idx_recreate")
            assert ret is True
            for idx in self.db.list_indexes():
                if idx["name"] != "idx_recreate":
                    continue
                self.assertEqual(idx["def"]["fields"], [{"bing": "asc"}])
                self.db.delete_index(idx["ddoc"], idx["name"])
                break
            post_indexes = self.db.list_indexes()
            self.assertEqual(pre_indexes, post_indexes)

    def test_delete_missing(self):
        # Missing design doc
        try:
            self.db.delete_index("this_is_not_a_design_doc_id", "foo")
        except Exception as e:
            self.assertEqual(e.response.status_code, 404)
        else:
            raise AssertionError("bad index delete")

        # Missing view name
        ret = self.db.create_index(["fields"], name="idx_01")
        indexes = self.db.list_indexes()
        not_special = [idx for idx in indexes if idx["type"] != "special"]
        idx = random.choice(not_special)
        ddocid = idx["ddoc"].split("/")[-1]
        try:
            self.db.delete_index(ddocid, "this_is_not_an_index_name")
        except Exception as e:
            self.assertEqual(e.response.status_code, 404)
        else:
            raise AssertionError("bad index delete")

        # Bad view type
        try:
            self.db.delete_index(ddocid, idx["name"], idx_type="not_a_real_type")
        except Exception as e:
            self.assertEqual(e.response.status_code, 404)
        else:
            raise AssertionError("bad index delete")

    def test_limit_skip_index(self):
        fields = ["field1"]
        ret = self.db.create_index(fields, name="idx_01")
        assert ret is True

        fields = ["field2"]
        ret = self.db.create_index(fields, name="idx_02")
        assert ret is True

        fields = ["field3"]
        ret = self.db.create_index(fields, name="idx_03")
        assert ret is True

        fields = ["field4"]
        ret = self.db.create_index(fields, name="idx_04")
        assert ret is True

        fields = ["field5"]
        ret = self.db.create_index(fields, name="idx_05")
        assert ret is True

        self.assertEqual(len(self.db.list_indexes(limit=2)), 2)
        self.assertEqual(len(self.db.list_indexes(limit=5, skip=4)), 2)
        self.assertEqual(len(self.db.list_indexes(skip=5)), 1)
        self.assertEqual(len(self.db.list_indexes(skip=6)), 0)
        self.assertEqual(len(self.db.list_indexes(skip=100)), 0)
        self.assertEqual(len(self.db.list_indexes(limit=10000000)), 6)

        try:
            self.db.list_indexes(skip=-1)
        except Exception as e:
            self.assertEqual(e.response.status_code, 500)

        try:
            self.db.list_indexes(limit=0)
        except Exception as e:
            self.assertEqual(e.response.status_code, 500)

    def test_out_of_sync(self):
        self.db.save_docs(copy.deepcopy(DOCS))
        self.db.create_index(["age"], name="age")

        selector = {"age": {"$gt": 0}}
        docs = self.db.find(
            selector, use_index="_design/a017b603a47036005de93034ff689bbbb6a873c4"
        )
        self.assertEqual(len(docs), 2)

        self.db.delete_doc("1")

        docs1 = self.db.find(
            selector,
            update="False",
            use_index="_design/a017b603a47036005de93034ff689bbbb6a873c4",
        )
        self.assertEqual(len(docs1), 1)


@unittest.skipUnless(mango.has_text_service(), "requires text service")
class IndexCrudTextTests(mango.DbPerClass):
    def setUp(self):
        self.db.recreate()

    def test_create_text_idx(self):
        fields = [
            {"name": "stringidx", "type": "string"},
            {"name": "booleanidx", "type": "boolean"},
        ]
        ret = self.db.create_text_index(fields=fields, name="text_idx_01")
        assert ret is True
        for idx in self.db.list_indexes():
            if idx["name"] != "text_idx_01":
                continue
            self.assertEqual(
                idx["def"]["fields"],
                [{"stringidx": "string"}, {"booleanidx": "boolean"}],
            )
            return
        raise AssertionError("index not created")

    def test_create_bad_text_idx(self):
        bad_fields = [
            True,
            False,
            "bing",
            2.0,
            ["foo", "bar"],
            [{"name": "foo2"}],
            [{"name": "foo3", "type": "garbage"}],
            [{"type": "number"}],
            [{"name": "age", "type": "number"}, {"name": "bad"}],
            [{"name": "age", "type": "number"}, "bla"],
            [{"name": "", "type": "number"}, "bla"],
        ]
        for fields in bad_fields:
            try:
                self.db.create_text_index(fields=fields)
            except Exception as e:
                self.assertEqual(e.response.status_code, 400)
            else:
                raise AssertionError("bad create text index")

    def test_limit_skip_index(self):
        fields = ["field1"]
        ret = self.db.create_index(fields, name="idx_01")
        assert ret is True

        fields = ["field2"]
        ret = self.db.create_index(fields, name="idx_02")
        assert ret is True

        fields = ["field3"]
        ret = self.db.create_index(fields, name="idx_03")
        assert ret is True

        fields = ["field4"]
        ret = self.db.create_index(fields, name="idx_04")
        assert ret is True

        fields = [
            {"name": "stringidx", "type": "string"},
            {"name": "booleanidx", "type": "boolean"},
        ]
        ret = self.db.create_text_index(fields=fields, name="idx_05")
        assert ret is True

        self.assertEqual(len(self.db.list_indexes(limit=2)), 2)
        self.assertEqual(len(self.db.list_indexes(limit=5, skip=4)), 2)
        self.assertEqual(len(self.db.list_indexes(skip=5)), 1)
        self.assertEqual(len(self.db.list_indexes(skip=6)), 0)
        self.assertEqual(len(self.db.list_indexes(skip=100)), 0)
        self.assertEqual(len(self.db.list_indexes(limit=10000000)), 6)

        try:
            self.db.list_indexes(skip=-1)
        except Exception as e:
            self.assertEqual(e.response.status_code, 500)

        try:
            self.db.list_indexes(limit=0)
        except Exception as e:
            self.assertEqual(e.response.status_code, 500)
