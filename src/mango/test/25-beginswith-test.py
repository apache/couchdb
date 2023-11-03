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

import copy
import mango

DOCS = [
    {"_id": "aaa", "name": "Jimi", "location": "AUS", "age": 27},
    {"_id": "abc", "name": "Eddie", "location": "AND", "age": 65},
    {"_id": "bbb", "name": "Harry", "location": "CAN", "age": 21},
    {"_id": "ccc", "name": "Eddie", "location": "DEN", "age": 37},
    {"_id": "ddd", "name": "Jones", "location": "ETH", "age": 49},
]


def to_utf8_bytes(list):
    return [x.encode() for x in list]


class BeginsWithOperator(mango.DbPerClass):
    def setUp(self):
        self.db.recreate()
        self.db.save_docs(copy.deepcopy(DOCS))
        self.db.create_index(["location"])
        self.db.create_index(["name", "location"])

    def get_mrargs(self, selector, sort=None):
        explain = self.db.find(selector, sort=sort, explain=True)
        return explain["mrargs"]

    def assertDocIds(self, user_ids, docs):
        user_ids_returned = list(d["_id"] for d in docs)
        user_ids.sort()
        user_ids_returned.sort()
        self.assertEqual(user_ids, user_ids_returned)

    def test_basic(self):
        docs = self.db.find({"location": {"$beginsWith": "A"}})

        self.assertEqual(len(docs), 2)
        self.assertDocIds(["aaa", "abc"], docs)

    def test_json_range(self):
        mrargs = self.get_mrargs({"location": {"$beginsWith": "A"}})

        self.assertEqual(mrargs["start_key"], ["A"])
        end_key_bytes = to_utf8_bytes(mrargs["end_key"])
        self.assertEqual(end_key_bytes, [b"A\xef\xbf\xbf", b"<MAX>"])

    def test_compound_key(self):
        selector = {"name": "Eddie", "location": {"$beginsWith": "A"}}
        mrargs = self.get_mrargs(selector)

        self.assertEqual(mrargs["start_key"], ["Eddie", "A"])
        end_key_bytes = to_utf8_bytes(mrargs["end_key"])
        self.assertEqual(end_key_bytes, [b"Eddie", b"A\xef\xbf\xbf", b"<MAX>"])

        docs = self.db.find(selector)
        self.assertEqual(len(docs), 1)
        self.assertDocIds(["abc"], docs)

    def test_sort(self):
        selector = {"location": {"$beginsWith": "A"}}
        cases = [
            {
                "sort": ["location"],
                "start_key": [b"A"],
                "end_key": [b"A\xef\xbf\xbf", b"<MAX>"],
                "direction": "fwd",
            },
            {
                "sort": [{"location": "desc"}],
                "start_key": [b"A\xef\xbf\xbf", b"<MAX>"],
                "end_key": [b"A"],
                "direction": "rev",
            },
        ]

        for case in cases:
            with self.subTest(sort=case["sort"]):
                mrargs = self.get_mrargs(selector, sort=case["sort"])
                self.assertEqual(to_utf8_bytes(mrargs["start_key"]), case["start_key"])
                self.assertEqual(to_utf8_bytes(mrargs["end_key"]), case["end_key"])
                self.assertEqual(mrargs["direction"], case["direction"])

    def test_all_docs_range(self):
        mrargs = self.get_mrargs({"_id": {"$beginsWith": "a"}})

        self.assertEqual(mrargs["start_key"], "a")
        end_key_bytes = to_utf8_bytes(mrargs["end_key"])
        self.assertEqual(end_key_bytes, [b"a", b"\xef\xbf\xbf"])

    def test_no_index(self):
        selector = {"foo": {"$beginsWith": "a"}}
        resp_explain = self.db.find(selector, explain=True)
        mrargs = resp_explain["mrargs"]

        self.assertEqual(resp_explain["index"]["type"], "special")
        self.assertEqual(mrargs["start_key"], None)
        self.assertEqual(mrargs["end_key"], "<MAX>")

    def test_invalid_operand(self):
        try:
            self.db.find({"_id": {"$beginsWith": True}})
        except Exception as e:
            self.assertEqual(e.response.status_code, 400)
            resp = e.response.json()
            self.assertEqual(resp["error"], "invalid_operator")
        else:
            raise AssertionError("expected find error")

    def test_does_not_match_non_string_value(self):
        docs = self.db.find({"age": {"$beginsWith": "a"}})
        self.assertEqual(len(docs), 0)

    def test_no_matches(self):
        docs = self.db.find({"name": {"$beginsWith": "Z"}})
        self.assertEqual(len(docs), 0)

    def test_case_sensitivity(self):
        docs = self.db.find({"name": {"$beginsWith": "j"}})
        self.assertEqual(len(docs), 0)

        docs = self.db.find({"name": {"$beginsWith": "J"}})
        self.assertEqual(len(docs), 2)
