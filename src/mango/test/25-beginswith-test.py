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
        explain = self.db.find({"location": {"$beginsWith": "A"}}, explain=True)
        self.assertEqual(explain["mrargs"]["start_key"], ["A"])
        end_key_bytes = to_utf8_bytes(explain["mrargs"]["end_key"])
        self.assertEqual(end_key_bytes, [b"A\xef\xbf\xbd", b"<MAX>"])

    def test_compound_key(self):
        selector = {"name": "Eddie", "location": {"$beginsWith": "A"}}
        explain = self.db.find(selector, explain=True)

        self.assertEqual(explain["mrargs"]["start_key"], ["Eddie", "A"])
        end_key_bytes = to_utf8_bytes(explain["mrargs"]["end_key"])
        self.assertEqual(end_key_bytes, [b"Eddie", b"A\xef\xbf\xbd", b"<MAX>"])

        docs = self.db.find(selector)
        self.assertEqual(len(docs), 1)
        self.assertDocIds(["abc"], docs)

    def test_sort_asc(self):
        selector = {"location": {"$beginsWith": "A"}}
        explain = self.db.find(selector, sort=["location"], explain=True)

        self.assertEqual(explain["mrargs"]["start_key"], ["A"])
        end_key_bytes = to_utf8_bytes(explain["mrargs"]["end_key"])
        self.assertEqual(end_key_bytes, [b"A\xef\xbf\xbd", b"<MAX>"])
        self.assertEqual(explain["mrargs"]["direction"], "fwd")

    def test_sort_desc(self):
        selector = {"location": {"$beginsWith": "A"}}
        explain = self.db.find(selector, sort=[{"location": "desc"}], explain=True)

        start_key_bytes = to_utf8_bytes(explain["mrargs"]["end_key"])
        self.assertEqual(start_key_bytes, [b"A"])
        self.assertEqual(explain["mrargs"]["end_key"], ["A"])
        self.assertEqual(explain["mrargs"]["direction"], "rev")

    def test_all_docs_range(self):
        explain = self.db.find({"_id": {"$beginsWith": "a"}}, explain=True)
        self.assertEqual(explain["mrargs"]["start_key"], "a")
        end_key_bytes = to_utf8_bytes(explain["mrargs"]["end_key"])
        self.assertEqual(end_key_bytes, [b"a", b"\xef\xbf\xbd"])

    def test_no_index(self):
        selector = {"foo": {"$beginsWith": "a"}}
        resp_explain = self.db.find(selector, explain=True)

        self.assertEqual(resp_explain["index"]["type"], "special")
        self.assertEqual(resp_explain["mrargs"]["start_key"], None)
        self.assertEqual(resp_explain["mrargs"]["end_key"], "<MAX>")

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
        selector = {"age": {"$beginsWith": "a"}}
        docs = self.db.find(selector)

        self.assertEqual(len(docs), 0)
