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
import unittest

DOCS = [
    {"_id": "aa", "name": "Jimi", "age": 10, "cars": 1},
    {"_id": "2", "name": "Eddie", "age": 20, "cars": 1},
    {"_id": "3", "name": "Jane", "age": 30, "cars": 2},
    {"_id": "4", "name": "Mary", "age": 40, "cars": 2},
    {"_id": "5", "name": "Sam", "age": 50, "cars": 3},
]


class JSONIndexSortOptimisations(mango.DbPerClass):
    def setUp(self):
        self.db.recreate()
        self.db.save_docs(copy.deepcopy(DOCS))

    def test_works_for_basic_case(self):
        self.db.create_index(["cars", "age"], name="cars-age")
        selector = {"cars": "2", "age": {"$gt": 10}}
        explain = self.db.find(selector, sort=["age"], explain=True)
        self.assertEqual(explain["index"]["name"], "cars-age")
        self.assertEqual(explain["args"]["direction"], "fwd")

    def test_works_for_all_fields_specified(self):
        self.db.create_index(["cars", "age"], name="cars-age")
        selector = {"cars": "2", "age": {"$gt": 10}}
        explain = self.db.find(selector, sort=["cars", "age"], explain=True)
        self.assertEqual(explain["index"]["name"], "cars-age")

    def test_works_for_no_sort_fields_specified(self):
        self.db.create_index(["cars", "age"], name="cars-age")
        selector = {"cars": {"$gt": 10}, "age": {"$gt": 10}}
        explain = self.db.find(selector, explain=True)
        self.assertEqual(explain["index"]["name"], "cars-age")

    def test_works_for_opp_dir_sort(self):
        self.db.create_index(["cars", "age"], name="cars-age")
        selector = {"cars": "2", "age": {"$gt": 10}}
        explain = self.db.find(selector, sort=[{"age": "desc"}], explain=True)
        self.assertEqual(explain["index"]["name"], "cars-age")
        self.assertEqual(explain["args"]["direction"], "rev")

    def test_not_work_for_non_constant_field(self):
        self.db.create_index(["cars", "age"], name="cars-age")
        selector = {"cars": {"$gt": 10}, "age": {"$gt": 10}}
        try:
            self.db.find(selector, explain=True, sort=["age"])
            raise Exception("Should not get here")
        except Exception as e:
            resp = e.response.json()
            self.assertEqual(resp["error"], "no_usable_index")

    def test_three_index_one(self):
        self.db.create_index(["cars", "age", "name"], name="cars-age-name")
        selector = {"cars": "2", "age": 10, "name": {"$gt": "AA"}}
        explain = self.db.find(selector, sort=["name"], explain=True)
        self.assertEqual(explain["index"]["name"], "cars-age-name")

    def test_three_index_two(self):
        self.db.create_index(["cars", "age", "name"], name="cars-age-name")
        selector = {"cars": "2", "name": "Eddie", "age": {"$gt": 10}}
        explain = self.db.find(selector, sort=["age"], explain=True)
        self.assertEqual(explain["index"]["name"], "cars-age-name")

    def test_three_index_fails(self):
        self.db.create_index(["cars", "age", "name"], name="cars-age-name")
        selector = {"name": "Eddie", "age": {"$gt": 1}, "cars": {"$gt": "1"}}
        try:
            self.db.find(selector, explain=True, sort=["name"])
            raise Exception("Should not get here")
        except Exception as e:
            resp = e.response.json()
            self.assertEqual(resp["error"], "no_usable_index")

    def test_empty_sort(self):
        self.db.create_index(["cars", "age", "name"], name="cars-age-name")
        selector = {"name": {"$gt": "Eddie"}, "age": 10, "cars": {"$gt": "1"}}
        explain = self.db.find(selector, explain=True)
        self.assertEqual(explain["index"]["name"], "cars-age-name")

    def test_in_between(self):
        self.db.create_index(["cars", "age", "name"], name="cars-age-name")
        selector = {"name": "Eddie", "age": 10, "cars": {"$gt": "1"}}
        explain = self.db.find(selector, explain=True)
        self.assertEqual(explain["index"]["name"], "cars-age-name")

        try:
            self.db.find(selector, sort=["cars", "name"], explain=True)
            raise Exception("Should not get here")
        except Exception as e:
            resp = e.response.json()
            self.assertEqual(resp["error"], "no_usable_index")

    def test_ignore_after_set_sort_value(self):
        self.db.create_index(["cars", "age", "name"], name="cars-age-name")
        selector = {"age": {"$gt": 10}, "cars": 2, "name": {"$gt": "A"}}
        explain = self.db.find(selector, sort=["age"], explain=True)
        self.assertEqual(explain["index"]["name"], "cars-age-name")

    def test_not_use_index_if_other_fields_in_sort(self):
        self.db.create_index(["cars", "age"], name="cars-age")
        selector = {"age": 10, "cars": {"$gt": "1"}}
        try:
            self.db.find(selector, sort=["cars", "name"], explain=True)
            raise Exception("Should not get here")
        except Exception as e:
            resp = e.response.json()
            self.assertEqual(resp["error"], "no_usable_index")
