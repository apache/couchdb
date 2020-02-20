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


import mango, requests, unittest


# @unittest.skip("this FDB doesn't support this")
# class UsersDbFindTests(mango.UsersDbTests):
#     def test_simple_find(self):
#         docs = self.db.find({"name": {"$eq": "demo02"}})
#         assert len(docs) == 1
#         assert docs[0]["_id"] == "org.couchdb.user:demo02"
#
#     def test_multi_cond_and(self):
#         self.db.create_index(["type", "roles"])
#         docs = self.db.find({"type": "user", "roles": {"$eq": ["reader"]}})
#         assert len(docs) == 1
#         assert docs[0]["_id"] == "org.couchdb.user:demo02"
#
#     def test_multi_cond_or(self):
#         docs = self.db.find(
#             {"$and": [{"type": "user"}, {"$or": [{"order": 1}, {"order": 3}]}]}
#         )
#         assert len(docs) == 2
#         assert docs[0]["_id"] == "org.couchdb.user:demo01"
#         assert docs[1]["_id"] == "org.couchdb.user:demo03"
#
#     def test_sort(self):
#         self.db.create_index(["order", "name"])
#         selector = {"name": {"$gt": "demo01"}}
#         docs1 = self.db.find(selector, sort=[{"order": "asc"}])
#         docs2 = list(sorted(docs1, key=lambda d: d["order"]))
#         assert docs1 is not docs2 and docs1 == docs2
#
#         docs1 = self.db.find(selector, sort=[{"order": "desc"}])
#         docs2 = list(reversed(sorted(docs1, key=lambda d: d["order"])))
#         assert docs1 is not docs2 and docs1 == docs2
#
#     def test_fields(self):
#         selector = {"name": {"$eq": "demo02"}}
#         docs = self.db.find(selector, fields=["name", "order"])
#         assert len(docs) == 1
#         assert sorted(docs[0].keys()) == ["name", "order"]
#
#     def test_empty(self):
#         docs = self.db.find({})
#         assert len(docs) == 3
#
#
# @unittest.skip("this FDB doesn't support this")
# class UsersDbIndexFindTests(UsersDbFindTests):
#     def setUp(self):
#         self.db.create_index(["name"])
#
#     def test_multi_cond_and(self):
#         self.db.create_index(["type", "roles"])
#         super(UsersDbIndexFindTests, self).test_multi_cond_and()
#
#     def test_multi_cond_or(self):
#         self.db.create_index(["type", "order"])
#         super(UsersDbIndexFindTests, self).test_multi_cond_or()
#
#     def test_sort(self):
#         self.db.create_index(["order", "name"])
#         super(UsersDbIndexFindTests, self).test_sort()
