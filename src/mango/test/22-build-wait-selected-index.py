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


class BuildAndWaitOnSelectedIndex(mango.DbPerClass):
    def setUp(self):
        self.db.recreate()
        docs = []
        for i in range(1000):
            docs.append({"_id": str(i), "val": i})
            if len(docs) == 250:
                self.db.save_docs(docs)
                docs = []

    def test_wait_for_query(self):
        self.db.create_index(["val"], ddoc="my-ddoc", wait_for_built_index=False)

        explain = self.db.find({"val": {"$gt": 990}}, use_index="my-ddoc", explain=True)
        self.assertEqual(explain["index"]["ddoc"], "_design/my-ddoc")

        docs = self.db.find({"val": {"$gte": 990}}, limit=10)

        self.assertEqual(len(docs), 10)

    def test_dont_wait(self):
        self.db.create_index(["val"], ddoc="my-ddoc", wait_for_built_index=False)

        explain = self.db.find({"val": {"$gt": 990}}, explain=True)
        self.assertEqual(explain["index"]["name"], "_all_docs")

        docs = self.db.find({"val": {"$gte": 990}})
        self.assertEqual(len(docs), 10)

    def test_update_false(self):
        self.db.create_index(["val"], ddoc="my-ddoc", wait_for_built_index=False)
        docs = self.db.find({"val": {"$gte": 990}}, update=False, use_index="my-ddoc")
        self.assertEqual(docs, [])
