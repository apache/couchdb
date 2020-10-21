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


class LongRunningMangoTest(mango.DbPerClass):
    def setUp(self):
        self.db.recreate()
        docs = []
        for i in range(100000):
            docs.append({"_id": str(i), "another": "field"})
            if i % 20000 == 0:
                self.db.save_docs(docs)
                docs = []

    # This test should run to completion and not timeout
    def test_query_does_not_time_out(self):
        selector = {"_id": {"$gt": 0}, "another": "wrong"}
        docs = self.db.find(selector)
        self.assertEqual(len(docs), 0)
