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
import copy

DOCS = [
    {"_id": "100", "name": "Jimi", "location": "AUS", "user_id": 1, "same": "value"},
    {"_id": "200", "name": "Eddie", "location": "BRA", "user_id": 2, "same": "value"},
    {"_id": "300", "name": "Harry", "location": "CAN", "user_id": 3, "same": "value"},
    {"_id": "400", "name": "Eddie", "location": "DEN", "user_id": 4, "same": "value"},
    {"_id": "500", "name": "Jones", "location": "ETH", "user_id": 5, "same": "value"}
]

class FdbIndexingTests(mango.DbPerClass):
    def setUp(self):
        self.db.recreate()
        self.db.create_index(["name"], name="name")
        self.db.save_docs(copy.deepcopy(DOCS))

    def test_doc_update(self):
        docs = self.db.find({"name": "Eddie"})
        self.assertEqual(len(docs), 2)
        self.assertEqual(docs[0]["_id"], "200")
        self.assertEqual(docs[1]["_id"], "400")

        doc = self.db.open_doc("400")
        doc["name"] = "NotEddie"
        self.db.save_doc(doc)

        docs = self.db.find({"name": "Eddie"})
        print("DD")
        print(docs)
        self.assertEqual(len(docs), 1)
        self.assertEqual(docs[0]["_id"], "200")



