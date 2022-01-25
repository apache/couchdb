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


class IndexFunctionTests(mango.DbPerClass):
    @classmethod
    def setUpClass(klass):
        super(IndexFunctionTests, klass).setUpClass()

        klass.db.create_index(
            [
                {"f1_words": {"$jq": '.f1 | split(" ") | .[]'}},
            ]
        )

        klass.db.create_index(
            [
                {"f2_words": {"$jq": '.f2 | split(" ") | .[]'}},
                {"f3_words": {"$jq": '.f3 | split(" ") | .[]'}},
            ],
        )

        klass.db.save_docs(
            [
                {"_id": "doc-1", "f1": "a b", "f2": "j k", "f3": "y z"},
                {"_id": "doc-2", "f1": "b c", "f2": "k l", "f3": "x y"},
            ]
        )

    def test_search_on_one_field(self):
        resp = self.db.find({"f1_words": "a"})
        self.assertEqual([doc["_id"] for doc in resp], ["doc-1"])

        resp = self.db.find({"f1_words": "b"})
        self.assertEqual([doc["_id"] for doc in resp], ["doc-1", "doc-2"])

    def test_search_on_two_fields(self):
        resp = self.db.find({"f2_words": "k", "f3_words": "x"})
        self.assertEqual([doc["_id"] for doc in resp], ["doc-2"])
