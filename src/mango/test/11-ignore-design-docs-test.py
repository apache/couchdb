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
import unittest

DOCS = [
    {"_id": "_design/my-design-doc"},
    {"_id": "54af50626de419f5109c962f", "user_id": 0, "age": 10, "name": "Jimi"},
    {"_id": "54af50622071121b25402dc3", "user_id": 1, "age": 11, "name": "Eddie"}
]


class IgnoreDesignDocsForAllDocsIndexTests(mango.DbPerClass):
    def test_should_not_return_design_docs(self):
        self.db.save_docs(DOCS)
        docs = self.db.find({"_id": {"$gte": None}})
        assert len(docs) == 2
