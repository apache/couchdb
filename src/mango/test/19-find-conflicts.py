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

DOC = [{"_id": "doc", "a": 2}]

CONFLICT = [{"_id": "doc", "_rev": "1-23202479633c2b380f79507a776743d5", "a": 1}]

@unittest.skip("re-enable once conflicts are supported")
class ChooseCorrectIndexForDocs(mango.DbPerClass):
    def setUp(self):
        self.db.recreate()
        self.db.save_docs(copy.deepcopy(DOC))
        self.db.save_docs_with_conflicts(copy.deepcopy(CONFLICT))

    def test_retrieve_conflicts(self):
        self.db.create_index(["_conflicts"])
        result = self.db.find({"_conflicts": {"$exists": True}}, conflicts=True)
        self.assertEqual(
            result[0]["_conflicts"][0], "1-23202479633c2b380f79507a776743d5"
        )
        self.assertEqual(result[0]["_rev"], "1-3975759ccff3842adf690a5c10caee42")
