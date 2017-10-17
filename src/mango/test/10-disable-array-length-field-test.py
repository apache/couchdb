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

@unittest.skipUnless(mango.has_text_service(), "requires text service")
class DisableIndexArrayLengthsTest(mango.UserDocsTextTests):

    def setUp(klass):
        self.db.recreate()
        self.db.create_text_index(ddoc="disable_index_array_lengths",
                                       analyzer="keyword",
                                       index_array_lengths=False)
        self.db.create_text_index(ddoc="explicit_enable_index_array_lengths",
                                       analyzer="keyword",
                                       index_array_lengths=True)

    def test_disable_index_array_length(self):
        docs = self.db.find({"favorites": {"$size": 4}},
                            use_index="disable_index_array_lengths")
        for d in docs:
            assert len(d["favorites"]) == 0

    def test_enable_index_array_length(self):
        docs = self.db.find({"favorites": {"$size": 4}},
                            use_index="explicit_enable_index_array_lengths")
        for d in docs:
            assert len(d["favorites"]) == 4
