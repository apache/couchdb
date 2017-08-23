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

class ExecutionStatsTests(mango.UserDocsTests):

    def test_simple_json_index(self):
        resp = self.db.find({"age": {"$lt": 35}}, return_raw=True, executionStats=True)
        assert len(resp["docs"]) == 3
        assert resp["execution_stats"]["total_keys_examined"] == 0
        assert resp["execution_stats"]["total_docs_examined"] == 3
        assert resp["execution_stats"]["total_quorum_docs_examined"] == 0
        assert resp["execution_stats"]["results_returned"] == 3
        assert resp["execution_stats"]["execution_time_ms"] > 0

    def test_no_execution_stats(self):
        resp = self.db.find({"age": {"$lt": 35}}, return_raw=True, executionStats=False)
        assert "execution_stats" not in resp

    def test_quorum_json_index(self):
        resp = self.db.find({"age": {"$lt": 35}}, return_raw=True, r=3, executionStats=True)
        assert len(resp["docs"]) == 3
        assert resp["execution_stats"]["total_keys_examined"] == 0
        assert resp["execution_stats"]["total_docs_examined"] == 0
        assert resp["execution_stats"]["total_quorum_docs_examined"] == 3
        assert resp["execution_stats"]["results_returned"] == 3
        assert resp["execution_stats"]["execution_time_ms"] > 0

@unittest.skipUnless(mango.has_text_service(), "requires text service")
class ExecutionStatsTests_Text(mango.UserDocsTextTests):

    def test_simple_text_index(self):
        resp = self.db.find({"$text": "Stephanie"}, 
                return_raw=True, 
                executionStats=True)
        assert len(resp["docs"]) == 1
        assert resp["execution_stats"]["total_keys_examined"] == 0
        assert resp["execution_stats"]["total_docs_examined"] == 1
        assert resp["execution_stats"]["total_quorum_docs_examined"] == 0
        assert resp["execution_stats"]["results_returned"] == 1
        assert resp["execution_stats"]["execution_time_ms"] > 0

    def test_no_execution_stats(self):
        resp = self.db.find({"$text": "Stephanie"}, 
                return_raw=True)
        assert "execution_stats" not in resp
