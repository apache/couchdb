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


class NoDefaultFieldTest(mango.UserDocsTextTests):

    DEFAULT_FIELD = False

    @classmethod
    def setUpClass(klass):
        raise unittest.SkipTest('text index service not available')

    def test_basic(self):
        docs = self.db.find({"$text": "Ramona"})
        # Or should this throw an error?
        assert len(docs) == 0

    def test_other_fields_exist(self):
        docs = self.db.find({"age": 22})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 9


class NoDefaultFieldWithAnalyzer(mango.UserDocsTextTests):

    DEFAULT_FIELD = {
        "enabled": False,
        "analyzer": "keyword"
    }

    @classmethod
    def setUpClass(klass):
        raise unittest.SkipTest('text not supported')

    def test_basic(self):
        docs = self.db.find({"$text": "Ramona"})
        assert len(docs) == 0

    def test_other_fields_exist(self):
        docs = self.db.find({"age": 22})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 9


class DefaultFieldWithCustomAnalyzer(mango.UserDocsTextTests):

    DEFAULT_FIELD = {
        "enabled": True,
        "analyzer": "keyword"
    }

    @classmethod
    def setUpClass(klass):
        raise unittest.SkipTest('text not supported')

    def test_basic(self):
        docs = self.db.find({"$text": "Ramona"})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 9

    def test_not_analyzed(self):
        docs = self.db.find({"$text": "Lott Place"})
        assert len(docs) == 1
        assert docs[0]["user_id"] == 9

        docs = self.db.find({"$text": "Lott"})
        assert len(docs) == 0

        docs = self.db.find({"$text": "Place"})
        assert len(docs) == 0
