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


def create_index(db, ddoc_id, view_id, definition):
    db.save_docs(
        [
            {
                "_id": ddoc_id,
                "language": "query",
                "views": {
                    view_id: {
                        "map": {"fields": definition, "partial_filter_selector": {}},
                        "reduce": "_count",
                        "options": {"def": {"fields": definition}},
                    }
                },
            }
        ]
    )


class IndexFunctionTests(mango.DbPerClass):
    @classmethod
    def setUpClass(klass):
        super(IndexFunctionTests, klass).setUpClass()

        create_index(
            klass.db,
            "_design/jq-split",
            "jq-split-json-index",
            {"bar_words": {"$explode": {"$field": "bar", "$separator": " "}}},
        )
        klass.db.save_docs([{"_id": "example-doc", "bar": "a b c"}])

    def test_index_by_length(self):
        resp = self.db.find({"bar_words": "b"})
        self.assertEqual([doc["_id"] for doc in resp], ["example-doc"])
