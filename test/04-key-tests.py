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


TEST_DOCS = [
    {
        "type": "complex_key",
        "title": "normal key"
    },
    {
        "type": "complex_key",
        "title": "key with dot",
        "dot.key": "dot's value",
        "none": {
            "dot": "none dot's value"
        },
        "name.first" : "Kvothe"
    },
    {
        "type": "complex_key",
        "title": "key with peso",
        "$key": "peso",
        "deep": {
            "$key": "deep peso"
        },
        "name": {"first" : "Master Elodin"}
    },
    {
        "type": "complex_key",
        "title": "unicode key",
        "": "apple"
    },
    {
        "title": "internal_fields_format",
        "utf8-1[]:string" : "string",
        "utf8-2[]:boolean[]" : True,
        "utf8-3[]:number" : 9,
        "utf8-3[]:null" : None
    }
]


class KeyTests(mango.DbPerClass):
    @classmethod
    def setUpClass(klass):
        super(KeyTests, klass).setUpClass()
        klass.db.save_docs(TEST_DOCS, w=3)
        klass.db.create_index(["type"], ddoc="view")
        klass.db.create_text_index(ddoc="text")

    def run_check(self, query, check, fields=None, indexes=None):
        if indexes is None:
            indexes = ["view", "text"]
        for idx in indexes:
            docs = self.db.find(query, fields=fields, use_index=idx)
            check(docs)

    def test_dot_key(self):
        query = {"type": "complex_key"}
        fields = ["title", "dot\\.key", "none.dot"]
        def check(docs):
            assert len(docs) == 4
            assert docs[1].has_key("dot.key")
            assert docs[1]["dot.key"] == "dot's value"
            assert docs[1].has_key("none")
            assert docs[1]["none"]["dot"] == "none dot's value"
        self.run_check(query, check, fields=fields)

    def test_peso_key(self):
        query = {"type": "complex_key"}
        fields = ["title", "$key", "deep.$key"]
        def check(docs):
            assert len(docs) == 4
            assert docs[2].has_key("$key")
            assert docs[2]["$key"] == "peso"
            assert docs[2].has_key("deep")
            assert docs[2]["deep"]["$key"] == "deep peso"
        self.run_check(query, check, fields=fields)

    def test_unicode_in_fieldname(self):
        query = {"type": "complex_key"}
        fields = ["title", ""]
        def check(docs):
            assert len(docs) == 4
            # note:  == \uf8ff
            assert docs[3].has_key(u'\uf8ff')
            assert docs[3][u'\uf8ff'] == "apple"
        self.run_check(query, check, fields=fields)

    # The rest of these tests are only run against the text
    # indexes because view indexes don't have to worry about
    # field *name* escaping in the index.

    def test_unicode_in_selector_field(self):
        query = {"" : "apple"}
        def check(docs):
            assert len(docs) == 1
            assert docs[0][u"\uf8ff"] == "apple"
        self.run_check(query, check, indexes=["text"])

    def test_internal_field_tests(self):
        queries = [
            {"utf8-1[]:string" : "string"},
            {"utf8-2[]:boolean[]" : True},
            {"utf8-3[]:number" : 9},
            {"utf8-3[]:null" : None}
        ]
        def check(docs):
            assert len(docs) == 1
            assert docs[0]["title"] == "internal_fields_format"
        for query in queries:
            self.run_check(query, check, indexes=["text"])

    def test_escape_period(self):
        query = {"name\\.first" : "Kvothe"}
        def check(docs):
            assert len(docs) == 1
            assert docs[0]["name.first"] == "Kvothe"
        self.run_check(query, check, indexes=["text"])

        query = {"name.first" : "Kvothe"}
        def check_empty(docs):
            assert len(docs) == 0
        self.run_check(query, check_empty, indexes=["text"])

    def test_object_period(self):
        query = {"name.first" : "Master Elodin"}
        def check(docs):
            assert len(docs) == 1
            assert docs[0]["title"] == "key with peso"
        self.run_check(query, check, indexes=["text"])

        query = {"name\\.first" : "Master Elodin"}
        def check_empty(docs):
            assert len(docs) == 0
        self.run_check(query, check_empty, indexes=["text"])
