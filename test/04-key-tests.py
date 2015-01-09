# -*- coding: utf-8 -*-

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
        }
    },
    {
        "type": "complex_key",
        "title": "key with peso",
        "$key": "peso",
        "deep": {
            "$key": "deep peso"
        }
    },
    {
        "type": "complex_key",
        "title": "unicode key",
        "": "apple"
    }
]


class KeyTests(mango.DbPerClass):
    @classmethod
    def setUpClass(klass):
        super(KeyTests, klass).setUpClass()
        klass.db.save_docs(TEST_DOCS, w=3)
        klass.db.create_index(["type"])

    def test_dot_key(self):
        fields = ["title", "dot\\.key", "none.dot"]
        docs = self.db.find({"type": "complex_key"}, fields = fields)
        assert len(docs) == 4
        assert docs[1].has_key("dot.key")
        assert docs[1]["dot.key"] == "dot's value"
        assert docs[1].has_key("none")
        assert docs[1]["none"]["dot"] == "none dot's value"

    def test_peso_key(self):
        fields = ["title", "$key", "deep.$key"]
        docs = self.db.find({"type": "complex_key"}, fields = fields)
        assert len(docs) == 4
        assert docs[2].has_key("$key")
        assert docs[2]["$key"] == "peso"
        assert docs[2].has_key("deep")
        assert docs[2]["deep"]["$key"] == "deep peso"

    def test_unicode_key(self):
        docs = self.db.find({"type": "complex_key"}, fields = ["title", ""])
        assert len(docs) == 4
        # note:  == \uf8ff
        assert docs[3].has_key(u'\uf8ff')
        assert docs[3][u'\uf8ff'] == "apple"
