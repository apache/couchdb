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

import copy
import mango
import unittest
import json

BOOLEN_ADB_DOC = {
    "_id": "hoary_marmot",
    "wiki_page": "http://en.wikipedia.org/wiki/Hoary_marmot",
    "min_weight": 4,
    "max_weight": 40,
    "min_length": 0.2,
    "max_length": 1,
    "latin_name": "Marmota caligata",
    "class": "mammal",
    "diet": "herbivore",
    "inherit_the_earth": True,
    "likes": ["to take tea", "kittens", "trees"],
    "habitat": [{"mountainous_environments": 8200, "states": ["Alaska", "Canada"]}],
}

BOOLEN_ADB_DOC2 = {
    "_id": "hoary_marmots",
    "wiki_page": "http://en.wikipedia.org/wiki/Hoary_marmot",
    "min_weight": 3,
    "max_weight": 30,
    "min_length": 0.2,
    "max_length": 1,
    "latin_name": "Marmota caligatas",
    "class": "mammal",
    "diet": "carnivore",
    "inherit_the_earth": True,
    "likes": ["to take tea", "kittens", "trees", "games"],
    "habitat": [{"mountainous_environments": 8200, "states": ["Alaska", "Canada"]}],
}

KOOKABURRA = {
    "_id": "kookaburra",
    "min_length": 0.28,
    "max_length": 0.42,
    "wiki_page": "http://en.wikipedia.org/wiki/Kookaburra",
    "class": "bird",
    "diet": "carnivore",
    "latin_name": "Dacelo novaeguineae",
}

DDOC = {
    "_id": "_design/views101",
    "indexes": {
        "animals": {
            "index": """
                function(doc){
                  index("default", doc._id);
                  if(doc.min_length){
                    index("min_length", doc.min_length, {"store": "yes"});
                  }
                  if (doc['class']){
                    index("class", doc['class'], {"store": "yes"});
                  }
                }
            """
        }
    },
}

DOCS = [BOOLEN_ADB_DOC, BOOLEN_ADB_DOC2, KOOKABURRA, DDOC]


@unittest.skipUnless(mango.has_text_service(), "requires text service")
class TestQuery(mango.DbPerClass):
    def setUp(self):
        self.db.recreate()
        self.db.save_docs(copy.deepcopy(DOCS))

    def search(self, view, idx, params, explain=False, return_raw=False, fail=False):
        path = self.db.url + "/_design/" + view + "/_search/" + idx
        r = self.db.sess.get(path, params=params)
        if not fail:
            r.raise_for_status()
        if explain or return_raw or fail:
            return r.json()
        else:
            print(r.json())
            return r.json()["docs"]

    def test_post_selector_arrays_element_text(self):
        self.db.create_text_index()
        selector = {"class": "mammal", "likes.0": "to take tea"}
        docs = self.db.find(selector, fields=["_id", "class"])
        self.assertEqual(len(docs), 2)
        self.assertEqual(docs[0]["_id"], "hoary_marmot")
        self.assertEqual(docs[1]["_id"], "hoary_marmots")

    def test_operators_all_text(self):
        self.db.create_text_index()
        selector = {
            "class": "mammal",
            "likes": {"$all": ["to take tea", "kittens", "trees"]},
        }
        docs = self.db.find(selector)
        self.assertEqual(len(docs), 2)
        self.assertEqual(docs[0]["_id"], "hoary_marmot")
        self.assertEqual(docs[1]["_id"], "hoary_marmots")

    def test_stale_ok(self):
        self.db.create_text_index(name="animals")
        res = self.search(
            "views101", "animals", {"q": "kookaburra", "stale": "ok"}, return_raw=True
        )
        assert "rows" in res
        assert "total_rows" in res

    def test_group_field_number_is_unsupported(self):
        self.db.create_text_index(name="animals")
        res = self.search(
            "views101",
            "animals",
            {"q": "*:*", "group_field": "min_length<number>"},
            fail=True,
        )
        assert "error" in res
        self.assertEqual(res["error"], "bad_request")
        assert res["reason"].startswith("Group by number not supported.")

    def test_post_index_invalid_header(self):
        dd_data = {"index": {"fields": ["foobar"]}}
        response = self.db.sess.post(
            self.db.url + "/_index",
            data=json.dumps(dd_data),
            headers={"Content-Type": None},
        )
        self.assertEqual(
            response.status_code,
            415,
            "This endpoint should require application/json header",
        )
        self.assertEqual(
            response.json()["reason"], "Content-Type must be application/json"
        )
