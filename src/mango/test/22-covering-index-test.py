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


class CoveringIndexTests:
    def test_index_covers_query_1field_index_id(self):
        self.is_covered({"age": {"$gte": 32}}, ["_id"], "age")

    def test_index_covers_query_2field_index_id(self):
        self.is_covered(
            {"company": "Lyria", "manager": True}, ["_id"], "company_and_manager"
        )

    def test_index_covers_query_2field_index_extract_field(self):
        self.is_covered(
            {"company": {"$exists": True}, "manager": True},
            ["company"],
            "company_and_manager",
        )

    def test_index_covers_query_2field_index_extract_field_force_index(self):
        self.is_covered(
            {"company": {"$exists": True}, "manager": True},
            ["company"],
            "company_and_manager",
            use_index="company_and_manager",
        )

    def test_index_covers_query_elemMatch(self):
        self.is_covered(
            {"favorites": {"$elemMatch": {"$eq": "Erlang"}}}, ["favorites"], "favorites"
        )

    def test_index_covers_query_composite_field_id(self):
        self.is_covered(
            {"name": {"first": "Stephanie", "last": "Kirkland"}}, ["_id"], "name"
        )

    def test_index_does_not_cover_query_empty_selector(self):
        self.is_not_covered({}, ["_id"])

    def test_index_does_not_cover_query_field_not_in_index(self):
        self.is_not_covered({"age": {"$gte": 32}}, ["name"])

    def test_index_does_not_cover_query_all_fields(self):
        self.is_not_covered({"age": {"$gte": 32}}, None)

    def test_index_does_not_cover_query_partial_selector_id(self):
        self.is_not_covered({"location.state": "Nevada"}, ["_id"])

    def test_index_does_not_cover_query_partial_selector(self):
        self.is_not_covered({"name.last": "Hernandez"}, ["name.first"])

    def test_index_does_not_cover_selector_with_more_fields(self):
        self.is_not_covered(
            {
                "$and": [
                    {"age": {"$ne": 23}},
                    {"twitter": {"$not": {"$regex": "^@.*[0-9]+$"}}},
                    {"location.address.number": {"$gt": 4288}},
                    {"location.city": {"$ne": "Pico Rivera"}},
                ]
            },
            ["twitter"],
            use_index="twitter",
        )


class RegularCoveringIndexTests(mango.UserDocsTests, CoveringIndexTests):
    @classmethod
    def setUpClass(klass):
        super(RegularCoveringIndexTests, klass).setUpClass()

    def is_covered(self, selector, fields, index, use_index=None):
        resp = self.db.find(selector, fields=fields, use_index=use_index, explain=True)
        self.assertEqual(resp["index"]["type"], "json")
        self.assertEqual(resp["index"]["name"], index)
        self.assertEqual(resp["mrargs"]["include_docs"], False)
        self.assertEqual(resp["covered"], True)

    def is_not_covered(self, selector, fields, use_index=None):
        resp = self.db.find(selector, fields=fields, use_index=use_index, explain=True)
        self.assertEqual(resp["mrargs"]["include_docs"], True)
        self.assertEqual(resp["covered"], False)

    def test_covering_index_provides_correct_answer_2field_index(self):
        docs = self.db.find(
            {"company": {"$exists": True}, "manager": True},
            sort=[{"company": "asc"}],
            fields=["company"],
            use_index="company_and_manager",
        )
        expected = [
            {"company": "Affluex"},
            {"company": "Globoil"},
            {"company": "Lyria"},
            {"company": "Manglo"},
            {"company": "Myopium"},
            {"company": "Niquent"},
            {"company": "Oulu"},
            {"company": "Prosely"},
            {"company": "Tasmania"},
            {"company": "Zialactic"},
        ]
        self.assertEqual(docs, expected)

    def test_covering_index_provides_correct_answer_id(self):
        docs = self.db.find({"age": {"$gte": 32}}, fields=["_id"])
        expected = [
            {"_id": "659d0430-b1f4-413a-a6b7-9ea1ef071325"},
            {"_id": "48ca0455-8bd0-473f-9ae2-459e42e3edd1"},
            {"_id": "e900001d-bc48-48a6-9b1a-ac9a1f5d1a03"},
            {"_id": "b31dad3f-ae8b-4f86-8327-dfe8770beb27"},
            {"_id": "71562648-6acb-42bc-a182-df6b1f005b09"},
            {"_id": "c78c529f-0b07-4947-90a6-d6b7ca81da62"},
            {"_id": "8e1c90c0-ac18-4832-8081-40d14325bde0"},
            {"_id": "6c0afcf1-e57e-421d-a03d-0c0717ebf843"},
            {"_id": "5b61abc1-a3d3-4092-b9d7-ced90e675536"},
            {"_id": "a33d5457-741a-4dce-a217-3eab28b24e3e"},
            {"_id": "b06aadcf-cd0f-4ca6-9f7e-2c993e48d4c4"},
            {"_id": "b1e70402-8add-4068-af8f-b4f3d0feb049"},
            {"_id": "0461444c-e60a-457d-a4bb-b8d811853f21"},
        ]
        self.assertEqual(docs, expected)


class PartitionedCoveringIndexTests(mango.PartitionedUserDocsTests, CoveringIndexTests):
    @classmethod
    def setUpClass(klass):
        super(PartitionedCoveringIndexTests, klass).setUpClass()

    def is_covered(self, selector, fields, index, use_index=None):
        resp = self.db.find(
            selector, fields=fields, use_index=use_index, explain=True, partition="0"
        )
        self.assertEqual(resp["index"]["type"], "json")
        self.assertEqual(resp["index"]["name"], index)
        self.assertEqual(resp["mrargs"]["include_docs"], False)
        self.assertEqual(resp["covered"], True)

    def is_not_covered(self, selector, fields, use_index=None):
        resp = self.db.find(
            selector, fields=fields, use_index=use_index, explain=True, partition="0"
        )
        self.assertEqual(resp["mrargs"]["include_docs"], True)
        self.assertEqual(resp["covered"], False)

    def test_covering_index_provides_correct_answer_2field_index(self):
        docs = self.db.find(
            {"company": {"$exists": True}, "manager": True},
            sort=[{"company": "asc"}],
            fields=["company"],
            use_index="company_and_manager",
            partition="0",
        )
        expected = [
            {"company": "Manglo"},
            {"company": "Oulu"},
            {"company": "Prosely"},
            {"company": "Tasmania"},
        ]
        self.assertEqual(docs, expected)

    def test_covering_index_provides_correct_answer_id(self):
        docs = self.db.find({"age": {"$gte": 32}}, fields=["_id"], partition="0")
        expected = [
            {"_id": "0:0461444c-e60a-457d-a4bb-b8d811853f21"},
            {"_id": "0:5b61abc1-a3d3-4092-b9d7-ced90e675536"},
            {"_id": "0:71562648-6acb-42bc-a182-df6b1f005b09"},
            {"_id": "0:b31dad3f-ae8b-4f86-8327-dfe8770beb27"},
        ]
        self.assertEqual(sorted(docs, key=lambda x: x["_id"]), expected)
