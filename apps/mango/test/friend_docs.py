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

"""
Generated with http://www.json-generator.com/

With this pattern:

[
  '{{repeat(15)}}',
  {
    _id: '{{index()}}',
    name: {
      first: '{{firstName()}}',
      last: '{{surname()}}'
    },
    friends: [
      '{{repeat(3)}}',
      {
        id: '{{index()}}',
        name: {
          first: '{{firstName()}}',
          last: '{{surname()}}'
        },
        type: '{{random("personal", "work")}}'
      }
    ]
  }
]
"""

import copy


def setup(db, index_type="view"):
    db.recreate()
    db.save_docs(copy.deepcopy(DOCS))
    if index_type == "view":
        add_view_indexes(db)
    elif index_type == "text":
        add_text_indexes(db)


def add_text_indexes(db):
    db.create_text_index()


DOCS = [
    {
        "_id": "54a43171d37ae5e81bff5ae0",
        "user_id": 0,
        "name": {"first": "Ochoa", "last": "Fox"},
        "friends": [
            {
                "id": 0,
                "name": {"first": "Sherman", "last": "Davidson"},
                "type": "personal",
            },
            {
                "id": 1,
                "name": {"first": "Vargas", "last": "Mendez"},
                "type": "personal",
            },
            {"id": 2, "name": {"first": "Sheppard", "last": "Cotton"}, "type": "work"},
        ],
    },
    {
        "_id": "54a43171958485dc32917c50",
        "user_id": 1,
        "name": {"first": "Sheppard", "last": "Cotton"},
        "friends": [
            {"id": 0, "name": {"first": "Ochoa", "last": "Fox"}, "type": "work"},
            {
                "id": 1,
                "name": {"first": "Vargas", "last": "Mendez"},
                "type": "personal",
            },
            {"id": 2, "name": {"first": "Kendra", "last": "Burns"}, "type": "work"},
        ],
    },
    {
        "_id": "54a431711cf025ba74bea899",
        "user_id": 2,
        "name": {"first": "Hunter", "last": "Wells"},
        "friends": [
            {"id": 0, "name": {"first": "Estes", "last": "Fischer"}, "type": "work"},
            {
                "id": 1,
                "name": {"first": "Farrell", "last": "Maddox"},
                "type": "personal",
            },
            {"id": 2, "name": {"first": "Kendra", "last": "Burns"}, "type": "work"},
        ],
    },
    {
        "_id": "54a4317151a70a9881ac28a4",
        "user_id": 3,
        "name": {"first": "Millicent", "last": "Guy"},
        "friends": [
            {"id": 0, "name": {"first": "Luella", "last": "Mendoza"}, "type": "work"},
            {
                "id": 1,
                "name": {"first": "Melanie", "last": "Foster"},
                "type": "personal",
            },
            {"id": 2, "name": {"first": "Hopkins", "last": "Scott"}, "type": "work"},
        ],
    },
    {
        "_id": "54a43171d946b78703a0e076",
        "user_id": 4,
        "name": {"first": "Elisabeth", "last": "Brady"},
        "friends": [
            {"id": 0, "name": {"first": "Sofia", "last": "Workman"}, "type": "work"},
            {"id": 1, "name": {"first": "Alisha", "last": "Reilly"}, "type": "work"},
            {"id": 2, "name": {"first": "Ochoa", "last": "Burch"}, "type": "personal"},
        ],
    },
    {
        "_id": "54a4317118abd7f1992464ee",
        "user_id": 5,
        "name": {"first": "Pollard", "last": "French"},
        "friends": [
            {
                "id": 0,
                "name": {"first": "Hollie", "last": "Juarez"},
                "type": "personal",
            },
            {"id": 1, "name": {"first": "Nelda", "last": "Newton"}, "type": "personal"},
            {"id": 2, "name": {"first": "Yang", "last": "Pace"}, "type": "personal"},
        ],
    },
    {
        "_id": "54a43171f139e63d6579121e",
        "user_id": 6,
        "name": {"first": "Acevedo", "last": "Morales"},
        "friends": [
            {"id": 0, "name": {"first": "Payne", "last": "Berry"}, "type": "personal"},
            {
                "id": 1,
                "name": {"first": "Rene", "last": "Valenzuela"},
                "type": "personal",
            },
            {"id": 2, "name": {"first": "Dora", "last": "Gallegos"}, "type": "work"},
        ],
    },
    {
        "_id": "54a431719783cef80876dde8",
        "user_id": 7,
        "name": {"first": "Cervantes", "last": "Marquez"},
        "friends": [
            {
                "id": 0,
                "name": {"first": "Maxwell", "last": "Norman"},
                "type": "personal",
            },
            {"id": 1, "name": {"first": "Shields", "last": "Bass"}, "type": "personal"},
            {"id": 2, "name": {"first": "Luz", "last": "Jacobson"}, "type": "work"},
        ],
    },
    {
        "_id": "54a43171ecc7540d1f7aceae",
        "user_id": 8,
        "name": {"first": "West", "last": "Morrow"},
        "friends": [
            {
                "id": 0,
                "name": {"first": "Townsend", "last": "Dixon"},
                "type": "personal",
            },
            {
                "id": 1,
                "name": {"first": "Callahan", "last": "Buck"},
                "type": "personal",
            },
            {
                "id": 2,
                "name": {"first": "Rachel", "last": "Fletcher"},
                "type": "personal",
            },
        ],
    },
    {
        "_id": "54a4317113e831f4af041a0a",
        "user_id": 9,
        "name": {"first": "Cotton", "last": "House"},
        "friends": [
            {
                "id": 0,
                "name": {"first": "Mckenzie", "last": "Medina"},
                "type": "personal",
            },
            {"id": 1, "name": {"first": "Cecilia", "last": "Miles"}, "type": "work"},
            {"id": 2, "name": {"first": "Guerra", "last": "Cervantes"}, "type": "work"},
        ],
    },
    {
        "_id": "54a43171686eb1f48ebcbe01",
        "user_id": 10,
        "name": {"first": "Wright", "last": "Rivas"},
        "friends": [
            {
                "id": 0,
                "name": {"first": "Campos", "last": "Freeman"},
                "type": "personal",
            },
            {
                "id": 1,
                "name": {"first": "Christian", "last": "Ferguson"},
                "type": "personal",
            },
            {"id": 2, "name": {"first": "Doreen", "last": "Wilder"}, "type": "work"},
        ],
    },
    {
        "_id": "54a43171a4f3d5638c162f4f",
        "user_id": 11,
        "name": {"first": "Lorene", "last": "Dorsey"},
        "friends": [
            {
                "id": 0,
                "name": {"first": "Gibbs", "last": "Mccarty"},
                "type": "personal",
            },
            {"id": 1, "name": {"first": "Neal", "last": "Franklin"}, "type": "work"},
            {"id": 2, "name": {"first": "Kristy", "last": "Head"}, "type": "personal"},
        ],
        "bestfriends": ["Wolverine", "Cyclops"],
    },
    {
        "_id": "54a431719faa420a5b4fbeb0",
        "user_id": 12,
        "name": {"first": "Juanita", "last": "Cook"},
        "friends": [
            {"id": 0, "name": {"first": "Wilkins", "last": "Chang"}, "type": "work"},
            {"id": 1, "name": {"first": "Haney", "last": "Rivera"}, "type": "work"},
            {"id": 2, "name": {"first": "Lauren", "last": "Manning"}, "type": "work"},
        ],
    },
    {
        "_id": "54a43171e65d35f9ee8c53c0",
        "user_id": 13,
        "name": {"first": "Levy", "last": "Osborn"},
        "friends": [
            {"id": 0, "name": {"first": "Vinson", "last": "Vargas"}, "type": "work"},
            {"id": 1, "name": {"first": "Felicia", "last": "Beach"}, "type": "work"},
            {"id": 2, "name": {"first": "Nadine", "last": "Kemp"}, "type": "work"},
        ],
        "results": [82, 85, 88],
    },
    {
        "_id": "54a4317132f2c81561833259",
        "user_id": 14,
        "name": {"first": "Christina", "last": "Raymond"},
        "friends": [
            {"id": 0, "name": {"first": "Herrera", "last": "Walton"}, "type": "work"},
            {"id": 1, "name": {"first": "Hahn", "last": "Rutledge"}, "type": "work"},
            {"id": 2, "name": {"first": "Stacie", "last": "Harding"}, "type": "work"},
        ],
    },
    {
        "_id": "589f32af493145f890e1b051",
        "user_id": 15,
        "name": {"first": "Tanisha", "last": "Bowers"},
        "friends": [
            {"id": 0, "name": {"first": "Ochoa", "last": "Pratt"}, "type": "personal"},
            {"id": 1, "name": {"first": "Ochoa", "last": "Romero"}, "type": "personal"},
            {"id": 2, "name": {"first": "Ochoa", "last": "Bowman"}, "type": "work"},
        ],
    },
]
