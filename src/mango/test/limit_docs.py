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


def setup(db, index_type="view"):
    db.recreate()
    db.save_docs(copy.deepcopy(DOCS))
    if index_type == "view":
        add_view_indexes(db)
    elif index_type == "text":
        add_text_indexes(db)


def add_text_indexes(db):
    db.create_text_index()


DOCS =  [
  {
    "_id": "54af50626de419f5109c962f",
    "user_id": 0,
    "age": 10
  },
  {
    "_id": "54af50622071121b25402dc3",
    "user_id": 1,
    "age": 11

  },
  {
    "_id": "54af50623809e19159a3cdd0",
    "user_id": 2,
    "age": 12
  },
  {
    "_id": "54af50629f45a0f49a441d01",
    "user_id": 3,
    "age": 13

  },
  {
    "_id": "54af50620f1755c22359a362",
    "user_id": 4,
    "age": 14
  },
  {
    "_id": "54af5062dd6f6c689ad2ca23",
    "user_id": 5,
    "age": 15
  },
  {
    "_id": "54af50623e89b432be1187b8",
    "user_id": 6,
    "age": 16
  },
  {
    "_id": "54af5062932a00270a3b5ab0",
    "user_id": 7,
    "age": 17

  },
  {
    "_id": "54af5062df773d69174e3345",
    "filtered_array" : [1, 2, 3],
    "age": 18
  },
  {
    "_id": "54af50629c1153b9e21e346d",
    "filtered_array" : [1, 2, 3],
    "age": 19
  },
  {
    "_id": "54af5062dabb7cc4b60e0c95",
    "user_id": 10,
    "age": 20
  },
  {
    "_id": "54af5062204996970a4439a2",
    "user_id": 11,
    "age": 21
  },
  {
    "_id": "54af50629cea39e8ea52bfac",
    "user_id": 12,
    "age": 22
  },
  {
    "_id": "54af50620597c094f75db2a1",
    "user_id": 13,
    "age": 23
  },
  {
    "_id": "54af50628d4048de0010723c",
    "user_id": 14,
    "age": 24
  },
  {
    "_id": "54af5062f339b6f44f52faf6",
    "user_id": 15,
    "age": 25
  },
  {
    "_id": "54af5062a893f17ea4402031",
    "user_id": 16,
    "age": 26
  },
  {
    "_id": "54af5062323dbc7077deb60a",
    "user_id": 17,
    "age": 27
  },
  {
    "_id": "54af506224db85bd7fcd0243",
    "filtered_array" : [1, 2, 3],
    "age": 28
  },
  {
    "_id": "54af506255bb551c9cc251bf",
    "filtered_array" : [1, 2, 3],
    "age": 29
  },
  {
    "_id": "54af50625a97394e07d718a1",
    "filtered_array" : [1, 2, 3],
    "age": 30
  },
  {
    "_id": "54af506223f51d586b4ef529",
    "user_id": 21,
    "age": 31
  },
  {
    "_id": "54af50622740dede7d6117b7",
    "user_id": 22,
    "age": 32
  },
  {
    "_id": "54af50624efc87684a52e8fb",
    "user_id": 23,
    "age": 33
  },
  {
    "_id": "54af5062f40932760347799c",
    "user_id": 24,
    "age": 34
  },
  {
    "_id": "54af5062d9f7361951ac645d",
    "user_id": 25,
    "age": 35
  },
  {
    "_id": "54af5062f89aef302b37c3bc",
    "filtered_array" : [1, 2, 3],
    "age": 36
  },
  {
    "_id": "54af5062498ec905dcb351f8",
    "filtered_array" : [1, 2, 3],
    "age": 37
  },
  {
    "_id": "54af5062b1d2f2c5a85bdd7e",
    "user_id": 28,
    "age": 38
  },
  {
    "_id": "54af50625061029c0dd942b5",
    "filtered_array" : [1, 2, 3],
    "age": 39
  },
  {
    "_id": "54af50628b0d08a1d23c030a",
    "user_id": 30,
    "age": 40
  },
  {
    "_id": "54af506271b6e3119eb31d46",
    "filtered_array" : [1, 2, 3],
    "age": 41
  },
  {
    "_id": "54af5062b69f46424dfcf3e5",
    "user_id": 32,
    "age": 42
  },
  {
    "_id": "54af5062ed00c7dbe4d1bdcf",
    "user_id": 33,
    "age": 43
  },
  {
    "_id": "54af5062fb64e45180c9a90d",
    "user_id": 34,
    "age": 44
  },
  {
    "_id": "54af5062241c72b067127b09",
    "user_id": 35,
    "age": 45
  },
  {
    "_id": "54af50626a467d8b781a6d06",
    "user_id": 36,
    "age": 46
  },
  {
    "_id": "54af50620e992d60af03bf86",
    "filtered_array" : [1, 2, 3],
    "age": 47
  },
  {
    "_id": "54af506254f992aa3c51532f",
    "user_id": 38,
    "age": 48
  },
  {
    "_id": "54af5062e99b20f301de39b9",
    "user_id": 39,
    "age": 49
  },
  {
    "_id": "54af50624fbade6b11505b5d",
    "user_id": 40,
    "age": 50
  },
  {
    "_id": "54af506278ad79b21e807ae4",
    "user_id": 41,
    "age": 51
  },
  {
    "_id": "54af5062fc7a1dcb33f31d08",
    "user_id": 42,
    "age": 52
  },
  {
    "_id": "54af5062ea2c954c650009cf",
    "user_id": 43,
    "age": 53
  },
  {
    "_id": "54af506213576c2f09858266",
    "user_id": 44,
    "age": 54
  },
  {
    "_id": "54af50624a05ac34c994b1c0",
    "user_id": 45,
    "age": 55
  },
  {
    "_id": "54af50625a624983edf2087e",
    "user_id": 46,
    "age": 56
  },
  {
    "_id": "54af50623de488c49d064355",
    "user_id": 47,
    "age": 57
  },
  {
    "_id": "54af5062628b5df08661a9d5",
    "user_id": 48,
    "age": 58
  },
  {
    "_id": "54af50620c706fc23032ae62",
    "user_id": 49,
    "age": 59
  },
  {
    "_id": "54af5062509f1e2371fe1da4",
    "user_id": 50,
    "age": 60
  },
  {
    "_id": "54af50625e96b22436791653",
    "user_id": 51,
    "age": 61
  },
  {
    "_id": "54af5062a9cb71463bb9577f",
    "user_id": 52,
    "age": 62
  },
  {
    "_id": "54af50624fea77a4221a4baf",
    "user_id": 53,
    "age": 63
  },
  {
    "_id": "54af5062c63df0a147d2417e",
    "user_id": 54,
    "age": 64
  },
  {
    "_id": "54af50623c56d78029316c9f",
    "user_id": 55,
    "age": 65
  },
  {
    "_id": "54af5062167f6e13aa0dd014",
    "user_id": 56,
    "age": 66
  },
  {
    "_id": "54af50621558abe77797d137",
    "filtered_array" : [1, 2, 3],
    "age": 67
  },
  {
    "_id": "54af50624d5b36aa7cb5fa77",
    "user_id": 58,
    "age": 68
  },
  {
    "_id": "54af50620d79118184ae66bd",
    "user_id": 59,
    "age": 69
  },
  {
    "_id": "54af5062d18aafa5c4ca4935",
    "user_id": 60,
    "age": 71
  },
  {
    "_id": "54af5062fd22a409649962f4",
    "filtered_array" : [1, 2, 3],
    "age": 72
  },
  {
    "_id": "54af5062e31045a1908e89f9",
    "user_id": 62,
    "age": 73
  },
  {
    "_id": "54af50624c062fcb4c59398b",
    "user_id": 63,
    "age": 74
  },
  {
    "_id": "54af506241ec83430a15957f",
    "user_id": 64,
    "age": 75
  },
  {
    "_id": "54af506224d0f888ae411101",
    "user_id": 65,
    "age": 76
  },
  {
    "_id": "54af506272a971c6cf3ab6b8",
    "user_id": 66,
    "age": 77
  },
  {
    "_id": "54af506221e25b485c95355b",
    "user_id": 67,
    "age": 78
  },
  {
    "_id": "54af5062800f7f2ca73e9623",
    "user_id": 68,
    "age": 79
  },
  {
    "_id": "54af5062bc962da30740534a",
    "user_id": 69,
    "age": 80
  },
  {
    "_id": "54af50625102d6e210fc2efd",
    "filtered_array" : [1, 2, 3],
    "age": 81
  },
  {
    "_id": "54af5062e014b9d039f02c5e",
    "user_id": 71,
    "age": 82
  },
  {
    "_id": "54af5062fbd5e801dd217515",
    "user_id": 72,
    "age": 83
  },
  {
    "_id": "54af50629971992b658fcb88",
    "user_id": 73,
    "age": 84
  },
  {
    "_id": "54af5062607d53416c30bafd",
    "filtered_array" : [1, 2, 3],
    "age": 85
  }
]
