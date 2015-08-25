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
import literal_gen


def setup(db, index_type="view"):
    db.recreate()
    DOCS[2]["number_string"] = literal_gen.floating_point_literal()
    DOCS[3]["number_string"] = literal_gen.hex_floating_point_literal()
    db.save_docs(copy.deepcopy(DOCS))
    if index_type == "view":
        add_view_indexes(db)
    elif index_type == "text":
        add_text_indexes(db)


def add_text_indexes(db):
    db.create_text_index()


DOCS =  [
  {
    "_id": "55118b87283f8f2901c59663",
    "number_string": "NaN"
  },
  {
    "_id": "55118b873c98123d69bff407",
    "number_string": "Infinity"
  },
  {
    "_id": "55118b87b4e99951e6fbe5c4",
    "number_string": "filler"
  },
  {
    "_id": "55118b87bc21952536ef00da",
    "number_string": "filler"
  }
]