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

import user_docs


def setup():
    user_docs.create_db_and_indexes()


def test_empty():
    db = user_docs.mkdb()
    try:
        db.find({})
    except Exception, e:
        assert e.response.status_code == 400
    else:
        raise AssertionError("bad find")


def test_empty_subsel():
    db = user_docs.mkdb()
    docs = db.find({
            "_id": {"$gt": None},
            "location": {}
        })
    assert len(docs) == 0


def test_empty_subsel_match():
    db = user_docs.mkdb()
    db.save_docs([{"user_id": "eo", "empty_obj": {}}])
    docs = db.find({
            "_id": {"$gt": None},
            "empty_obj": {}
        })
    assert len(docs) == 1
    assert docs[0]["user_id"] == "eo"
