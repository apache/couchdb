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

import json
import time
import unittest
import uuid
import os

import requests

import friend_docs
import user_docs
import limit_docs


def random_db_name():
    return "mango_test_" + uuid.uuid4().hex

def has_text_service():
    return os.path.isfile(os.getcwd() + "/../src/mango_cursor_text.erl")


class Database(object):
    def __init__(self, host, port, dbname, auth=None):
        self.host = host
        self.port = port
        self.dbname = dbname
        self.sess = requests.session()
        self.sess.auth = ('testuser', 'testpass')
        self.sess.headers["Content-Type"] = "application/json"

    @property
    def url(self):
        return "http://{}:{}/{}".format(self.host, self.port, self.dbname)

    def path(self, parts):
        if isinstance(parts, (str, unicode)):
            parts = [parts]
        return "/".join([self.url] + parts)

    def create(self, q=1, n=3):
        r = self.sess.get(self.url)
        if r.status_code == 404:
            r = self.sess.put(self.url, params={"q":q, "n": n})
            r.raise_for_status()

    def delete(self):
        r = self.sess.delete(self.url)

    def recreate(self):
        self.delete()
        time.sleep(1)
        self.create()
        time.sleep(1)

    def save_doc(self, doc):
        self.save_docs([doc])

    def save_docs(self, docs, **kwargs):
        body = json.dumps({"docs": docs})
        r = self.sess.post(self.path("_bulk_docs"), data=body, params=kwargs)
        r.raise_for_status()
        for doc, result in zip(docs, r.json()):
            doc["_id"] = result["id"]
            doc["_rev"] = result["rev"]

    def open_doc(self, docid):
        r = self.sess.get(self.path(docid))
        r.raise_for_status()
        return r.json()

    def ddoc_info(self, ddocid):
        r = self.sess.get(self.path([ddocid, "_info"]))
        r.raise_for_status()
        return r.json()

    def create_index(self, fields, idx_type="json", name=None, ddoc=None, selector=None):
        body = {
            "index": {
                "fields": fields
            },
            "type": idx_type,
            "w": 3
        }
        if name is not None:
            body["name"] = name
        if ddoc is not None:
            body["ddoc"] = ddoc
        if selector is not None:
            body["index"]["selector"] = selector
        body = json.dumps(body)
        r = self.sess.post(self.path("_index"), data=body)
        r.raise_for_status()
        assert r.json()["id"] is not None
        assert r.json()["name"] is not None
        return r.json()["result"] == "created"

    def create_text_index(self, analyzer=None, selector=None, idx_type="text",
        default_field=None, fields=None, name=None, ddoc=None,index_array_lengths=None):
        body = {
            "index": {
            },
            "type": idx_type,
            "w": 3,
        }
        if name is not None:
            body["name"] = name
        if analyzer is not None:
            body["index"]["default_analyzer"] = analyzer
        if default_field is not None:
            body["index"]["default_field"] = default_field
        if index_array_lengths is not None:
            body["index"]["index_array_lengths"] = index_array_lengths
        if selector is not None:
            body["selector"] = selector
        if fields is not None:
            body["index"]["fields"] = fields
        if ddoc is not None:
            body["ddoc"] = ddoc
        body = json.dumps(body)
        r = self.sess.post(self.path("_index"), data=body)
        r.raise_for_status()
        return r.json()["result"] == "created"

    def list_indexes(self, limit="", skip=""):
        if limit != "":
            limit = "limit=" + str(limit)
        if skip != "":
            skip = "skip=" + str(skip)
        r = self.sess.get(self.path("_index?"+limit+";"+skip))
        r.raise_for_status()
        return r.json()["indexes"]

    def delete_index(self, ddocid, name, idx_type="json"):
        path = ["_index", ddocid, idx_type, name]
        r = self.sess.delete(self.path(path), params={"w":"3"})
        r.raise_for_status()

    def bulk_delete(self, docs):
        body = {
            "docids" : docs,
            "w": 3
        }
        body = json.dumps(body)
        r = self.sess.post(self.path("_index/_bulk_delete"), data=body)
        return r.json()

    def find(self, selector, limit=25, skip=0, sort=None, fields=None,
                r=1, conflicts=False, use_index=None, explain=False,
                bookmark=None, return_raw=False, update=True, executionStats=False):
        body = {
            "selector": selector,
            "use_index": use_index,
            "limit": limit,
            "skip": skip,
            "r": r,
            "conflicts": conflicts
        }
        if sort is not None:
            body["sort"] = sort
        if fields is not None:
            body["fields"] = fields
        if bookmark is not None:
            body["bookmark"] = bookmark
        if update == False:
            body["update"] = False
        if executionStats == True:
            body["execution_stats"] = True
        body = json.dumps(body)
        if explain:
            path = self.path("_explain")
        else:
            path = self.path("_find")
        r = self.sess.post(path, data=body)
        r.raise_for_status()
        if explain or return_raw:
            return r.json()
        else:
            return r.json()["docs"]

    def find_one(self, *args, **kwargs):
        results = self.find(*args, **kwargs)
        if len(results) > 1:
            raise RuntimeError("Multiple results for Database.find_one")
        if len(results):
            return results[0]
        else:
            return None


class UsersDbTests(unittest.TestCase):

    @classmethod
    def setUpClass(klass):
        klass.db = Database("127.0.0.1", "15984", "_users")
        user_docs.setup_users(klass.db)

    def setUp(self):
        self.db = self.__class__.db


class DbPerClass(unittest.TestCase):

    @classmethod
    def setUpClass(klass):
        klass.db = Database("127.0.0.1", "15984", random_db_name())
        klass.db.create(q=1, n=3)

    def setUp(self):
        self.db = self.__class__.db


class UserDocsTests(DbPerClass):

    @classmethod
    def setUpClass(klass):
        super(UserDocsTests, klass).setUpClass()
        user_docs.setup(klass.db)


class UserDocsTestsNoIndexes(DbPerClass):

    @classmethod
    def setUpClass(klass):
        super(UserDocsTestsNoIndexes, klass).setUpClass()
        user_docs.setup(
                    klass.db,
                    index_type="_all_docs"
            )


class UserDocsTextTests(DbPerClass):

    DEFAULT_FIELD = None
    FIELDS = None

    @classmethod
    def setUpClass(klass):
        super(UserDocsTextTests, klass).setUpClass()
        if has_text_service():
            user_docs.setup(
                    klass.db,
                    index_type="text",
                    default_field=klass.DEFAULT_FIELD,
                    fields=klass.FIELDS
            )


class FriendDocsTextTests(DbPerClass):

    @classmethod
    def setUpClass(klass):
        super(FriendDocsTextTests, klass).setUpClass()
        if has_text_service():
            friend_docs.setup(klass.db, index_type="text")

class LimitDocsTextTests(DbPerClass):

    @classmethod
    def setUpClass(klass):
        super(LimitDocsTextTests, klass).setUpClass()
        if has_text_service():
            limit_docs.setup(klass.db, index_type="text")
