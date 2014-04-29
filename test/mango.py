
import json

import requests


class Database(object):
    def __init__(self, url, auth=None):
        self.url = url
        self.qurl = url.rstrip("/") + "/_query"
        self.sess = requests.session()
        if auth is not None:
            self.sess.auth = auth
        self.sess.headers["Content-Type"] = "application/json"

    def create(self):
        r = self.sess.get(self.url)
        if r.status_code == 404:
            r = self.sess.put(self.url)
            r.raise_for_status()

    def delete(self):
        r = self.sess.delete(self.url)

    def recreate(self):
        self.delete()
        self.create()

    def insert(self, docs):
        if isinstance(docs, dict):
            docs = [docs]
        action = {
            "action": "insert",
            "docs": docs
        }
        body = json.dumps([action])
        r = self.sess.post(self.qurl, data=body)
        r.raise_for_status()
        result = r.json()[0]
        if not result["ok"]:
            raise RuntimError(result["result"])
        for doc, result in zip(docs, result["result"]):
            doc["_id"] = result["id"]
            doc["_rev"] = result["rev"]

        return r.json()

    def find(self, selector, limit=25, skip=0, sort=None, fields=None,
                r=1, conflicts=False):
        action = {
            "action": "find",
            "selector": selector,
            "limit": limit,
            "skip": skip,
            "r": r,
            "conflicts": conflicts
        }
        if sort is not None:
            action["sort"] = sort
        if fields is not None:
            action["fields"] = fields
        body = json.dumps([action])
        r = self.sess.post(self.qurl, data=body)
        r.raise_for_status()
        result = r.json()[0]
        if not result["ok"]:
            raise RuntimeError(result["result"])
        return result["result"]

    def update(self, selector, update, upsert=False, limit=1, sort=None,
                r=1, w=2):
        action = {
            "action": "update",
            "selector": selector,
            "update": update,
            "upsert": upsert,
            "limit": limit,
            "r": r,
            "w": w
        }
        if sort is not None:
            action["sort"] = sort
        body = json.dumps([action])
        r = self.sess.post(self.qurl, data=body)
        print r.text
        r.raise_for_status()
        result = r.json()[0]
        if not result["ok"]:
            raise RuntimeError(result["result"])
        return result["result"]

    def find_one(self, *args, **kwargs):
        results = self.find(*args, **kwargs)
        if len(results) > 1:
            raise RuntimeError("Multiple results for Database.find_one")
        if len(results):
            return results[0]
        else:
            return None

    def idx_create(self, fields, missing_is_null=False,
                idx_type="json", name=None, ddoc=None):
        action = {
            "action": "create_index",
            "index": {"fields": fields, "missing_is_null": missing_is_null},
            "type": idx_type
        }
        if name is not None:
            action["name"] = name
        if ddoc is not None:
            action["ddoc"] = ddoc
        body = json.dumps([action])
        r = self.sess.post(self.qurl, data=body)
        r.raise_for_status()
        print r.text
        result = r.json()[0]
        if not result["ok"]:
            raise RuntimeError(result["result"])
        return result["result"] == "created"



