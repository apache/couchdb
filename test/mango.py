
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


