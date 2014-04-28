
import time

import mango


def mkdb():
    return mango.Database("http://127.0.0.1:5984/mango_test")


def setup():
    db = mkdb()
    db.recreate()
    time.sleep(0.25)


def test_doc_insert():
    db = mkdb()
    doc = {"_id": "foo", "bar": "bam"}
    db.insert(doc)
    assert doc["_id"] == "foo"
    assert len(doc.get("_rev")) > 0

    url = db.url.strip("/") + "/foo"
    r = db.sess.get(url)
    r.raise_for_status()
    assert r.json() == doc


def test_doc_create_id():
    db = mkdb()
    doc = {"bar": "bam"}
    db.insert(doc)
    assert "_id" in doc
    assert len(doc.get("_rev")) > 0

    url = db.url.rstrip("/") + "/" + doc["_id"]
    r = db.sess.get(url)
    r.raise_for_status()
    assert r.json() == doc


def test_multi_docs():
    db = mkdb()
    docs = [
        {"_id": "bam", "zig": "zag"},
        {"baz": "bag"}
    ]
    db.insert(docs)

    for doc in docs:
        assert "_id" in doc
        assert "_rev" in doc

    for doc in docs:
        url = db.url.rstrip("/") + "/" + doc["_id"]
        r = db.sess.get(url)
        r.raise_for_status()
        assert r.json() == doc


def test_doc_update():
    db = mkdb()
    doc = {"_id": "bing"}
    db.insert(doc)
    rev1 = doc["_rev"]
    doc["zap"] = "zang"
    db.insert(doc)

    assert doc["_rev"] != rev1

    url = db.url.rstrip("/") + "/" + doc["_id"]
    r = db.sess.get(url)
    r.raise_for_status()
    assert r.json() == doc

