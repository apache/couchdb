
import time

import mango


def mkdb():
    return mango.Database("127.0.0.1", "5984", "mango_test")


def setup():
    db = mkdb()
    db.recreate()
    time.sleep(1)
    docs = []
    for i in range(1, 11):
        docs.append({
            "_id": str(i),
            "int": i,
            "even": (i % 2) == 0
        })
    assert db.create_index(["int"]) == True
    db.save_docs(docs)


def test_simple_find():
    db = mkdb()
    docs = db.find({"int": {"$gt": 9}})
    print docs

    