
import time

import mango


def mkdb():
    return mango.Database("http://127.0.0.1:5984/mango_test")


def setup():
    db = mkdb()
    db.recreate()
    time.sleep(0.25)
    docs = []
    for i in range(1, 11):
        docs.append({
            "_id": str(i),
            "int": i,
            "even": (i % 2) == 0
        })
    assert db.idx_create(["int"]) == True
    db.insert(docs)


def test_basic_update():
    db = mkdb()
    result = db.update({"_id": "1"}, {"$set": {"int": 11}})
    assert result[0]["id"] == "1"
    assert "rev" in result[0]

