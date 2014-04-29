
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


def test_id_lookup():
    db = mkdb()
    doc = db.find_one({"_id": "1"})
    assert doc["_id"] == "1"
    assert "_rev" in doc
    assert doc["int"] == 1


def test_id_range():
    db = mkdb()
    docs = db.find({"_id": {"$and": [{"$gt": "1"}, {"$lte": "2"}]}})
    # returns "10" and "2"
    assert len(docs) == 2
    assert docs[0]["_id"] == "10"
    assert docs[1]["_id"] == "2"


def test_id_open_range():
    # This should fail but accidentally doesn't because of
    # collation order differences between bson comparisons
    # and the raw collation order for _id.
    db = mkdb()
    docs = db.find({"_id": {"$gt": "1"}})
    assert len(docs) == 9


def test_secondary_idx_lookup():
    db = mkdb()
    doc = db.find_one({"int": 1})
    assert doc["_id"] == "1"
    assert "_rev" in doc
    assert doc["int"] == 1


def test_secondary_idx_range():
    # Subtle test here showing that ordering by int
    # values gets the "10" _id in the right spot.
    db = mkdb()
    docs = db.find({"int": {"$and": [{"$gte": 1}, {"$lte": 4}]}})
    assert len(docs) == 4
    assert docs[0]["_id"] == "1"
    assert docs[1]["_id"] == "2"
    assert docs[2]["_id"] == "3"
    assert docs[3]["_id"] == "4"


def test_secondary_idx_exclusion():
    db = mkdb()
    docs = db.find({"int": {"$gt": 5}, "even": True})
    assert len(docs) == 3
    assert docs[0]["_id"] == "6"
    assert docs[1]["_id"] == "8"
    assert docs[2]["_id"] == "10"
