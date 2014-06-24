
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
