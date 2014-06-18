
import user_docs


def setup():
    user_docs.create_db_and_indexes()


def test_regex():
    db = user_docs.mkdb()
    
    docs = db.find({
            "age": {"$gt": 40},
            "location.state": {"$regex": "(?i)new.*"}
        })
    assert len(docs) == 2
    assert docs[0]["user_id"] == 2
    assert docs[1]["user_id"] == 10