
import time

import mango


def mkdb():
    return mango.Database("127.0.0.1", "5984", "mango_test")


def setup():
    db = mkdb()
    db.recreate()
    time.sleep(1)


def test_create_idx_01():
    db = mkdb()
    fields = ["foo", "bar"]
    ret = db.create_index(fields, name="idx_01")
    assert ret is True
    for idx in db.list_indexes():
        if idx["name"] != "idx_01":
            continue
        assert idx["def"]["fields"] == [{"foo": "asc"}, {"bar": "asc"}]
        assert idx["def"]["missing_is_null"] == False
        return
    raise AssertionError("index not created")


def test_create_idx_01_exists():
    db = mkdb()
    fields = ["foo", "bar"]
    ret = db.create_index(fields, name="idx_01")
    assert ret is False


def test_create_idx_02():
    db = mkdb()
    fields = ["baz", "foo"]
    ret = db.create_index(fields, missing_is_null=True, name="idx_02")
    assert ret is True
    for idx in db.list_indexes():
        if idx["name"] != "idx_02":
            continue
        assert idx["def"]["fields"] == [{"baz": "asc"}, {"foo": "asc"}]
        assert idx["def"]["missing_is_null"] is True
        return
    raise AssertionError("index not created")


def test_read_idx_doc():
    db = mkdb()
    for idx in db.list_indexes():
        if idx["type"] == "special":
            continue
        ddocid = idx["ddoc"]
        doc = db.open_doc(ddocid)
        assert doc["_id"] == ddocid
        info = db.ddoc_info(ddocid)
        assert info["name"] == ddocid


def test_delete_idx():
    db = mkdb()
    pre_indexes = db.list_indexes()
    ret = db.create_index(["bing"], name="idx_del")
    assert ret is True
    for idx in db.list_indexes():
        if idx["name"] != "idx_del":
            continue
        assert idx["def"]["fields"] == [{"bing": "asc"}]
        assert idx["def"]["missing_is_null"] is False
        db.delete_index(idx["ddoc"].split("/")[-1], idx["name"])
    post_indexes = db.list_indexes()
    assert pre_indexes == post_indexes
