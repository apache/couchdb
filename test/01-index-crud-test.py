
import random
import time

import mango


def mkdb():
    return mango.Database("127.0.0.1", "5984", "mango_test")


def setup():
    db = mkdb()
    db.recreate()
    time.sleep(1)


def test_bad_fields():
    db = mkdb()
    bad_fields = [
        None,
        True,
        False,
        "bing",
        2.0,
        {"foo": "bar"},
        [{"foo": 2}],
        [{"foo": "asc", "bar": "desc"}],
        [{"foo": "asc"}, {"bar": "desc"}]
    ]
    for fields in bad_fields:
        try:
            db.create_index(fields)
        except Exception, e:
            assert e.response.status_code == 400
        else:
            raise AssertionError("bad create index")


def test_bad_types():
    db = mkdb()
    bad_types = [
        None,
        True,
        False,
        1.5,
        "foo",
        "text", # Future support
        "geo", # Future support
        {"foo": "bar"},
        ["baz", 3.0]
    ]
    for bt in bad_types:
        try:
            db.create_index(["foo"], idx_type=bt)
        except Exception, e:
            assert e.response.status_code == 400, (bt, e.response.status_code)
        else:
            raise AssertionError("bad create index")


def test_bad_names():
    db = mkdb()
    bad_names = [
        True,
        False,
        1.5,
        {"foo": "bar"},
        [None, False]
    ]
    for bn in bad_names:
        try:
            db.create_index(["foo"], name=bn)
        except Exception, e:
            assert e.response.status_code == 400
        else:
            raise AssertionError("bad create index")
        try:
            db.create_index(["foo"], ddoc=bn)
        except Exception, e:
            assert e.response.status_code == 400
        else:
            raise AssertionError("bad create index")


def test_create_idx_01():
    db = mkdb()
    fields = ["foo", "bar"]
    ret = db.create_index(fields, name="idx_01")
    assert ret is True
    for idx in db.list_indexes():
        if idx["name"] != "idx_01":
            continue
        assert idx["def"]["fields"] == [{"foo": "asc"}, {"bar": "asc"}]
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
    ret = db.create_index(fields, name="idx_02")
    assert ret is True
    for idx in db.list_indexes():
        if idx["name"] != "idx_02":
            continue
        assert idx["def"]["fields"] == [{"baz": "asc"}, {"foo": "asc"}]
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
        db.delete_index(idx["ddoc"].split("/")[-1], idx["name"])
    post_indexes = db.list_indexes()
    assert pre_indexes == post_indexes


def test_delete_misisng():
    db = mkdb()

    # Missing design doc
    try:
        db.delete_index("this_is_not_a_design_doc_id", "foo")
    except Exception, e:
        assert e.response.status_code == 404
    else:
        raise AssertionError("bad index delete")

    # Missing view name
    indexes = db.list_indexes()
    idx = random.choice([idx for idx in indexes if idx["type"] != "special"])
    ddocid = idx["ddoc"].split("/")[-1]
    try:
        db.delete_index(ddocid, "this_is_not_an_index_name")
    except Exception, e:
        assert e.response.status_code == 404
    else:
        raise AssertionError("bad index delete")

    # Bad view type
    try:
        db.delete_index(ddocid, idx["name"], idx_type="not_a_real_type")
    except Exception, e:
        assert e.response.status_code == 404
    else:
        raise AssertionError("bad index delete")
