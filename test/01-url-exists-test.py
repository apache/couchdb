
import time

import mango


def mkdb():
    return mango.Database("http://127.0.0.1:5984/mango_test")


def setup():
    db = mkdb()
    db.recreate()
    time.sleep(0.25)


def test_url_exists():
    db = mkdb()
    r = db.sess.get(db.qurl)
    assert r.status_code == 405


def test_error_empty_actions():
    db = mkdb()
    r = db.sess.post(db.qurl, data="[]")
    assert r.status_code == 200
    assert r.headers["Content-Type"] == "application/json"
    assert r.headers["Transfer-Encoding"] == "chunked"
    assert r.json() == []


def test_bad_content_type():
    db = mkdb()
    hdrs = {"Content-Type": "text/plain"}
    r = db.sess.post(db.qurl, headers=hdrs, data="foo")
    assert r.status_code == 415


def test_invalid_json_body():
    db = mkdb()
    r = db.sess.post(db.qurl, data="foo")
    assert r.status_code == 400


def test_body_json_not_array():
    db = mkdb()
    r = db.sess.post(db.qurl, data="{}")
    assert r.status_code == 400
    assert r.json()["error"] == "bad_request"
    assert len(r.json().get("reason", "")) > 0

