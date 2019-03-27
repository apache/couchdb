#!/usr/bin/env python

import requests

S = requests.session()
S.auth = ("adm", "pass")
S.headers["Content-Type"] = "application/json"


def main():
    base_url = "http://127.0.0.1:15984"
    db_url = base_url + "/fdb-test"
    S.delete(db_url)

    r = S.get(base_url + "/_all_dbs")
    assert r.status_code == 200
    assert "fdb-test" not in r.json()

    r = S.put(db_url)
    assert r.status_code == 201

    r = S.get(base_url + "/_all_dbs")
    assert r.status_code == 200
    assert "fdb-test" in r.json()

    r = S.get(db_url)
    assert r.status_code == 200
    info = r.json()
    assert info["doc_count"] == 0
    assert info["doc_del_count"] == 0
    assert info["sizes"]["external"] == 2
    assert info["update_seq"] == ("0" * 20)

    r = S.delete(db_url)
    assert r.status_code == 200
    r = S.put(db_url)
    assert r.status_code == 201

    r = S.put(db_url + "/foo", data = "{}")
    print r.text
    assert r.status_code == 201
    assert "rev" in r.json()

    r = S.get(db_url + "/foo")
    print r.text
    assert r.status_code == 200
    assert "_rev" in r.json()

    r = S.get(db_url + "/_all_docs")
    print r.text
    assert r.status_code == 200
    assert "rows" in r.json()
    ids = [r["key"] for r in r.json()["rows"]]
    assert "foo" in ids

    r = S.get(db_url + "/_changes")
    print r.text
    assert r.status_code == 200
    assert "results" in r.json()
    ids = [r["id"] for r in r.json()["results"]]
    assert "foo" in ids

if __name__ == "__main__":
    main()
