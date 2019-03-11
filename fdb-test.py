#!/usr/bin/env python

import requests

S = requests.session()
S.auth = ("adm", "pass")
S.headers["Content-Type"] = "application/json"


def main():
    db_url = "http://127.0.0.1:15984/fdb-test"
    S.delete(db_url)

    r = S.put(db_url)
    assert r.status_code == 201

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


if __name__ == "__main__":
    main()
