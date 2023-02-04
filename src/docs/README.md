# CouchDB Documentation

This directory contains the Sphinx source for Apache CouchDB's documentation.
You can view the latest rendered build of this content at:

    http://docs.couchdb.org/en/latest

# Building the docs

Install Python 3 and pip. Then:

```sh
$ python3 -m venv .venv
$ source .venv/bin/activate
$ pip3 install -r requirements.txt
$ make html # builds the docs
$ make check # syntax checks the docs
```

# Feedback, Issues, Contributing

General feedback is welcome at our [user][1] or [developer][2] mailing lists.

Apache CouchDB has a [CONTRIBUTING][3] file with details on how to get started
with issue reporting or contributing to the upkeep of this project.

[1]: http://mail-archives.apache.org/mod_mbox/couchdb-user/
[2]: http://mail-archives.apache.org/mod_mbox/couchdb-dev/
[3]: https://github.com/apache/couchdb/blob/main/CONTRIBUTING.md
