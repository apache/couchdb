#!/usr/bin/env python3
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.


import datetime
import glob
import json
import os
import tarfile
import time

import requests

COUCH_URL = "https://logs.couchdb.org/ci_errorlogs"
TARFILE = "couchlog.tar.gz"


def _tojson(req):
    """Support requests v0.x as well as 1.x+"""
    if requests.__version__[0] == "0":
        return json.loads(req.content)
    return req.json()


def collect_logfiles():
    """ Find and tarball all logfiles """
    tb = tarfile.open(name=TARFILE, mode="w:gz")
    # Test results
    for log in glob.glob("test-results.log"):
        tb.add(log)
    # EUnit
    for log in glob.glob("src/*/.eunit/couch.log"):
        tb.add(log)
    # JS harness
    for log in glob.glob("dev/logs/node1.log"):
        tb.add(log)
    # couchjs OS process IO logs
    for log in glob.glob("/tmp/couchjslogs/*"):
        tb.add(log)
    tb.close()


def build_ci_doc():
    """ Build a metadata document with relevant detail from CI env """
    doc = {}
    if "TRAVIS" in os.environ:
        doc["builder"] = "travis"
        doc["build_id"] = os.environ["TRAVIS_JOB_ID"]
        doc["erlang"] = os.environ["TRAVIS_OTP_RELEASE"]
        doc["url"] = (
            "https://travis-ci.org/apache/couchdb/jobs/" + os.environ["TRAVIS_JOB_ID"]
        )
        doc["branch"] = os.environ["TRAVIS_BRANCH"]
        doc["commit"] = os.environ["TRAVIS_COMMIT"]
        doc["repo"] = "https://github.com/" + os.environ["TRAVIS_REPO_SLUG"]
    elif "JENKINS_URL" in os.environ:
        doc["builder"] = "jenk-ins"
        doc["build_id"] = os.environ["BUILD_NUMBER"]
        doc["url"] = os.environ["BUILD_URL"]
        doc["branch"] = os.environ["BRANCH_NAME"]
        doc["repo"] = "https://github.com/apache/couchdb"
    else:
        doc["builder"] = "manual"
        # TODO: shell out to get correct repo, commit, branch info?
        doc["repo"] = "https://github.com/apache/couchdb"
        doc["build_id"] = str(time.time())

    # shorten doc id
    repo = doc["repo"].split("/")[-1]
    repo = repo.replace(".git", "")

    doc["_id"] = (
        doc["builder"]
        + "-"
        + repo
        + "-"
        + doc["build_id"]
        + "-"
        + datetime.datetime.utcnow().isoformat()
    )

    return doc


def upload_logs():
    try:
        lp = os.environ["COUCHAUTH"].split(":")
    except KeyError as e:
        print("ERROR: COUCHAUTH credentials unavailable! " "Unable to upload logfiles.")
        exit(1)

    creds = (lp[0], lp[1])
    doc = build_ci_doc()
    req = requests.post(
        COUCH_URL,
        data=json.dumps(doc),
        auth=creds,
        headers={"Content-type": "application/json"},
    )
    req.raise_for_status()
    req = _tojson(req)
    with open(TARFILE, "rb") as f:
        # ancient versions of requests break if data is iterable
        fdata = f.read()
        req2 = requests.put(
            COUCH_URL + "/" + doc["_id"] + "/" + TARFILE,
            headers={"Content-type": "application/x-gtar"},
            auth=creds,
            params={"rev": req["rev"]},
            data=fdata,
        )
    req2.raise_for_status()
    return req2


def main():
    """ Find latest logfile and upload to Couch logfile db. """
    print("Uploading logfiles...")
    collect_logfiles()
    req = upload_logs()
    print(req.url.split("?")[0])
    print(req.content)
    print("Upload complete!")


if __name__ == "__main__":
    main()
