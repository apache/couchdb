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

"""Erlang formatter lib for CouchDB
Warning: this file is not meant to be executed manually
"""

import pathlib
import subprocess


def get_erlang_version():
    args = [
        "erl",
        "-eval",
        "io:put_chars(erlang:system_info(otp_release)), halt().",
        "-noshell",
    ]
    res = subprocess.run(args, stdout=subprocess.PIPE, check=True)
    str_version = res.stdout.decode("utf-8").strip().strip('"')
    return int(str_version)


# Generate source paths as "directory/*.erl" wildcard patterns
# those can be directly consumed by erlfmt and processed in parallel
#
def get_source_paths():
    curdir = None
    for item in (
        subprocess.run(
            ["git", "ls-files", "--", "*.erl"],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        .stdout.decode("utf-8")
        .split("\n")
    ):
        path = pathlib.Path(item)
        if path.parent != curdir:
            yield str(path.parent.joinpath("*.erl").as_posix())
            curdir = path.parent
    if curdir is not None:
        yield str(curdir.joinpath("*.erl").as_posix())
