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

"""Erlang formatter for CouchDB
Warning: this file needs to run from the CouchDB repo root.
USAGE: ERLFMT_PATH=<path_to_erlfmt> python3 dev/format_all.py
"""

import os
import subprocess

from format_lib import get_source_paths


def get_hashes():
    hashes = {}
    for item in get_source_paths():
        if item["is_source_path"]:
            beam_path = f"{item['dirname']}/ebin/{item['filename']}.beam"
            hashes[item["raw_path"]] = subprocess.run(
                ["md5sum", beam_path], encoding="utf-8", capture_output=True
            ).stdout
        else:
            # command = ["erl",
            #            "-eval",
            #            "{ok, _, Binary} = compile:file(\"" + item['raw_path'] +
            #            "\", [binary, no_line_info, deterministic])," +
            #            "erlang:display(crypto:hash(md5, Binary)), halt().",
            #            "-noshell"]
            # hashes[item['raw_path']] = subprocess.run(command, encoding="utf-8",
            #                                           capture_output=True).stdout
            pass
    return hashes


if __name__ == "__main__":
    print("Cleaning...")
    subprocess.run(["make", "clean"], encoding="utf-8", stdout=subprocess.PIPE)
    print("Compiling...")
    subprocess.run(
        ["bin/rebar", "compile"],
        encoding="utf-8",
        stdout=subprocess.PIPE,
        env={"ERL_OPTS": "no_line_info"},
    )
    os.chdir("src")
    print("Getting previous hashes...")
    prev = get_hashes()
    for key in prev.keys():
        subprocess.run(
            [os.environ["ERLFMT_PATH"], "-w", key],
            encoding="utf-8",
            stdout=subprocess.PIPE,
        )
    os.chdir("..")
    subprocess.run(
        ["bin/rebar", "compile"],
        encoding="utf-8",
        stdout=subprocess.PIPE,
        env={"ERL_OPTS": "no_line_info"},
    )
    os.chdir("src")
    print("Getting post hashes...")
    post = get_hashes()
    if prev == post:
        print("Hashes match")
    else:
        print("Hash mismatch")
        print("Diff: ", set(prev.items()) ^ set(post.items()))
