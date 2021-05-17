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
USAGE: ERLFMT_PATH=<path_to_erlfmt> python3 dev/format-all.py
"""

import os
import pathlib
import re
import subprocess


def get_hashes():
    hashes = {}
    for item in subprocess.run(["git", "ls-files"], encoding="utf-8",
                               capture_output=True).stdout.split("\n"):
        item_path = pathlib.Path(item)
        if item_path.suffix != '.erl':
            continue
        result = re.search(r'([^/]+?)/src/([^/]+?).erl', item)
        if result is not None:
            beam_path = f"{result.group(1)}/ebin/{result.group(2)}.beam"
            hashes[item] = subprocess.run(["md5sum", beam_path], encoding="utf-8",
                                          capture_output=True, ).stdout
        else:
            pass
            # command = ["erl",
            #            "-eval",
            #            "{ok, _, Binary} = compile:file(\"" + item + "\", [binary])," +
            #            "erlang:display(crypto:hash(md5, Binary)), halt().",
            #            "-noshell"]
            # hashes[item] = subprocess.run(command, encoding="utf-8", capture_output=True).stdout
    return hashes


if __name__ == "__main__":
    print("Cleaning...")
    subprocess.run(["make", 'clean'], encoding="utf-8", stdout=subprocess.PIPE)
    print("Compiling...")
    subprocess.run(["bin/rebar", 'compile'], encoding="utf-8",
                   stdout=subprocess.PIPE, env={'ERL_OPTS': 'no_line_info'})
    os.chdir('src')
    print("Getting previous hashes...")
    prev = get_hashes()
    for key in prev.keys():
        subprocess.run([os.environ["ERLFMT_PATH"], '-w', key],
                       encoding="utf-8", stdout=subprocess.PIPE)
    os.chdir('..')
    subprocess.run(["bin/rebar", 'compile'], encoding="utf-8",
                   stdout=subprocess.PIPE, env={'ERL_OPTS': 'no_line_info'})
    os.chdir('src')
    print("Getting post hashes...")
    post = get_hashes()
    if prev == post:
        print("Hashes match")
    else:
        print("Hash mismatch")
        print("Diff: ", set(prev.items()) ^ set(post.items()))
