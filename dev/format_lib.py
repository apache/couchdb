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
import re
import subprocess


def get_source_paths():
    for item in subprocess.run(
        ["git", "ls-files"], encoding="utf-8", capture_output=True
    ).stdout.split("\n"):
        item_path = pathlib.Path(item)
        if item_path.suffix != ".erl":
            continue

        regex_result = re.search(r"([^/]+?)/src/([^/]+?).erl", item)
        result_dict = {
            "raw_path": item,
            "item_path": item_path,
            "is_source_path": regex_result is not None,
        }
        if result_dict["is_source_path"]:
            result_dict.update(
                {"dirname": regex_result.group(1), "filename": regex_result.group(2)}
            )
        yield result_dict
