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
import sys
import subprocess

from format_lib import get_source_paths, get_erlang_version

if __name__ == "__main__":
    if get_erlang_version() < 21:
        print("Erlang version is < 21. Skipping format check")
        sys.exit(0)

    for path in get_source_paths():
        subprocess.run(
            [os.environ["ERLFMT_PATH"], "-w", path],
            stdout=subprocess.PIPE,
        )
