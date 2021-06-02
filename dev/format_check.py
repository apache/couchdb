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
USAGE: ERLFMT_PATH=<path_to_erlfmt> python3 dev/format_check.py
"""

import os
import subprocess
import sys

from format_lib import get_source_paths

FILTERED_LINES = [
    "Checking formatting...",
    "[warn] Code style issues found in the above file(s). Forgot to run erlfmt?",
    "",
]

if __name__ == "__main__":
    os.chdir("src")
    failed_checks = 0
    for item in get_source_paths():
        if item["is_source_path"]:
            run_result = subprocess.run(
                [
                    os.environ["ERLFMT_PATH"],
                    "-c",
                    "--verbose",
                    # We have some long lines and erlfmt doesn't forcefully wrap
                    # them all. We should decrease this over time
                    "--print-width=167",
                    item["raw_path"],
                ],
                encoding="utf-8",
                capture_output=True,
            )
            if run_result.returncode != 0:
                # erlfmt sometimes returns a non-zero status code with no
                # actual errors. This is a workaround
                stderr_lines = [
                    line
                    for line in run_result.stderr.split("\n")
                    if line not in FILTERED_LINES
                    and not line.startswith("Formatting ")
                    and not line.startswith("[warn] ")
                ]
                if len(stderr_lines) > 0:
                    print("\n".join(stderr_lines), file=sys.stderr)
                    failed_checks += 1
    os.chdir("..")
    sys.exit(failed_checks)
