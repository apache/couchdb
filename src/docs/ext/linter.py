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


# This is very-very-very simple linter made in one evening without thoughts of
# making something great, but just a thing that works.

import os
import re


RULES = []
HAS_ERRORS = False
IGNORE_ERROR = False


def error_report(file, line, msg, _state=[]):
    global HAS_ERRORS, IGNORE_ERROR
    if IGNORE_ERROR:
        return
    if _state and _state[0] == file.name:
        pass
    else:
        if _state:
            _state[0] = file.name
        else:
            _state.append(file.name)
        sys.stderr.write(file.name + "\n")
    sys.stderr.write(" ".join(["  line", str(line), ":", msg]) + "\n")
    HAS_ERRORS = True


def register_rule(func):
    RULES.append(func)
    return func


def main(path):
    for file in iter_rst_files(os.path.abspath(path)):
        validate(file)
    sys.exit(HAS_ERRORS)


def iter_rst_files(path):
    if os.path.isfile(path):
        with open(path) as f:
            yield f
        return
    for root, dirs, files in os.walk(path):
        for file in files:
            if file.endswith(".rst"):
                with open(os.path.join(root, file), "rb") as f:
                    yield f


def validate(file):
    global IGNORE_ERROR
    IGNORE_ERROR = False
    rules = [rule(file) for rule in RULES]
    for rule in rules:
        for _ in rule:
            # initialize coroutine
            break
    while True:
        line = file.readline().decode("utf-8")
        exhausted = []
        for idx, rule in enumerate(rules):
            try:
                error = rule.send(line)
            except StopIteration:
                exhausted.append(rule)
            else:
                if error:
                    error_report(*error)

        # not very optimal, but I'm lazy to figure anything better
        for rule in exhausted:
            rules.pop(rules.index(rule))

        if not line:
            break


@register_rule
def silent_scream(file):
    """Sometimes we must accept presence of some errors by some relevant
    reasons. Here we're doing that."""
    global IGNORE_ERROR
    counter = 0
    while True:
        line = yield None
        if not line:
            break

        if counter:
            IGNORE_ERROR = True
            counter -= 1
        else:
            IGNORE_ERROR = False

        match = re.match("\s*.. lint: ignore errors for the next (\d+) line?", line)
        if match:
            # +1 for empty line right after comment
            counter = int(match.group(1)) + 1


@register_rule
def license_adviser(file):
    """Each source file must include ASF license header."""
    header = iter(
        """
.. Licensed under the Apache License, Version 2.0 (the "License"); you may not
.. use this file except in compliance with the License. You may obtain a copy of
.. the License at
..
..   http://www.apache.org/licenses/LICENSE-2.0
..
.. Unless required by applicable law or agreed to in writing, software
.. distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
.. WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
.. License for the specific language governing permissions and limitations under
.. the License.

""".lstrip().splitlines(
            False
        )
    )
    error = None
    for n, hline in enumerate(header):
        fline = yield error
        error = None
        if hline != fline.strip("\r\n"):
            error = (
                file,
                n + 1,
                "bad ASF license header\n"
                "  expected: {0}\n"
                "  found:    {1}".format(hline, fline.strip()),
            )


@register_rule
def whitespace_committee(file):
    """Whitespace committee takes care about whitespace (surprise!) characters
    in files. The documentation style guide says:

    - There should be no trailing white space;
    - More than one empty line is not allowed and there shouldn't be such
      at the end of file;
    - The last line should ends with newline character

    Additionally it alerts about tabs if they were used instead of spaces.

    TODO: check for indentation
    """
    error = prev = None
    n = 0
    while True:
        line = yield error
        error = None
        if not line:
            break
        n += 1

        # Check for trailing whitespace
        if line.strip("\r\n").endswith(" "):
            error = (file, n + 1, "trailing whitespace detected!\n" "{0}".format(line))

        # Check for continuous empty lines
        if prev is not None:
            if prev.strip() == line.strip() == "":
                error = (file, n + 1, "too many empty lines")

        # Nobody loves tabs-spaces cocktail, we prefer spaces
        if "\t" in line:
            error = (file, n + 1, "no tabs please")

        prev = line

    # Accidentally empty file committed?
    if prev is None:
        error = (file, 0, "oh no! file seems empty!")

    # Empty last lines not welcome
    elif prev.strip() == "":
        error = (file, n + 1, "no empty last lines please")

    # Last line should ends with newline character
    elif not prev.endswith("\n"):
        error = (file, n + 1, "last line should ends with newline character")

    yield error
    return


@register_rule
def line_length_checker(file):
    """Use a modern max line length of 90 chars, as recommended by things like
    https://github.com/ambv/black and https://youtu.be/wf-BqAjZb8M?t=260 .
    """
    in_code_block = False
    seen_emptyline = False
    n = 0
    error = None
    while True:
        line = yield error
        error = None
        if not line:
            break
        n += 1
        line = line.rstrip()

        # We have to ignore stuff in code blocks since it's hard to keep it
        # within 90 chars wide box.
        if line.strip().startswith(".. code") or line.endswith("::"):
            in_code_block = True
            continue

        # Check for line length unless we're not in code block
        if len(line) > 90 and not in_code_block:
            if line.startswith(".."):
                # Ignore long lines with external links
                continue

            if line.endswith(">`_"):
                # Ignore long lines because of URLs
                # TODO: need to be more smart here
                continue

            error = (
                file,
                n + 1,
                "too long ({0} > 90) line\n{1}\n" "".format(len(line), line),
            )

        # Empty lines are acts as separators for code block content
        elif not line:
            seen_emptyline = True

        # So if we saw an empty line and here goes content without indentation,
        # so it mostly have to sign about the end of our code block
        # (if it ever occurs)
        elif seen_emptyline and line and not line.startswith(" "):
            seen_emptyline = False
            in_code_block = False

        else:
            seen_emptyline = False


@register_rule
def my_lovely_hat(file):
    """Everyone loves to wear a nice hat on they head, so articles does too."""
    error = None
    n = 0
    while True:
        line = yield error
        error = None
        if not line:
            break

        line = line.strip()

        if not line:
            continue

        if line.startswith(".."):
            continue

        if set(line) < set(["#", "-", "=", "*"]):
            break
        else:
            lines = [line, "\n", (yield None), (yield None)]
            yield (file, n + 1, "bad title header:\n" "{}".format("".join(lines)))
            return


if __name__ == "__main__":
    import sys

    if len(sys.argv) == 1:
        sys.stderr.write("Argument with the target path is missed")
        sys.exit(2)
    main(sys.argv[1])
