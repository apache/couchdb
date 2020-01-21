#!/usr/bin/env python2.7

import argparse
import glob
import json
import os
import re
import xml.dom.minidom as md


TEST_COLLECTIONS = {
    "EUnit": "src/**/.eunit/*.xml",
    "EXUnit": "_build/integration/lib/couchdbtest/*.xml",
    "Mango": "src/mango/*.xml",
    "JavaScript": "test/javascript/*.xml",
}


def _attrs(elem):
    ret = {}
    for (k, v) in elem.attributes.items():
        ret[k.lower()] = v
    return ret


def _text(elem):
    rc = []
    for node in elem.childNodes:
        if node.nodeType == node.TEXT_NODE:
            rc.append(node.data)
        else:
            rc.append(self._text(node))
    return "".join(rc)


class TestCase(object):
    def __init__(self, elem):
        self.elem = elem

        attrs = _attrs(elem)

        self.name = self._name(attrs)
        self.time = float(attrs["time"])

        self.failure = False
        self._check_failure(elem, attrs)

        self.error = False
        self._check_error(elem, attrs)

        self.skipped = False
        self._check_skipped(elem, attrs)

    def _check_failure(self, elem, attrs):
        failures = elem.getElementsByTagName("failure")
        if not failures:
            return

        self.failure = True
        self.failure_msg = _text(failures[0]).strip()

    def _check_error(self, elem, attrs):
        errors = elem.getElementsByTagName("error")
        if not errors:
            return

        self.error = True
        self.error_msg = _text(errors[0]).strip()

    def _check_skipped(self, elem, attrs):
        skipped = elem.getElementsByTagName("skipped")
        if not skipped:
            return

        attrs = _attrs(skipped[0])
        self.skipped = True
        self.skipped_msg = attrs.get("message", attrs.get("type", "<unknown>"))

    def _name(self, attrs):
        klass = attrs.get("classname", "")
        if klass.startswith("Elixir."):
            klass = klass[len("Elixir.") :]
        if klass:
            return "%s - %s" % (klass, attrs["name"])
        return attrs["name"]


class TestSuite(object):
    SUITE_NAME_PATTERNS = [re.compile("module '([^']+)'"), re.compile("Elixir\.(.+)")]

    def __init__(self, elem):
        self.elem = elem

        attrs = _attrs(elem)

        self.name = self._name(attrs)

        self.time = 0.0
        if "time" in attrs:
            self.time = float(attrs["time"])

        self.num_tests = int(attrs["tests"])
        self.num_failures = int(attrs["failures"])
        self.num_errors = int(attrs["errors"])
        self.num_skipped = 0

        self.tests = []
        self.test_time = 0.0

        for t_elem in elem.getElementsByTagName("testcase"):
            self.tests.append(TestCase(t_elem))
            self.test_time += self.tests[-1].time
            if self.tests[-1].skipped:
                self.num_skipped += 1

        if self.time == 0.0 and self.test_time > 0.0:
            self.time = self.test_time

    def _name(self, attrs):
        raw_name = attrs["name"]
        for p in self.SUITE_NAME_PATTERNS:
            match = p.match(raw_name)
            if match:
                return match.group(1)
        return raw_name


class TestCollection(object):
    def __init__(self, name, pattern):
        self.name = name
        self.pattern = pattern
        self.suites = []
        self.bad_files = []

        for fname in glob.glob(pattern):
            self._load_file(fname)

    def _load_file(self, filename):
        try:
            dom = md.parse(filename)
        except:
            self.bad_files.append(filename)
            return
        for elem in dom.getElementsByTagName("testsuite"):
            self.suites.append(TestSuite(elem))


def parse_args():
    parser = argparse.ArgumentParser(description="Show test result summaries")
    parser.add_argument(
        "--ignore-failures",
        action="store_true",
        default=False,
        help="Don't display test failures",
    )
    parser.add_argument(
        "--ignore-errors",
        action="store_true",
        default=False,
        help="Don't display test errors",
    )
    parser.add_argument(
        "--ignore-skipped",
        action="store_true",
        default=False,
        help="Don't display skipped tests",
    )
    parser.add_argument(
        "--all", type=int, default=0, help="Number of rows to show for all groups"
    )
    parser.add_argument(
        "--collection",
        action="append",
        default=[],
        help="Which collection to display. May be repeated.",
    )
    parser.add_argument(
        "--suites", type=int, default=0, help="Number of suites to show"
    )
    parser.add_argument("--tests", type=int, default=0, help="Number of tests to show")
    parser.add_argument(
        "--sort",
        default="total",
        choices=["test", "fixture", "total"],
        help="Timing column to sort on",
    )
    return parser.parse_args()


def display_failures(collections):
    failures = []
    for collection in collections:
        for suite in collection.suites:
            for test in suite.tests:
                if not test.failure:
                    continue
                failures.append((test.name, test.failure_msg))

    if not len(failures):
        return
    print "Failures"
    print "========"
    print
    for failure in failures:
        print failure[0]
        print "-" * len(failure[0])
        print
        print failure[1]
        print


def display_errors(collections):
    errors = []
    for collection in collections:
        for suite in collection.suites:
            for test in suite.tests:
                if not test.error:
                    continue
                errors.append((test.name, test.error_msg))

    if not len(errors):
        return
    print "Errors"
    print "======"
    print
    for error in errors:
        print error[0]
        print "-" * len(error[0])
        print
        print error[1]
        print


def display_skipped(collections):
    skipped = []
    for collection in collections:
        for suite in collection.suites:
            for test in suite.tests:
                if not test.skipped:
                    continue
                name = "%s - %s - %s" % (collection.name, suite.name, test.name)
                skipped.append((name, test.skipped_msg))
    if not skipped:
        return
    print "Skipped"
    print "======="
    print
    for row in sorted(skipped):
        print "  %s: %s" % row
    print


def display_table(table):
    for ridx, row in enumerate(table):
        new_row = []
        for col in row:
            if isinstance(col, float):
                new_row.append("%4.1fs" % col)
            elif isinstance(col, int):
                new_row.append("%d" % col)
            else:
                new_row.append(col)
        table[ridx] = new_row
    for row in table:
        fmt = " ".join(["%10s"] * len(row))
        print fmt % tuple(row)


def display_collections(collections, sort):
    rows = []
    for collection in collections:
        total_time = 0.0
        test_time = 0.0
        num_tests = 0
        num_failures = 0
        num_errors = 0
        num_skipped = 0
        for suite in collection.suites:
            total_time += suite.time
            test_time += suite.test_time
            num_tests += suite.num_tests
            num_failures += suite.num_failures
            num_errors += suite.num_errors
            num_skipped += suite.num_skipped
        cols = (
            total_time,
            max(0.0, total_time - test_time),
            test_time,
            num_tests,
            num_failures,
            num_errors,
            num_skipped,
            collection.name + "        ",
        )
        rows.append(cols)

    scol = 0
    if sort == "fixture":
        scol = 1
    elif sort == "test":
        scol = 2

    def skey(row):
        return (-1.0 * row[scol], row[-1])

    rows.sort(key=skey)

    print "Collections"
    print "==========="
    print
    headers = ["Total", "Fixture", "Test", "Count", "Failed", "Errors", "Skipped"]
    display_table([headers] + rows)
    print


def display_suites(collections, count, sort):
    rows = []
    for collection in collections:
        for suite in collection.suites:
            cols = [
                suite.time,
                max(0.0, suite.time - suite.test_time),
                suite.test_time,
                suite.num_tests,
                suite.num_failures,
                suite.num_errors,
                suite.num_skipped,
                collection.name + " - " + suite.name,
            ]
            rows.append(cols)

    scol = 0
    if sort == "fixture":
        scol = 1
    elif sort == "test":
        scol = 2

    def skey(row):
        return (-1.0 * row[scol], row[-1])

    rows.sort(key=skey)

    rows = rows[:count]

    print "Suites"
    print "======"
    print
    headers = ["Total", "Fixture", "Test", "Count", "Failed", "Errors", "Skipped"]
    display_table([headers] + rows)
    print


def display_tests(collections, count):
    rows = []
    for collection in collections:
        for suite in collection.suites:
            for test in suite.tests:
                if test.failure or test.error or test.skipped:
                    continue
                fmt = "%s - %s - %s"
                display = fmt % (collection.name, suite.name, test.name)
                rows.append((test.time, display))

    def skey(row):
        return (-1.0 * row[0], row[-1])

    rows.sort(key=skey)
    rows = rows[:count]

    print "Tests"
    print "====="
    print
    display_table(rows)
    print


def main():
    args = parse_args()

    if not args.collection:
        args.collection = ["eunit", "exunit", "mango", "javascript"]

    collections = []
    for (name, pattern) in TEST_COLLECTIONS.items():
        if name.lower() not in args.collection:
            continue
        collections.append(TestCollection(name, pattern))

    if not args.ignore_failures:
        display_failures(collections)

    if not args.ignore_errors:
        display_errors(collections)

    if not args.ignore_skipped:
        display_skipped(collections)

    display_collections(collections, args.sort)

    if args.all > 0:
        args.suites = args.all
        args.tests = args.all

    if args.suites > 0:
        display_suites(collections, args.suites, args.sort)

    if args.tests > 0:
        display_tests(collections, args.tests)


if __name__ == "__main__":
    main()
