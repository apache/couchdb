---
name: Formal RFC
about: Submit a formal Request For Comments for consideration by the team.
title: 'Use ExUnit testing framework for unit testing'
labels: rfc, discussion
assignees: ''

---

# Introduction

With the upgrade of supported Erlang version and introduction of Elixir into our
integration test suite we have an opportunity to replace currently used eunit
(for new tests only) with Elixir based ExUnit. 

## Abstract

Eunit testing framework has a number of issues which makes it very hard to use.
We already use alternative testing framework called ExUnit for integration tests.
The proposal is to extend the use of ExUnit to CouchDB unit tests as well.

## Requirements Language

[NOTE]: # ( Do not alter the section below. Follow its instructions. )

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and "OPTIONAL" in this
document are to be interpreted as described in
[RFC 2119](https://www.rfc-editor.org/rfc/rfc2119.txt).

## Terminology

[TIP]:  # ( Provide a list of any unique terms or acronyms, and their definitions here.)

---

# Detailed Description

The eunit testing framework is very hard to maintain. In particular, it has the
following problems:
- the process structure is designed in such a way that failure in setup or teardown
  of one test affects the execution environment of subsequent tests. Which makes it
  really hard to locate the place where the problem is coming from.
- inline test in the same module as the functions it tests might be skipped
- incorrect usage of ?assert vs ?_assert is not detectable since it makes tests pass
- there is a weird (and hard to debug) interaction when used in combination with meck
   - https://github.com/eproxus/meck/issues/133#issuecomment-113189678
   - https://github.com/eproxus/meck/issues/61
   - meck:unload() must be used instead of meck:unload(Module)
- teardown is not always run, which affects all subsequent tests
- grouping of tests is tricky
- it is hard to group tests so individual tests have meaningful descriptions
- eunit implementation of `{with, Tests}` doesn't detect test name correctly
- it is hard to skip certain tests when needed

ExUnit shouldn't have these problems:
- on_exit function is reliable in ExUnit
- it is easy to group tests using `describe` directive
- code-generation is trivial, which makes it is possible to generate tests from formal spec (if/when we have one)

# Advantages and Disadvantages

## Advantages

- Modern testing framework
- Easy codegeneration of tests from formal spec
- Reliability of teardown functions
- Increased productivity due to smart test scheduling (run only failing tests)
- Unified style enforced by code linter
- Possibly more contributions from Elixir community
- We already use ExUnit for integration tests
- Support for test tags which could help us to introduce schedule of tests ([see #1885](https://github.com/apache/couchdb/issues/1885)).
  We could run tests in the optimal order: 
    - recently modified
    - couch_db API based
    - fabric API based
    - http API based
    - performance tests
    - property based tests

## Disadvantages

- New language & tooling to learn
- We make Elixir required dependency (currently it is somewhat optional)

# Key Changes

- move all eunit tests from `<app>/test/*.erl` into `<app>/test/eunit/*.erl`
- add `make exunit` target to Makefile
- move `.credo.exs` (linter configuration) into root of a project
- create `<app>/test/exunit/` directory to hold new test suites
- add different test helpers under `test/elixir/lib`
- add `mix.exs` into root of the project

## Applications and Modules affected

There is a possibility that we would need to modify content of `test/elixir/lib` 
to have similar experience in both integration and unit test framework.

## HTTP API additions

N/A

## HTTP API deprecations

N/A

# Security Considerations

Production code is not updated. Therefore there is no security risk.

# References

- [Discussion on mailing list](https://lists.apache.org/thread.html/f842ca637f7cb06b34af699a793cab0a534e65970172e8117bf0b228@%3Cdev.couchdb.apache.org%3E)

# Acknowledgements

Thanks to everyone who participated on the mailing list discussion

- @davisp
- @wohali
- @garrensmith