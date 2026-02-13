---
name: Formal RFC
about: Submit a formal Request For Comments for consideration by the team.
title: ''
labels: rfc, discussion
assignees: ''

---

[NOTE]: # ( ^^ Provide a general summary of the RFC in the title above. ^^ )

# Introduction

## Abstract

[NOTE]: # ( Provide a 1-to-3 paragraph overview of the requested change. )
[NOTE]: # ( Describe what problem you are solving, and the general approach. )

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

This document specifies a system of declarative document validation and
authorization for CouchDB. It lets users write rules for validating document
updates using expressions that can be evaluated inside the main CouchDB process,
instead of having to invoke JavaScript code and incurring the overhead of
round-tripping documents to the external JavaScript engine.


## Design documents

Users encode validation rules by storing a design document with the following
fields:

- `language`: must have the value `"query"`
- `validate_doc_update`: contains an _Extended Mango_ expression that encodes
  the desired validation rules
- `defs`: (optional) a set of named Extended Mango expressions that may be
  referenced from the `validate_doc_update` expression, as a way to encode
  reusable functions


## Handling write requests

When a document is updated, the `validate_doc_update` (VDU) fields of all the
design docs in the database are evaluated, and all of them must return a
successful response in order for the update to be accepted. For existing VDUs
written in JavaScript, those continue to be evaluated using the JavaScript
engine. For VDUs in docs with the field `"language": "query"`, the VDU is
evaluated using the functions in the `mango` application, particularly the
`mango_selector` module. This module will need additional functionality to
handle Extended Mango expressions as described below.


### Input to declarative VDUs

JavaScript-based VDUs are functions that accept four arguments: the old and new
versions of the document, the user context, and the database security object.
The input to a declarative VDU is a virtual JSON document with the following
top-level properties:

- `$newDoc`: the body of the new version of the document that the client is
  attempting to write
- `$oldDoc`: the body of the previous leaf revision of the document that the new
  revision would follow on from
- `$userCtx`: the user context containing the current user's `name` and an array
  of `roles` the user possesses
- `$secObj`: the database security object, containing arrays of users and roles
  with admin access, and users and roles with member access

Example of a user context:

    {
      "db": "movies",
      "name": "Alice",
      "roles": ["_admin"]
    }

Example of a security object:

    {
      "admins": {
        "names": ["Bob"],
        "roles": []
      },
      "members": {
        "names": ["Mike", "Alice"],
        "roles": []
      }
    }

To evaluate a declarative VDU against this virtual JSON document, evaluate the
Extended Mango expression in the `validate_doc_update` field. This will produce
a list of _failures_ that describe the ways in which the input does not match
the selector. Evaluation is considered successful if this list is empty.

If the expression produces a non-empty list, then no further expressions from
other design docs are evaluated, the write is rejected, and a representation of
the failures is returned to the client. If the expression produces an empty
list, then expressions from other design docs are evaluated. If none of the
`validate_doc_update` fields in any design doc produces a failure, the write is
accepted.


### Responses to write requests

If the selector expression in the `validate_doc_update` field returns an empty
list of failures, then the write is accepted and proceeds as normal, leading to
a 201 or 202 response.

If any of the selectors fails, then either its list of failures or a custom
error response is returned to the caller, with a 401 or 403 status code as
indicated by the selector itself. For example, imagine a design doc contains the
following:

    "validate_doc_update": {
      "$all": [
        {
          "$userCtx.roles": { "$all": ["_admin"] },
          "$error": "unauthorized"
        },
        {
          "$newDoc.type": { "$in": ["movie", "director"] },
          "$error": "forbidden"
        }
      ]
    }

To evaluate this expression, the following steps are performed:

- Check whether the user context's `roles` array contains the value `"_admin"`.
  If it does not, return a 401 response to the client.
- Check whether the new doc's `type` field has the value `"movie"` or
  `"director"`. If it does not, return a 403 response to the client.
- Otherwise, accept the write and return a 201 or 202.

The body of the response contains two fields:

- `error`: this is either `"unauthorized"` or `"forbidden"`
- `reason`: this contains either a custom error message, or the list of failures
  generated by the first non-matching selector.

If no custom `$reason` is set, then the `reason` field contains a list of
failures like so:

    {
      "error": "forbidden",
      "reason": {
        "failures": [
          {
            "path": ["$newDoc", "type"],
            "type": "in",
            "params": ["movie", "director"]
          }
        ]
      }
    }

This is consistent with the current working of JavaScript VDUs. Such functions
can call `throw({ forbidden: obj })` where `obj` is an object, and it will be
passed back to the client as JSON, i.e. it is already possible for user-defined
VDUs to generate responses like that above.

A custom error response can be generated by adding extra information to the
selector expressions; see "`$error` and `$reason`" below.

The intent of this interface is that each individual selector expression
produces a complete list of _all_ the ways in which the input did not match the
selector expression, so that the client can show all the validation errors to
the user in one go.


## Extended Mango

Declarative VDU functions are expressed in an extended variant of Mango. It
includes all the selector operators previously designed for use in queries and
filters, and a few additions that are particularly suited to the task of
defining VDUs. Some of these features _only_ make sense for VDUs and should only
be allowed in this context.


### Return values

Currently, the evaluation of a Mango selector by the `mango_selector:match`
function returns a boolean value to indicate whether or not the input value
matched the selector. When evaluating Extended Mango for VDUs, `match` should
instead return a list of _failures_, which are records that describe the ways in
which the input did not match. A failure has the following properties:

- `path`: an array of keys that give the path to the non-matching value, from
  the root of the input document, such that a client could locate the
  non-matching value by evaluating `path.reduce((val, key) => val[key], doc)`
- `type`: the name of the matching operator that failed, e.g. `"eq"`, `"in"`,
  `"type"`, etc.
- `params`: an array of any other values that the operator used to determine the
  match result. For most operators this would just be the expected value.

An example of a failure object:

    {
      "path": ["$secObj", "members", "names", 1],
      "type": "eq",
      "params": ["Alice"]
    }

A client should be able to construct useful user-facing error messages from the
information in these failure objects such that the user could correct any
mistakes in their input.

An Extended Mango expression is considered to match a given input if its
evaluation returns an empty list.

To produce the `path` field, the `match` function will need to track the path it
used to reach the current value. This can be done by adding a "match context" to
its list of parameters that tracks this information along with other things.
This will also be needed to handle negation and relative `$data` references.


### General evaluation

Since `match` currently just returns `true` or `false`, the `mango_selector`
module can implement certain operations using "short-circuit" semantics. For
example, `$and` can be implemented by checking each of its sub-selectors, and
returning `false` as soon as a single one of them returns `false`. Likewise
`$or` can return `true` as soon as single sub-selector returns `true`.

For VDUs, we want to return a complete list of match failures to the client, so
some compound operators must evaluate all their inputs completely, without
short-circuiting. Specifically:

- `$and` should evaluate all its sub-selectors and return the combined list of
  failures produced by any of them.
- `$or` may return an empty list as soon as any of its sub-selectors returns an
  empty list. If all its sub-selectors return non-empty failure lists, it should
  return a combined list of all failures.
- `$nor` should be translated as described under "Negation" below, before the
  expression is evaluated.
- `$allMatch` should evaluate all list items against the sub-selector and return
  a list of all failures produced by any of the items. It must only return an
  empty list of all items produce an empty list.
- `$elemMatch` may return an empty list as soon as an item is found that
  produces an empty list for the sub-selector. If no item does so, the combined
  list of failures from all items should be returned.

For normal object matchers, `{ "a": X, "b": Y }` should have the same behaviour
as `{ "$and": [{ "a": X }, { "b": Y }] }`. That is, all the fields in the object
should be checked, rather than returning `false` as soon as a single field does
not match, and all failures from all fields should be returned to the caller.


### `$if`/`$then`/`$else`

To produce better error messages for dependent validation rules, a new set of
conditional operators is added. The general form is:

    { "$if": A, "$then": B, "$else": C }

`A`, `B` and `C` are sub-selector expressions. Both the `$then` and `$else`
fields are optional. `$then` defaults to `NONE($then)`, a selector which always
fails with the message that `$then` is required. `$else` defaults to `ANY`, a
selector which always succeeds. (This definition may seem odd but is necessary
for these expressions to be automatically negated; see "Negation" below.)

To evaluate this operator for input `Doc`, perform these steps:

- If `match(A, Doc)` returns an empty list, return the result of `match(B, Doc)`
- Otherwise, return the result of `match(C, Doc)`

If these operators appear in a selector alongside other operators, the effect is
the same as if any other of combination of operators was used. That is, the
`$if`/`$then`/`$else` operators must succeed, and all other operators in the
selector must succeed, in order for the match to be considered successful. For
example:

    {
      "$gt": 0,
      "$if": { "$gt": 10 },
      "$then": { "$mod": [5, 0] }
    }

This matches inputs that are greater than 0, and if they are greater than 10
they must also be a multiple of 5.

These operators may be evaluated in normal query/filter Mango contexts by
translating `{ "$if": A, "$then": B, "$else": C }` to:

    {
      "$or": [
        { "$and": [A, B] },
        { "$and": [{ "$not": A }, C] }
      ]
    }

This translation should be applied before negations are normalised.


### `$data`

Some rules, especially those concerned with authorization, will need to compare
different fields within the input, particularly comparing the user context to
the security object and the new document. To enable this, Extended Mango
provides a way to reference data from elsewhere in the input.

The `$data` operator produces the value at the location indicated by its
argument. For example, to require that one of the user's roles must be in the
database's admins list, one would write:

    {
      "$userCtx.roles": {
        "$elemMatch": {
          "$in": { "$data": "$secObj.admins.roles" }
        }
      }
    }

The `$data` operator finds the value located at `$secObj.admins.roles` within
the input document, and provides it as the operand to the `$in` operator.

The path given to `$data` may begin with one or more dots to indicate a relative
path. If no dots appear at the start of the path, then the path is absolute, and
is resolved from the root of the input document. If one or more dots are
present, the path is resolved by walking that many levels "up" from the current
value before proceeding.

For example, to indicate that a field must contain an array of objects, where
each object has a `max` field that is greater than its `min` field, one can
write:

    {
      "$allMatch": {
        "max": { "$gt": { "$data": ".min" } }
      }
    }

The dot at the start of the path `.min` indicates that the path should be
resolved by starting at the object containing the `max` field currently being
checked, rather than starting at the root of the input. A path starting with two
dots would indicate starting with the current object's parent, three dots
indicates starting at the grandparent, and so on.

The rest of the path should contain one or more field identifiers separated by
single dots. Field identifiers can be non-empty strings that identify a property
in an object, or non-negative integers that identify elements in an array. The
path should be evaluated by performing an operation equivalent to the JavaScript
expression:

    path.split('.').reduce((val, field) => val[field], input)

`$data` references must only be allowed in positions where a literal value would
otherwise be expected. If they were allowed in places that expect selector
expressions, this would allow an input document to inject its own validation
logic. Specifically, `$data` is only allowed in these positions:

- As the operand to `$eq`, `$ne`, `$lt`, `$lte`, `$gt`, `$gte`
- As the entire array operand, or as an individual array element, for `$in`,
  `$nin`, `$all` or `$mod`

When used with a normal object field, it should be interpreted as an exact
equality constraint. i.e. `{ "a": { "$data": "b" } }` means `{ "a": { "$eq": {
"$data": "b" } } }`, otherwise this would allow operator injection.


### `$cat`

The `$cat` operator takes an array whose elememts are either literal strings, or
`$data` references, and produces the result of concatenating them after
resolving the `$data` references. For example:

    {
      "_id": { "$cat": ["org.couchdb.user:", { "$data": "name" }] }
    }

This means that the `_id` field must equal the `name` field with
`org.couchdb.user:` as a prefix.

`$cat` has the same restrictions on its use as the `$data` field, i.e. it may
only appear where a literal value is expected, and must not appear anywhere a
sub-selector expression is expected. When it appears as the matcher for an
object property, it should be understood as a strict equality constraint, i.e.
the above expression means the same as:

    {
      "_id": {
        "$eq": {
          "$cat": ["org.couchdb.user:", { "$data": "name" }]
        }
      }
    }


### `$ref`

Declarative VDUs can make use of reusable expressions when defining their logic.
Definitions of such expressions are placed in the `defs` field of the design
document, and then referenced within `validate_doc_update` expressions via the
`$ref` operator.

For example, to define an expression for matching even numbers, this structure
is placed in the root of the design document:

    "defs": {
      "even-number": {
        "$type": "number",
        "$mod": [2, 0]
      }
    }

Then, a field can be validated as an even number using this syntax:

    "some_field": { "$ref": "defs.even-number" }

The `$ref` operator takes a path with the same syntax as `$data`, but it is
resolved relative to the root of the design document.

The effect of using `$ref` and other operators in the same selector should be
the same as if the `$ref` expression where combined with the rest of the
selector via `$and`. For example, `{ "$ref": "devs.even-number", "$gt": 20 }`
should have the same effect as:

    {
      "$and": [
        { "$ref": "defs.even-number" },
        { "$gt": 20 }
      ]
    }

If the implementation wishes to inline `$ref` expressions before evaluation, it
should first translate any uses of `$ref` into the above form to avoid key
collisions when merging objects.

This operator may be used to define recursive structures. For example, if we had
a way of representing a tree of HTML nodes, where each node has a tag name, a
set of attributes, and a list of child nodes, we could validate it using this
definition:

    "defs": {
      "html-tree": {
        "tagName": { "$type": "string" },
        "attributes": { "$type": "object" },
        "children": {
          "$type": "array",
          "$allMatch": { "$ref": "defs.html-tree" }
        }
      }
    }

**To be decided**: we may wish to prohibit such recursion, as doing so would let
us inline any `$ref` references before evaluating an expression. This would
require detecting any sets of mutually recursive definitions in a design
document, and the complexity of doing this may not be worth it.


### Negation

To produce good failure messages, negation needs to be pushed to the "leaves" of
the expression tree. For example, when evaluating `{ "$not": { "$eq": 42 } }`
against the input `42`, the `$eq` produces an empty list, and `$not` then has to
"invent" a failure record to return to the caller. It can't meaningfully do this
without understanding the meaning of its sub-selector. If the selector is
instead translated to `{ "$ne": 42 }` then it can directly produce a meaningful
failure message.

Many Mango expressions can be translated in a way that pushes a `$not` operator
from the root of the expression towards the leaves. Specifically:

- `{ "$not": { "field": X } }` becomes `{ "field": { "$ne": X } }`
- `{ "$not": { "$eq": X } }` becomes `{ "$ne": X }` and vice versa
- `{ "$not": { "$in": X } }` becomes `{ "$nin": X }` and vice versa
- `{ "$not": { "$lt": X } }` becomes `{ "$gte": X }` and vice versa
- `{ "$not": { "$gt": X } }` becomes `{ "$lte": X }` and vice versa
- `{ "$not": { "$exists": X } }` becomes `{ "$exists": !X }`
- `{ "$not": { "$not": X } }` becomes `X`

- `{ "$not": { "$all": [A, B] } }` becomes `{ "$or": [{ "$not": A }, { "$not": B
  }] }`

- `{ "$not": { "$or": [A, B] } }` becomes `{ "$and": [{ "$not": A }, { "$not": B
  }] }`

- `{ "$nor": [A, B] }` becomes `{ "$and": [{ "$not": A }, { "$not": B }] }`

- `{ "$not": { "$nor": [A, B] } }` becomes `{ "$or": [A, B] }`

- `{ "$not": { "$allMatch": A } }` becomes `{ "$elemMatch": { "$not": A } }`

- `{ "$not": { "$elemMatch": A } }` becomes `{ "$allMatch": { "$not": A } }`

- `{ "$not": { "$if": A, "$then": B, "$else": C } }` becomes `{ "$if": A,
  "$then": { "$not": B }, "$else": { "$not": C } }`

- `{ "k": { "$not": NONE(k) } }` becomes `{ "k": ANY }`

- `{ "k": { "$not": ANY } }` becomes `{ "k": NONE(k) }`

Most of these translations are already implemented by the `norm_negations()`
function because this helps with identifying indexes that may be used for
queries. This function may prove useful for evaluating VDUs, but there are some
operators that do not have a built-in negation:

- `$type`
- `$size`
- `$mod`
- `$regex`
- `$beginsWith`
- `$all`

To make these give good failure messages, the `match` operation would need to
dynamically keep track of whether the expression it's currently evaluating has
been negated. This could be tracked via the "match context" which we'd need to
add to support the `$data` operator and the `path` failure property.

Note that in principle, `{ "$not": { "$all": [A, B, C] } }` could be translated
to:

    {
      "$or": [
        { "$allMatch": { "$ne": X } },
        { "$allMatch": { "$ne": Y } },
        { "$allMatch": { "$ne": Z } }
      ]
    }

However, this is only possible if the required values are specified literally.
If a `$data` operator is used to dynamically find the list of required values,
the translator cannot "see through" that and `$data` may resolve to a different
value each time it's encountered. So the negation still needs to be tracked
during the `match` operation, rather than being completely handled by
translation steps that happen before `match` starts.

Use of `$ref` also precludes full normalisation of expressions before evaluating
them, since it allows the creation of recursive matching procedures. These
cannot be translated by just inlining any referenced expressions, since any loop
of mutually recursive expressions would prevent this from completing.


### `$error` and `$reason`

A failing VDU can cause a document write request to return either a `401
Unauthorized` or `403 Forbidden` response. In JavaScript VDUs, which one to
return is indicated by throwing either `{ unauthorized: reason }` or `{
forbidden: reason }`.

Declarative VDUs can indicate which type of response each check produces using
the `$error` annotation. Any selector expression in the `validate_doc_update`
field may have this property set, but it defaults to `forbidden`. For example,
the first check here checks the user's name and returns a 401 on failure, while
the second checks the document structure and returns a 403 on failure.

    "validate_doc_update": {
      "$all": [
        {
          "$userCtx.roles": { "$all": ["_admin"] },
          "$error": "unauthorized"
        },
        {
          "$newDoc.type": { "$in": ["movie", "director"] },
          "$error": "forbidden"
        }
      ]
    }

A selector expression can also include a `$reason` field that overrides the
failure message generated by the selector itself. If the second selector above
fails, then the response would be a 403 with the body:

    {
      "error": "forbidden",
      "reason": {
        "failures": [
          {
            "path": ["$newDoc", "type"],
            "type": "in",
            "params": ["movie", "director"]
          }
        ]
      }
    }

But if the selector is modified like so:

      {
        "$newDoc.type": { "$in": ["movie", "director"] },
        "$error": "forbidden",
        "$reason": "Document must be a movie or director"
      }

Then the response will be:

    {
      "error": "forbidden",
      "reason": "Document must be a movie or director"
    }

The intent of this design is that by default, a declarative VDU generates a
sufficient machine-readable description of the validation failure that the
application can convert into a useful user-facing error message. Using the
`$reason` field overrides this and returns the given message directly on
failure.


# Advantages and Disadvantages

CouchDB currently supports validation of document updates by placing a
JavaScript function in the `validate_doc_update` field of one or more design
documents. When a doc is updated, all such functions defined in the target
database must run without throwing an exception, otherwise the write is
rejected.

This incurs a performance penalty since every new document must be round-tripped
through the JavaScript engine, requiring a bunch of (de)serialisation and I/O
compared to operations that happen inside the Erlang process. We would like to
investigate the possibility of adding a validation system that can execute
in-process, without JavaScript, in order to remove this overhead.

The most "obvious" way to achieve this would be simply allow
`validate_doc_update` (VDU) functions to be written in Erlang, as we do for
map-reduce index definitions. However, this is not especially accessible to most
users, and allows them to execute arbitrary code inside the database engine. A
better solution would be to design a way for validation rules to be expressed in
a declarative fashion, that the database engine can interpret.


## Implementation candidates

In early discussions, two main candidates emerged for implementing this feature.
Both provide a declarative means of expressing constraints on the shape of
acceptable documents.

- [Mango selectors](https://docs.couchdb.org/en/stable/ddocs/mango.html), which
  are already used by CouchDB to express queries and replication filters. This
  has the advantage of already being built into CouchDB and being familiar to
  its users. Evaluation of Mango selectors runs inside the Erlang runtime,
  making it significantly faster than the equivalent JavaScript map/reduce
  filters.

- [JSON Schema](https://json-schema.org/), which is a widely used tool for
  validation of JSON documents. While it is not built into CouchDB, a good and
  maintained Erlang library for it exists, named
  [jesse](https://github.com/2600hz/erlang-jesse). While this would add a new
  way of doing things to CouchDB, it is already familiar to many JSON users, and
  fits with CouchDB's original ethos of building around existing web
  technologies.

In this document we compare these two systems in depth, in terms of what
constraints they can express, and which features are unique to either system.
Following this comparison we explore a way forward for implementing declarative
validation, along with features to be added to the chosen system to make this
easier and more user-friendly.


## Functional comparison

In this section we give a comprehensive comparison of the operators included in
Mango and JSON Schema, and what each system is capable of expressing. Where
possible, the equivalent expression in each language is given. This section is
organised by data type, starting with basic matchers and combinators, before
looking at type-specific operators.

In code examples, the meta-variables `X`, `Y`, `Z` denote literal values, while
those beginning with `S` denote a nested schema or selector expression.


### Values and types

    | Mango                               | JSON Schema                         |
    | ----------------------------------- | ----------------------------------- |
    |                                     |                                     |
    | { "$eq": X }, or just X             | { "const": X }                      |
    |                                     |                                     |
    | { "$ne": X }                        | { "not": { "const": X } }           |
    |                                     |                                     |
    | { "$in": [X, Y, Z] }                | { "enum": [X, Y, Z] }               |
    |                                     |                                     |
    | { "$nin": [X, Y, Z] }               | { "not": { "enum": [X, Y, Z] } }    |
    |                                     |                                     |
    | { "$type": T }                      | { "type": T }                       |

Mango's `{ "$eq": X }` may be abbreviated to just `X` when it is used as the
selector for an object property, e.g. `{ "foo": 42 }` means the same as `{
"foo": { "$eq": 42 } }`. In other contexts, for example in combination with
`$elemMatch`, `$eq` must be used explicitly.

In both JSON Schema and Mango, equality is based on deep/structural comparison,
so it can be used to match objects and arrays, not just scalars.

JSON Schema supports all the types that are in Mango: `null`, `boolean`,
`number`, `string`, `object` and `array`. It also has a specific `integer` type
that will reject floating point values.


### Combinators

    | Mango                               | JSON Schema                         |
    | ----------------------------------- | ----------------------------------- |
    |                                     |                                     |
    | { "$and": [S1, S2, ...] }           | { "allOf": [S1, S2, ...] }          |
    |                                     |                                     |
    | { "$or": [S1, S2, ...] }            | { "anyOf": [S1, S2, ...] }          |
    |                                     |                                     |
    | -                                   | { "oneOf": [S1, S2, ...] }          |
    |                                     |                                     |
    | { "$nor": [S1, ...] }               | { "not": { "anyOf": [S1, ...] } }   |
    |                                     |                                     |
    | {                                   | {                                   |
    |   "$or": [                          |   "if": S1,                         |
    |     { "$and": [S1, S2] },           |   "then": S2,                       |
    |     {                               |   "else" S3                         |
    |       "$and": [                     | }                                   |
    |         { "$not": S1 },             |                                     |
    |         S3                          |                                     |
    |       ]                             |                                     |
    |     }                               |                                     |
    |   ]                                 |                                     |
    | }                                   |                                     |

JSON Schema's `if`/`then`/`else` construct can be replicated in Mango using
`$and`, `$or` and `$not`, at the slight inconvenience of having to repeat the
definition of `S1`. If the `else` clause is not required, the Mango selector
simplifies to:

    {
      "$or": [
        { "$and": [S1, S2] },
        { "$not": S1 }
      ]
    }

`oneOf` is very useful for expressing sum types, i.e. a type formed by the union
of two non-overlapping sets. This would be a good fit for expressing that a
database contains docs that conform to exactly one of a set of different
document types. For example, a doc may represent a movie, or a director, but not
both. It can technically be replicated in Mango but is somewhat awkward and does
not scale nicely as more subtypes are added:

    // JSON Schema

    { "oneOf": [S1, S2, S3] }

    // Mango

    {
      "$or": [
        { "$and": [S1, { "$nor": [S2, S3] }] },
        { "$and": [S2, { "$nor": [S1, S3] }] },
        { "$and": [S3, { "$nor": [S1, S2] }] },
      ]
    }

Expressing `oneOf` in Mango requires all N sub-selectors to be repeated N times,
so a `oneOf` expression for N schemas requires N^2 selectors to be expressed in
Mango. This also means that evaluating the expression takes O(N^2) time.

In practice this may not be such a big problem, since the distinction between
different document variants can usually be done by checking a single field,
rather than by reproducing the full schema for each variant. For example, a
Mango selector for validating both `movie` and `director` documents might look
like:

    {
      "$or": [
        {
          "type": "movie",
          "title": { "$type": "string" },
          "year": { "$type": "number" },
          "duration": { "$type": "number", "$gt": 0 },
          ...
        },
        {
          "type": "director",
          "name": { "$type": "string" },
          "birthdate": { "$type": "string", "$regex": "^[0-9]{4}-[0-9]{2}-[0-9]{2}$" },
          ...
        }
      ]
    }

Since each branch of the outer `$or` has a distinct `type` field, it is only
possible for any given document to match one of the branches, which mimics the
behaviour of `oneOf`.


### Numbers

    | Mango                               | JSON Schema                         |
    | ----------------------------------- | ----------------------------------- |
    |                                     |                                     |
    | { "$gte": X }                       | { "minimum": X }                    |
    |                                     |                                     |
    | { "$gt": X }                        | { "exclusiveMinimum": X }           |
    |                                     |                                     |
    | { "$lte": X }                       | { "maximum": X }                    |
    |                                     |                                     |
    | { "$lt": X }                        | { "exclusiveMaximum": X }           |
    |                                     |                                     |
    | { "$mod": [X, 0] }                  | { "multipleOf": X }                 |

Mango's comparison operators can be applied to _any_ lexicographically sortable
values: numbers, strings, and arrays of sortable values. (Strictly speaking,
Mango defines a sort order over all values, including values of different types,
but these operators are most useful for types with a natural lexicographic
ordering.) In JSON Schema these bounding operators are only applicable to
numbers.


### Strings

JSON Schema's `pattern` keyword maps directly to `$regex` in Mango; `{ "$regex":
P }` is equivalent to `{ "type": "string", "pattern": P }`.

Mango's `$beginsWith` operator can be implemented in JSON Schema by using
`pattern` with a regex using the `^` anchor.

JSON Schema also has a `format` keyword that allows matching against various
built-in grammars, such as dates, times, URLs, IP and email addresses, etc. No
such formats are built into Mango.

Mango also lacks the `minLength` and `maxLength` operators from JSON Schema.
Although in JavaScript strings are objects, and you could in theory express a
string length constraint as `{ "field": { "length": { "$lt": X } } }`, this does
not work in Mango. This sort of property access only works on _JSON objects_,
not scalar values. However, these keywords can also be implemented using
`$regex` with a pattern like `^.{min,max}$`.


### Arrays

    | Mango                               | JSON Schema                         |
    | ----------------------------------- | ----------------------------------- |
    |                                     |                                     |
    | { "$allMatch": S }                  | { "items": S }                      |
    |                                     |                                     |
    | { "$elemMatch": S }                 | { "contains": S }                   |
    |                                     |                                     |
    | { "$all": [X, Y, ...] }             | {                                   |
    |                                     |   "allOf": [                        |
    |                                     |     { "contains": { "const": X } }, |
    |                                     |     { "contains": { "const": Y } }, |
    |                                     |     ...                             |
    |                                     |   ]                                 |
    |                                     | }                                   |
    |                                     |                                     |
    | { "$size": N }                      | { "minItems": N, "maxItems": N }    |

Note that Mango's `$allMatch` and `$elemMatch` need a selector _object_ as an
argument, not a bare value. So for example to express "field `foo` contains the
value `"bar"`" one must write `{ "foo": { "$elemMatch": { "$eq": "bar" } } }`,
not just `{ "foo": { "$elemMatch": "bar" } }`.

There is a potential gotcha in the way `contains` works, since it operates on
both arrays _and strings_. For example, say you need to express that a user's
`roles` field contains any one of `"foo"` or `"bar"`. In Mango this would be
written:

    {
      "roles": { "$elemMatch": { "$in": ["foo", "bar"] } }
    }

The analog in JSON Schema is:

    {
      "properties": {
        "roles": { "contains": { "enum": ["foo", "bar"] } }
      },
      "required": ["roles"]
    }

(As we'll see shortly, JSON Schema is more verbose in expressing that certain
fields must be present.) However, this accepts docs where `roles` is a string,
which contains `"foo"` or `"bar"` as a substring, e.g.:

    { "roles": "food" }

This is usually not what's intended here, and an additional constraint of
`"type": "array"` must be added on the `roles` property to prevent this.

    {
      "properties": {
        "roles": {
          "type": "array",
          "contains": { "enum": ["foo", "bar"] }
        }
      },
      "required": ["roles"]
    }

Mango has no analog for the JSON Schema keywords:

- `minItems` or `maxItems` when used independently
- `minContains` or `maxContains`
- `prefixItems`
- `uniqueItems`

The most noticeable downside of this is that Mango has no direct way of
representing tuples or sets. Mango's main advantage over JSON Schema is its
`$all` operator, which provides a direct way of expressing set containment.


### Objects

The two systems differ substantially in how they represent requirements on
object properties. In JSON Schema, `"foo": S` means "if the field `foo` is
present then it must match `S`" whereas in Mango it means "the field `foo` must
exist and match `S`". This means that expressing optional vs required properties
is very different in each system:

    | Mango                               | JSON Schema                         |
    | ----------------------------------- | ----------------------------------- |
    | {                                   | {                                   |
    |   "foo": {                          |   "properties": {                   |
    |     "$or": [                        |     "foo": S1,                      |
    |       { "$exists": false },         |     "bar": S2,                      |
    |       S1                            |     ...                             |
    |     ]                               |   }                                 |
    |   },                                | }                                   |
    |   "bar": {                          |                                     |
    |     "$or": [                        |                                     |
    |       { "$exists": false },         |                                     |
    |       S2                            |                                     |
    |     ]                               |                                     |
    |   },                                |                                     |
    |   ...                               |                                     |
    | }                                   |                                     |
    | ----------------------------------- | ----------------------------------- |
    | {                                   | {                                   |
    |   "foo": S1,                        |   "properties": {                   |
    |   "bar": S2,                        |     "foo": S1,                      |
    |   ...                               |     "bar": S2,                      |
    | }                                   |     ...                             |
    |                                     |   },                                |
    |                                     |   "required": ["foo", "bar"]        |
    |                                     | }                                   |
    | ----------------------------------- | ----------------------------------- |
    | {                                   | {                                   |
    |   "foo": {                          |   "properties": {                   |
    |     "bar": S                        |     "foo": {                        |
    |   }                                 |       "properties": {               |
    | }                                   |         "bar": S                    |
    |                                     |       },                            |
    | or,                                 |       "required": ["bar"]           |
    |                                     |     }                               |
    | {                                   |   },                                |
    |   "foo.bar": S                      |   "required": ["foo"]               |
    | }                                   | }                                   |

JSON Schema has a way of making the validation of some fields conditional on the
presence of others; `"dependentSchemas": { "foo": S }` means that if an object
has the field `foo`, then the object must also match schema `S`. Its general
form is:

    // JSON Schema

    {
      "dependentSchemas": {
        "foo": S1,
        "bar": S2,
        ...
      }
    }

This can be expressed in Mango using logical combinators; for each field, either
it is present and the object also matches the corresponding selector, or, the
field does not exist.

    // Mango

    {
      "$and": [
        {
          "$or": [
            { "$and": [{ "foo": { "$exists": true } }, S1] },
            { "foo": { "$exists": false } }
          ]
        },
        {
          "$or": [
            { "$and": [{ "bar": { "$exists": true } }, S2] },
            { "bar": { "$exists": false } }
          ]
        },
        ...
      ]
    }

Although this translation is technically possible, it's quite likely the
resulting selector would be quite hard to read. The intended structure and its
meaning would not be readily apparent, especially when `S1`, `S2` etc are filled
in with actual selectors.

The related keyword `dependentRequired` specifies that if a field is present,
then a list of other fields must also be present.

    // JSON Schema

    {
      "dependentRequired": {
        "foo": ["bar", "baz", ...],
        ...
      }
    }

This can also be expressed with combinations of the Mango `$exists` operator.

    // Mango

    {
      "$and": [
        {
          "$or": [
            {
              "foo": { "$exists": true },
              "bar": { "$exists": true },
              "baz": { "$exists": true },
              ...
            },
            {
              "foo": { "$exists": false }
            }
          ]
        },
        ...
      ]
    }

The above forms are most useful for when an object is used to represent a
_record_, i.e. a structure with a fixed set of fields with known names and where
each field has a distinct type. JSON Schema also provides keywords suited to
validating representations of _maps_, i.e. open-ended sets of key-value pairs
where the values all have the same type, or the type depends on the key name.

Those keywords are `propertyNames`, `patternProperties`, and
`additionalProperties`. These can be used to match keys by a pattern, set value
schemas based on key patterns, and to set a "default" schema for any keys not
covered by `properties` or `patternProperties`.

Mango does not have any obvious analog for this functionality and so it is not
good at working with objects that represent maps. This is somewhat to be
expected, since documents stored in databases tend to have record structure more
often than map structure, and queries tend to address specific fields rather
than looking at properties of maps whose keys are not fixed in application code.
This is also a self-reinforcing phenomenon in that users design their documents
around the types of queries their database makes easy to express.


### Abstraction and composition

JSON Schema provides a means of abstraction for parts of a schema. By
convention, the `$defs` keyword is used to store reusable schema fragments, and
then `$ref` can be used to refer to them from elsewhere in the schema. For
example, this is a schema for an object whose `foo` and `bar` fields must both
be even numbers:

    {
      "$defs" {
        "even-number": { "type": "number", "multipleOf": 2 }
      },
      "properties": {
        "foo": { "$ref": "#/$defs/even-number" },
        "bar": { "$ref": "#/$defs/even-number" }
      }
    }

In general `$ref` may be any URI; the fragment part contains a JSON pointer to
be resolved from the root of the schema. The Erlang library `jesse` will fetch
any URI referenced by a `$ref` field, but this behaviour can be turned off,
which is what we would want in order to prevent CouchDB being used as an open
HTTP client and fetching potentially untrusted content.

If the referenced URI has already been encountered as the `$id` of a schema or
sub-schema, it will not be fetched, i.e. this does not require an HTTP request:

    {
      "$id": "https://example.com/schema.json",
      "$defs" {
        "even-number": { "type": "number", "multipleOf": 2 }
      },
      "properties": {
        "foo": { "$ref": "https://example.com/schema.json#/$defs/even-number" },
        "bar": { "$ref": "/schema.json#/$defs/even-number" }
      }
    }

The `$id` keyword sets the "base URI" for the current schema, and any `$id` and
`$ref` fields found within the schema are resolved relative to this base URI. A
`$ref` that contains only a URI fragment is resolved relative to the current
schema (the nearest enclosing object with an `$id` field, or the root object is
none exists).

The `$ref` keyword can be used to refer to a parent of the current node, thus
creating a recursive schema. For example, this defines a possible schema for
representing a tree of HTML nodes, by using the root of the schema as a `$ref`:

    {
      "type": "object",
      "properties": {
        "tagName": { "type": "string" },
        "attributes": { "type": "object" },
        "children": { "type": "array", "items": { "$ref": "#" } }
      }
    }

Mango does not provide any means of abstraction or reuse of selectors. This may
pose a significant downside when compared to authoring `validate_doc_update()`
functions in JavaScript, where arbitrary functions can be defined inside the VDU
function itself and invoked as needed. We may need to come up with a means of
doing this, and we explore this in the worked example at the end of this
document.


### Summary

To sum up the differences we have seen between JSON Schema and Mango:

- Mango has a clear syntactic distinction between its own keywords and document
  data fields via the `$` sigil.

- JSON Schema lacks a direct analog of the `$nin` operator.

- JSON Schema has an additional type named `integer`.

- The JSON Schema keyword `oneOf` can technically be translated into Mango,
  though it is usually inconvenient to translate it strictly. In many practical
  situations, there is an idiomatic Mango expression that gives equivalent
  behaviour without much overhead.

- Mango lacks a direct equivalent of `if`/`then`/`else` and requires repetition
  of the condition to achieve the same meaning.

- Mango's comparison/ordering operators can be applied to any lexicographically
  sortable data type, not just numbers.

- Mango has no built-in string formats, c.f. JSON Schema's `format` keyword.

- Mango's `$all` operator is much more verbose to express in JSON Schema and
  requires the relevant values to be literally present in the schema. As we
  shall see later, this is not necessarily the case for the kinds of comparisons
  VDUs typically need to perform. This makes expressing some set relations
  hard/impossible in JSON Schema.

- JSON Schema is able to express tuple and set types that have no analog in
  Mango. This may not be a problem in practice as database docs tend to use
  records with named fields instead of tuples. Sets are important for expressing
  permissions, and we investigate this further later on.

- JSON Schema is generally much more verbose than Mango when expressing rules
  about object properties, especially when those properties are required, which
  is frequently the case in VDU logic. It lacks the `{ "foo.bar": X }` shorthand
  and must express the same thing as `{ "properties": { "foo": { "properties": {
  "bar": { "const": X } }, "required": ["bar"] } }, "required": ["foo"] }`.

- Mango lacks a way of banning any extraneous fields from objects. Specific
  fields can be banned via `"field": { "$exists": false }`, but it cannot
  express that an object must have no additional properties beyond those listed
  in the selector.

- JSON Schema has some facilities for expressing maps which do not exist in
  Mango; Mango is much more adept at representing records. `dependentSchemas`
  and `dependentRequired` can be expressed in Mango but in a somewhat convoluted
  way.

- Mango has no means of abstraction equivalent to the `$id`, `$anchor` and
  `$ref` keywords in JSON Schema.


## Cross-field comparisons

The current `validate_doc_update()` interface provides the user-defined VDU
function with four values:

- `newDoc`, the document value that is to be validated
- `oldDoc`, the previous revision of the document in the revtree
- `userCtx`, a representation of the user attempting the updating, containing
  their `name` and `roles`
- `secObj` - the database security object, containing lists of users and roles
  with admin access, and users and roles with member access

Given these inputs there are a few typical use cases for VDUs:

- Checking that documents have a valid structure, i.e. they conform to a schema,
  and are valid values in themselves. This requires inspecting only `newDoc`.

- Restricting the set of changes that the new version is allowed to make,
  relative to the old version. This means comparing `newDoc` to `oldDoc`. This
  may depend on the user's permissions, and thus also require comparing
  `userCtx` to the other inputs.

- Authorising particular users to alter the database, regardless of the
  document in question. This means comparing `userCtx` to `secObj`.

- Restricting what documents, or parts of documents, a given user can change, or
  what types of changes they're allowed to make. This requires at least
  comparing `userCtx` and `newDoc` but may involve `oldDoc` and `secObj` as
  well.

Most of these use cases require comparing parts of the different input values.
Neither Mango nor JSON Schema provides any way of expressing a comparison to a
value elsewhere in the document, as opposed to a value given inline in the
selector/schema. Some JSON Schema validators such as AJV apparently support an
operator named `$data` (see e.g.
[json-schema-spec#51](https://github.com/json-schema-org/json-schema-spec/issues/51)),
but this is not standardised.

This suggests we would need to invent a way of doing this, unless we decide to
restrict declarative VDUs to only deal with schema validation. Almost all
real-world authorization use cases involve cross-field comparison. If we have to
start inventing new features, it would be easiest to do this in Mango, rather
than try to add things to a language and implementation we have no control over.

Even some schema validation rules are best expressed as cross-field constraints.
For example, an object representing a range must have a `max` field that is
larger than its `min` value. This could be expressed in Mango with the addition of
a `$data` operator:

    { "max": { "$gt": { $data": "min" } } }

This would allow the document `{ "min": 1, "max": 2 }` but reject `{ "min": 1,
"max": 0 }`.


### Format for `$data` references

If we need a way to refer to values inside of a JSON document, it is tempting to
adopt [JSON Pointer](https://datatracker.ietf.org/doc/html/rfc6901). JSON Schema
uses this for its `$ref` feature and the non-standard `$data` feature. However,
JSON Pointer only supports absolute paths, which traverse from the root of the
document.

This means it's not possible to express constraints on nested objects,
for example if a document should contain an array of objects whose `max` field
is greater than their `min` value. To express such a constraint we need to be
able to say that every `max` field must be greater than its sibling `min` field,
rather than compare it to a single `min` field in the root document.

There do exist other standards for addressing JSON data, for example
[JSONPath](https://datatracker.ietf.org/doc/html/rfc9535) was standardised
relatively recently after many years of development. However, it is
substantially more complex than JSON Pointer and contains many features that are
not applicable to our current problem.

We would instead suggest a small extension to the existing Mango shorthand
syntax wherein `{ "a.b": x }` is equivalent to `{ "a": { "b": x } }`. These
paths could be used with `$data` to identify fields in the document, starting at
its root. We can extend this by allowing paths that begin with one or more `.`
characters, which indicate relative traversal. Each `.` character indicates
navigation one level "up" from the current value.

For example, `"max": { "$gt": { "$data": ".min" } }` would mean that `max` needs
to be greater than the `min` field in the same object as the `max` field. The
path `..min` would mean the `min` field in the "parent" object, i.e. the nearest
object that contains the current one.

Using this facility we could express our "list of valid min/max ranges"
constraint as:

    {
      "ranges": {
        "$allMatch": {
          "max": { "$gt": { "$data": ".min" } }
        }
      }
    }

This could be implemented by keeping a stack of the objects that lead to the
current value while evaluating a selector. If a path begins with N `.` chars,
then the rest of the path is taken relative to the Nth-from-last object on the
stack. Otherwise the path is taken from the root object.


### Modelling the inputs to VDUs

The existing JavaScript-based `validate_doc_update` API is a function that takes
the four inputs we identified above: the new and old documents, the user
context, and the security object. We need a way of presenting these four inputs
to the selector/schema for a declarative VDU so that it can validate them,
including using `$data` references to compare parts of the different inputs.

The simplest way to achieve this is to model the VDU inputs as a single JSON
document whose root object contains four fields: `$newDoc`, `$oldDoc`,
`$userCtx`, and `$secObj`. It is a slight inconsistency that we are using `$` to
denote data fields, rather than operators, but we should mark these fields as
"special" and distinguish them from user input in some way. Using `_` is not
viable because a user-submitted design doc would not be allowed to contain
expressions like `{ "_userCtx.name": "alice" }` since user documents cannot
contain fields beginning with `_`.

Given this input, selectors could be written to put constrains on the shape of
the new doc by writing `{ "$newDoc": { "a": x, "b": y, ... } }`. `$data` can be
used to access any part of the input, e.g. `{ "$data": "$userCtx.name" }` would
get the current user's name.

In [issue 1554](https://github.com/apache/couchdb/issues/1554) and [@garbados'
related
gist](https://gist.github.com/garbados/dd698bc835bf6f0df0552c4edff6eb56), some
proposals make an explicit distinction between rules to do with schema
validation, and rules about authorisation. Given the number of different use
cases involving comparisons between the VDU inputs, we don't think it's a good
idea to arbitrarily restrict what VDUs can express in this way. Though there
will definitely be some common patterns to validation and authorisation code,
and we may wish to bake some of these into built-in operators, we think it would
be better to initially implement this feature in a way that lets users write
arbitrary expressions over the full VDU input, and does not preempt the kinds of
rules they might want to write. Mango is certainly sufficiently expressive to
encode most common authorisation needs, as we explore later under in the worked
example.


### Restrictions on the use of `$data` references

If we allowed `$data` to appear anywhere in a selector, then the input document
could use it to inject its own selector operators and bypass the intended
validation constraints. We therefore must only allow its use in places where a
literal value is expected, that is:

- As the operand to `$eq`, `$ne`, `$lt`, `$lte`, `$gt`, `$gte`
- As the full array operand, or an individual array item, for `$in`, `$nin`,
  `$all` or `$mod`

When used with a normal object field, it should be interpreted as an exact
equality constraint. i.e. `{ "a": { "$data": "b" } }` means `{ "a": { "$eq": {
"$data": "b" } } }`, otherwise this would allow operator injection.

`$data` must not be allowed to provide a selector item for the `$and`, `$or`,
`$nor`, `$not`, `$allMatch` or `$elemMatch` operators. It may appear _inside_
selectors given to these operators, as in the above example, but must not be
allowed as a direct input to them.

Although the `$type`, `$exists`, `$size`, `$regex` and `$beginsWith` operators
take literal values, it does not seem especially necessary to allow use of
`$data` with them. In most cases this would simply allow documents to override
what is intended as a static constraint, rather than to provide a useful way to
express constraints over multiple fields.


### Note on translation of `$all` and `$data` into JSON Schema

In our comparison of Mango and JSON Schema, we noted that the Mango selector
`"$all": [X, Y, Z]` can be translated into the equivalent JSON Schema
expression:

    {
      "allOf": [
        { "contains": { "const": X } },
        { "contains": { "const": Y } },
        { "contains": { "const": Z } }
      ]
    }

This translation can be done when the values `X`, `Y` and `Z` are literally
present in the selector document. However, if we combine `$all` with a `$data`
reference, as in `"$all": { "$data": "path.to.array" }`, this is no longer the
case. There's no way of translating this statically into JSON Schema because its
meaning depends on the input document.

You would instead need a way to express that an array must be looked up using a
`$data` reference, and then expanded into a schema by doing `{ allOf:
data.map((val) => ({ contains: { const: val } }))`. JSON Schema provides no way
of doing this, and this combination of `$all` and `$data` operators is very
useful for expressing some permission rules, as we shall see later. This is a
further point against using JSON Schema and instead using Mango with some
features added.


## Generating good feedback

In Mango, the purpose of selectors is to return a binary `true`/`false` result
to indicate whether a value matches. If we are going to use Mango for schema
validation, it is not very useful just to tell the caller that a doc did not
validate. Instead, we should give the caller a list of reasons the doc failed to
match the validation rules, and this changes how selectors need to be evaluated.


### Selector output and short-circuit behaviour

As currently implemented, the evaluation of Mango selectors will "short circuit"
as soon as it encounters an operator that returns `false`. For example, to
evaluate `$and`, we can return `false` as soon as we find a single selector that
does not match the input. For compound object field matchers like `{ "a": X,
"b": Y }`, we can return `false` as soon as we find a single field whose
selector fails.

If we're instead using selectors to provide validation feedback, this is not a
good strategy. When filling out a form, it's better to be given all the
validation errors at once rather than getting an error on one field, submitting
again, getting a new error on a different field, and so on.

This requires a different evaluation strategy where instead of returning `true`
or `false`, `mango_selector:match/2` should return a list of _failures_, which
are records/tuples describing the ways the input failed to match. A failure
should include some representation of the path to the invalid value in the input
doc, the type of the mismatch, and any parameters.

For example, if we're validating against the schema `{ "title": { "$type":
"string" }, "year": { "$gt": 0 } }`, then a list of failures might look like:

    [
      { "path": ["title"], "type": "type_mismatch", "params": ["string"] },
      { "path": ["year"], "type": "too_low", "params": [0] }
    ]

The `path` field is a list of strings and integers that give the location of the
invalid value in the input doc, such that the value can be found by evaluating:

    path.reduce((val, field) => val[field], doc)

All selector operators should return a possibly-empty list of failures, and a
match is considered successful if an empty list is returned.

In order to provide all validation failures to the caller at once, selector
evaluation must not short-circuit on operators that involve lists. That is, the
`$and`, `$or`, `$nor`, `$allMatch` and `$elemMatch` operators should evaluate
all their selectors and input items, and collect all resulting failures into a
single list, rather than returning as soon as an internal `match/2` call returns
a non-empty list.

In order to generate the `path` element of failures, `mango_selector:match`
would need to keep track of the path it has taken to reach the current value. It
will need an extra parameter to keep track of this, which we'll call a _match
context_. The path to the current value is just one thing we will need to store
in the match context as selectors are evaluated.

One other thing we may want to store in the context is a "mode" that indicates
whether the selector is being used just as a filter in a `_find` call or a
replication, or if it's being used to validation. In the former case, it would
still be allowed to short-circuit to save time.

We also note here that `jesse` (the Erlang JSON Schema library) only returns the
first failure that it found by default, but it can be made to return all
failures by passing the option `{allowed_errors, infinity}`.


### Dealing with negation

To give good feedback for selectors involving negation, a naive evaluation
strategy is not sufficient. For example, given a selector `{ "$not": { "$eq": X
} }`, if we just need to return a boolean, we can implement this by having the
`$not` operator invoke the sub-selector `{ "$eq": X }` and then negate its
result.

When operators return a list of failures, this will not work. If `{ "$eq": X }`
returns a non-empty list, then this can be "negated" into an empty list. But if
it returns an empty list, the `$not` operator then has to "invent" a failure
message, but since it doesn't know anything about the internal structure of its
operand, it cannot possibly give any useful feedback to the caller.

To solve this, negation needs to be pushed to the "leaves" of the selector's
node tree. For some operators there is a trivial translation to a different
built-in operator, for example `{ "$not": { "$eq": X } }` is just `{ "$ne": X
}`. Translating the selector in this way means the `$ne` operator can give
meaningful failure feedback in a way that the `$not`/`$eq` combination cannot.

Most of the operators in Mango can be translated so that the `$not` operator is
replaced with a different built-in operator:

- `{ "$not": { "field": X } }` becomes `{ "field": { "$ne": X } }`
- `{ "$not": { "$eq": X } }` becomes `{ "$ne": X }` and vice versa
- `{ "$not": { "$in": X } }` becomes `{ "$nin": X }` and vice versa
- `{ "$not": { "$lt": X } }` becomes `{ "$gte": X }` and vice versa
- `{ "$not": { "$gt": X } }` becomes `{ "$lte": X }` and vice versa
- `{ "$not": { "$exists": X } }` becomes `{ "$exists": !X }`
- `{ "$not": { "$not": X } }` becomes `X`

- `{ "$not": { "$all": [A, B] } }` becomes `{ "$or": [{ "$not": A }, { "$not": B
  }] }`

- `{ "$not": { "$or": [A, B] } }` becomes `{ "$and": [{ "$not": A }, { "$not": B
  }] }`

- `{ "$nor": [A, B] }` becomes `{ "$and": [{ "$not": A }, { "$not": B }] }`

- `{ "$not": { "$nor": [A, B] } }` becomes `{ "$or": [A, B] }`

- `{ "$not": { "$allMatch": A } }` becomes `{ "$elemMatch": { "$not": A } }`

- `{ "$not": { "$elemMatch": A } }` becomes `{ "$allMatch": { "$not": A } }`

This conversion is already implemented by `mango_selector:norm_negations/1`
because it helps with identifying indexes than are applicable to a given query.
However, some operators don't have a built-in negation, for example:

- `$type`
- `$size`
- `$mod`
- `$regex`
- `$beginsWith`

These operators would need some way of understanding that they were being
negated, either by wrapping their operand in a negation marker (`{ "$type": {
"$not": "string" } }`), or by using the match context to track when a `$not`
operator has been encountered and so any operators underneath it should be
negated. This would allow these operators to give better feedback like "the
`tags` field must not be empty" in the case of the selector `{ "tags": { "$not":
{ "$size": 0 } } }`.

One operator that's especially tricky to negate is `$all`. `{ "$not": { "$all":
[X, Y, Z] } }` means that the input array must not contain _all_ of `X`, `Y` and
`Z`. That is, it must either not contain `X`, or not contain `Y`, or not contain
`Z`. It is equivalent to:

    {
      "$or": [
        { "$allMatch": { "$ne": X } },
        { "$allMatch": { "$ne": Y } },
        { "$allMatch": { "$ne": Z } }
      ]
    }

This translation could be done by `norm_negations/1` when the array `[X, Y, Z]`
is given literally (as of 3.5.0 it does not appear that `norm_negations/1` and
`negate/1` actually translate the `$all` operator). However, if it is produced
by a `$data` reference, as in `{ "$not": { "$all": { "$data": "x" } } }`, the
translation cannot "see through" the `$data` operator and the translation has to
be done when the selector is evaluated on a particular document. This is another
reason that negation may need to be passed around in the match context, rather
than handled entirely by translation of the selector before evaluation.

Note that this cannot be handled by resolving `$data` references before applying
`norm_negations/1` to the selector. The existence of relative `$data` lookups
means that a reference will resolve to a different value depending which part of
the document the selector is currently matching, so they can only be resolved
during evaluation by the `match()` function.


### The need for guard clauses

Earlier we gave an example where the existing Mango operators can be used to
express a sum type, for example a document whose `type` field identifies it as a
`movie` or a `director`, and the rules for its other fields depend on which one
it is.

    {
      "$or": [
        {
          "type": "movie",
          "title": { "$type": "string" },
          "year": { "$type": "number" },
          "duration": { "$type": "number", "$gt": 0 },
          ...
        },
        {
          "type": "director",
          "name": { "$type": "string" },
          "birthdate": { "$type": "string", "$regex": "^[0-9]{4}-[0-9]{2}-[0-9]{2}$" },
          ...
        }
      ]
    }

Strictly speaking, this is an accurate description of the intended validation
rules. However, it will not give good feedback. Under the evaluation rules
described above, where all operators return a possibly-empty list of failures,
the `$or` operator would work be collecting a list of failures for each of its
sub-selectors, and then if any of these lists is empty, then `$or` returns an
empty list. If none of the lists is empty, then the most reasonable thing for
`$or` to do would be to return the combined list of all failures from its
sub-selectors, since it has no idea what any of them mean.

For example, the document `{ "type": "movie", "title": "Porco Rosso", "year":
1922, "duration": 89 }` matches the first sub-selector, and so `$or` returns an
empty list representing success in this case. But the document `{ "type":
"movie", "year": 1922 }` matches neither sub-selector; the first sub-selector
produces these failures:

- the `title` field is not a string
- the `duration` field is not a number and is not greater than 0

The second sub-selector produces these failures:

- the `type` field is not `director`
- the `name` field is not a string
- the `birthdate` field does not have the required format

It's impossible for the `$or` operator to infer that the intent of this rule is
for the `type` field to be either `"movie"` or `"director"` and for the rules
for other fields to depend on that. All it can reasonably do is return _all_
these failures to the caller, but this would be confusing. The caller is not
expected to correct all these failures, it only needs to correct those that go
with the document's `type`, and some of the failures may be mutually
contradictory.

To give better feedback in this case, we need a better way of expressing the
constraints. In [issue 1554](https://github.com/apache/couchdb/issues/1554) and
[@garbados' related
gist](https://gist.github.com/garbados/dd698bc835bf6f0df0552c4edff6eb56), the
idea of guard clauses has been discussed. The terminology varies but the general
idea is to be able to express that a "condition" selector should only be
evaluated if some other "guard" selector succeeds; if the guard selector fails
this is not considered a validation error.

The terms "guard" and "condition" are potentially confusing and it may be easy
for users to forget which word means what, and we would suggest adopting the
"if/then/else" terminology here. Using these operators, the above rule could be
rewritten as:

    {
      "$and": [
        { "type": { "$in": ["movie", "director"] } },
        {
          "$if": { "type": "movie" },
          "$then": {
            "title": { "$type": "string" },
            "year": { "$type": "number" },
            "duration": { "$type": "number", "$gt": 0 },
            ...
          },
        },
        {
          "$if": { "type": "director" },
          "$then": {
            "name": { "$type": "string" },
            "birthdate": { "$type": "string", "$regex": "^[0-9]{4}-[0-9]{2}-[0-9]{2}$" },
            ...
          }
        }
      ]
    }

This would give much better feedback. If the `type` field is neither `"movie"`
nor `"director"`, then this is the only failure that is reported, since the
`$if` conditions in the other selectors will not match. If `type` is `"movie"`,
then only the rules from the selector with `"$if": { "type": "movie" }` are
applied, and likewise if `type` is `"director"`. Rather than getting the union
of all possible validation failures for all document types, the caller only gets
those for the type in question.

The general behaviour of the selector `{ "$if": A, "$then": B, "$else": C }` is
then as follows:

- If selector `A` matches the input:
  - If a `$then` clause is given, then return the result of matching the input
    against `B`.
  - If no `$then` clause is given, return a list containing a single failure,
    reporting that a `$then` clause is required.
- If selector `A` does not match the input:
  - If an `$else` clause is given, then return the result of matching the input
    against `C`.
  - If no `$else` clause is given, then return an empty list.

When other operators or fields are present in the selector, then the usual
behaviour of compound selectors applies: an input must match all the operators
in a selector to be valid. For example, the following:

    {
      "$gt": 0,
      "$if": { "$gt": 10 },
      "$then": { "$mod": [5, 0] }
    }

This selector means the input must be greater than 0, and, if it is greater than
10, then it must be a multiple of 5. This behaviour, and the possibility for
`$else` clauses, is why we have an explicit `$then` field. It means we can have
rules that always apply to a value, and rules that are conditional, in the same
selector.

The other design choice we could make here is that the effect of an `$if` field
is to make all its sibling fields dependent on the guard clause. This makes
matching more complicated because it means we need to check for `$if` first
rather than treating all fields equally, and it makes it more complicated to
create compound selector expressions.

If we had no `$then` field, and the sub-selector was inlined into the selector
with the `$if` field, the above would need to be written:

    {
      "$and": [
        { "$gt": 0 },
        {
          "$if": { "$gt": 10 },
          "$mod": [5, 0]
        }
      }
    }

Removing the `$then` wrapper around the `$mod` selector is a minor convenience
and would make expression of compound selectors more complicated, so on balance
it is best if the `$then` field is required to scope the selectors that depend
on the guard clause.

The final thing we need to take care of is how `$if`/`$then`/`$else` interacts
with negation and data references. `$data` is easily addressed; since the `$if`,
`$then` and `$else` fields take selectors as operands, a `{ "$data" }` reference
may not be used as the operand for any of them, as this would allow operator
injection.

As for negation: `!(A ? B : C)` is the same as `A ? !B : !C`, so the combination
of `$not` and `$if` translates as:

    {
      "$not": {                       {
        "$if": A,                       "$if": A,
        "$then": B,         ->          "$then": { "$not": B },
        "$else": C                      "$else": { "$not": C }
      }                               }
    }

Since `$then` and `$else` may not be present, we need to decide how negation is
applied when they're missing. Per the above rules, if `$then` is not set then it
defaults to `NONE($then)`, a selector which always fails by reporting that the
`$then` field is required. If `$else` is not set then it defaults to `ANY`, a
selector which always passes. Negating `"f": NONE(f)` gives `"f": ANY`, and vice
versa.

When evaluating a Mango selector as part of a `_find` query, replication filter,
or any other currently existing use case, `$if`/`$then`/`$else` can be
translated into a combination of other operations that give the same boolean
result; `{ "$if": A, "$then": B, "$else": C }` is equivalent to:

    {
      "$or": [
        { "$and": [A, B] },
        { "$and": [{ "$not": A }, C] }
      ]
    }


### Custom error messages

Sometimes, the failure messages generated by the leaf nodes of a selector
expression may not be a good or clear thing to present to the caller. This is
especially the case if a rule involves the combination of many smaller pieces of
logic, or involves regular expressions or array comparisons.

VDUs also need to indicate whether the server should return a `401 Unauthorized`
or `403 Forbidden` response on failure. JavaScript VDUs indicate this by
throwing `{ unauthorized: reason }` or `{ forbidden: reason }`.

We can address this need by annotating any selector in the tree with `$error`
(which defaults to `"forbidden"`) and `$reason` fields. When these are present,
they are added to the failure records that the selector generates, and then
propagated back up the tree unless some higher-level selector overrides them.
The server can then use this information to construct a response for the client.

For example, this selector uses a regex to validate a field but produces a
human-readable error message, whereas the default failure record for `$regex`
operations would just tell the caller the field needs to match an inscrutable
pattern.

        {
          "name": { "$regex": "^[^_:][^:]*$" },
          "$error": "forbidden",
          "$reason": "Usernames must not begin with underscore or contain colons"
        }


## Worked example

To illustrate how the features proposed here could work in practice, we'll now
work through an example of translating an exiting JavaScript VDU to a
declarative form. Specifically we will look at the [example from the CouchDB VDU
docs](https://docs.couchdb.org/en/stable/ddocs/ddocs.html#validate-document-update-functions).

The function begins with the following:

    if (newDoc._deleted === true) {
        // allow deletes by admins and matching users
        // without checking the other fields
        if ((userCtx.roles.indexOf('_admin') !== -1) ||
            (userCtx.name == oldDoc.name)) {
            return;
        } else {
            throw({forbidden: 'Only admins may delete other user docs.'});
        }
    }

This code runs if the doc is a tombstone (i.e. it has `_deleted: true`). If the
user has the `_admin` role, or their name matches the doc's `name`, the VDU
returns, otherwise it throws a `forbidden` error. Either way, if
`newDoc._deleted === true`, this is the only code that gets executed; the rest
of the VDU does not run.

We can model this declaratively by using `$if`, putting the logic for this
section in the `$then` section, and the rest of the VDU in the `$else`.

    {
      "validate_doc_update": {
        "$if": { "$newDoc._deleted": true },
        "$then": {
          "$or": [
            { "$userCtx.roles": { "$all": ["_admin"] } },
            { "$userCtx.name": { "$data": "$oldDoc.name" } }
          ],
          "$error": "forbidden",
          "$reason": "Only admins may delete other user docs."
        },
        "$else": {
          // ...
        }
      }
    }

After this there are a few basic checks on the structure of the document, one of
which compares it to the old document:

    if ((oldDoc && oldDoc.type !== 'user') || newDoc.type !== 'user') {
        throw({forbidden : 'doc.type must be user'});
    } // we only allow user docs for now

    if (!newDoc.name) {
        throw({forbidden: 'doc.name is required'});
    }

    if (!newDoc.roles) {
        throw({forbidden: 'doc.roles must exist'});
    }

    if (!isArray(newDoc.roles)) {
        throw({forbidden: 'doc.roles must be an array'});
    }

    if (newDoc._id !== ('org.couchdb.user:' + newDoc.name)) {
        throw({
            forbidden: 'Doc ID must be of the form org.couchdb.user:name'
        });
    }

    if (oldDoc) { // validate all updates
        if (oldDoc.name !== newDoc.name) {
            throw({forbidden: 'Usernames can not be changed.'});
        }
    }

    if (newDoc.password_sha && !newDoc.salt) {
        throw({
            forbidden: 'Users with password_sha must have a salt.' +
                'See /_utils/script/couch.js for example code.'
        });
    }

With the exception of the rule about the format of `_id`, which we'll deal with
shortly, these can be straightforwardly converted to declarative rules, so we
put the following in the `$else` section:


    {
      "$oldDoc": {
        "$or": [
          { "$exists": false },
          {
            "type": "user",
            "name": { "$data": "$newDoc.name" }
          }
        ]
      },
      "$newDoc": {
        "type": "user",
        "name": { "$exists": true },
        "roles": { "$type": "array" },
        "$if": { "password_sha": { "$exists": true } },
        "$then": { "salt": { "$exists": true } }
      }
    }

The `_id` format rule could be partially translated using `$beginsWith`:

    {
      "$newDoc": {
        "_id": { "$beginsWith": "org.couchdb.user:" }
      }
    }

However this doesn't capture that `_id` should equal
`org.couchdb.user:{doc.name}`. Possibly we could add an `$endsWith` operator and
express the rule like this:

    {
      "$newDoc": {
        "_id": {
          "$beginsWith": "org.couchdb.user:",
          "$endsWith": { "$data": $newDoc.name" }
        }
      }
    }

However, this would still allow values that begin and end with the expected
substrings, but have additional content between them. To force an exact match we
really need a way to express concatenation, then we could write:

    {
      "$newDoc": {
        "_id": { "$cat": ["org.couchdb.user:", { "$data": "$newDoc.name" }] }
      }
    }

`$cat` would take an array containing either literal values or `$data`
references, and produce the result of concatenating all of them. It should have
the same restrictions on where it can be used as `$data`, to avoid the input
document injecting operators.

After this, we have a function definition. The function is only invoked once, so
we could inline it in theory, but for the sake of defining a way to create
reusable functions, let's translate it.

    var is_server_or_database_admin = function(userCtx, secObj) {
        // see if the user is a server admin
        if(userCtx.roles.indexOf('_admin') !== -1) {
            return true; // a server admin
        }

        // see if the user a database admin specified by name
        if(secObj && secObj.admins && secObj.admins.names) {
            if(secObj.admins.names.indexOf(userCtx.name) !== -1) {
                return true; // database admin
            }
        }

        // see if the user a database admin specified by role
        if(secObj && secObj.admins && secObj.admins.roles) {
            var db_roles = secObj.admins.roles;
            for(var idx = 0; idx < userCtx.roles.length; idx++) {
                var user_role = userCtx.roles[idx];
                if(db_roles.indexOf(user_role) !== -1) {
                    return true; // role matches!
                }
            }
        }

        return false; // default to no admin
    }

This possibly looks quite complicated: we have use of array operations like
`indexOf()` and `length`, `if` statements, `for` loops, and so on. Surely we
don't need to replicate all the features of a programming language to express
the same thing?

What this function is checking boils down to, either:

- The user has the `_admin` rule
- The user's name is a member of the database's `admins` list
- Any of the user's roles is a member of the database's `admins` list

These requirements can be easily expressed using Mango. Taking a cue from JSON
Schema, let's invent a place to put reusable definitions in the design document,
and then refer to them from `validate_doc_update`. First, we put the desired
expressing in `defs` in the root of the design doc:

    "defs": {
      "is-server-or-database-admin": {
        "$or": [
          { "roles": { "$all": ["_admin"] } },
          { "name": { "$in": { "$data": "$secObj.admins.names" } } },
          {
            "roles": {
              "$elemMatch": { "$in": { "$data": "secObj.admins.roles" } }
            }
          }
        ]
      }
    }

We can then refer to this definition as a predicate that values must match, e.g.
`"$userCtx": { "$ref": "defs.is-server-or-database-admin" }` has the same
meaning as if the definition was inlined to produce:

    "$userCtx": {
      "$or": [
        { "roles": { "$all": ["_admin"] } },
        { "name": { "$in": { "$data": "$secObj.admins.names" } } },
        {
          "roles": {
            "$elemMatch": { "$in": { "$data": "secObj.admins.roles" } }
          }
        }
      ]
    }

We then have a section of logic that's conditional on this function:

    if (!is_server_or_database_admin(userCtx, secObj)) {
        if (oldDoc) { // validate non-admin updates
            if (userCtx.name !== newDoc.name) {
                throw({
                    forbidden: 'You may only update your own user document.'
                });
            }
            // validate role updates
            var oldRoles = oldDoc.roles.sort();
            var newRoles = newDoc.roles.sort();

            if (oldRoles.length !== newRoles.length) {
                throw({forbidden: 'Only _admin may edit roles'});
            }

            for (var i = 0; i < oldRoles.length; i++) {
                if (oldRoles[i] !== newRoles[i]) {
                    throw({forbidden: 'Only _admin may edit roles'});
                }
            }
        } else if (newDoc.roles.length > 0) {
            throw({forbidden: 'Only _admin may set roles'});
        }
    }

This again looks quite complicated, involving a bunch of iteration, sorting, and
conditional logic, but what this function is actually expressing is:

- If `oldDoc` exists (i.e. this is an update):
  - The doc `name` must match the user's `name`
  - The old and new docs' `roles` array must contain the same items
- Otherwise:
  - The doc's `roles` must be empty

The requirement that two arrays contain the same items, regardless of order, can
be expressed by saying that each of them must contain all the items in the
other. This can be done using the `$all` and `$data` operators.

    {
      "$if": { "$not": { "$userCtx": { "$ref": "defs.is-server-or-database-admin" } } },
      "$then": {
        "$if": { "$oldDoc": { "$exists": true } },
        "$then": {
          "$all": [
            {
              "$newDoc.name": { "$data": "$userCtx.name" },
              "$error": "forbidden",
              "$reason": "You may only update your own user document."
            },
            {
              "$oldDoc.roles": { "$all": { "$data": "$newDoc.roles" } },
              "$newDoc.roles": { "$all": { "$data": "$oldDoc.roles" } },
              "$error": "forbidden",
              "$reason": "Only _admin may edit roles"
            }
          ]
        },
        "$else": {
          {
            "$newDoc.roles": { "$size": 0 },
            "$error": "forbidden",
            "$reason": "Only _admin may set roles"
          }
        }
      }
    }

The final piece of the JavaScript VDU enforces a few rules about allowed
characters in certain fields:

    // no system roles in users db
    for (var i = 0; i < newDoc.roles.length; i++) {
        if (newDoc.roles[i][0] === '_') {
            throw({
                forbidden:
                'No system roles (starting with underscore) in users db.'
            });
        }
    }

    // no system names as names
    if (newDoc.name[0] === '_') {
        throw({forbidden: 'Username may not start with underscore.'});
    }

    var badUserNameChars = [':'];

    for (var i = 0; i < badUserNameChars.length; i++) {
        if (newDoc.name.indexOf(badUserNameChars[i]) >= 0) {
            throw({forbidden: 'Character `' + badUserNameChars[i] +
                    '` is not allowed in usernames.'});
        }
    }

Once again, this looks like we might need various string operations, array
indexing, and other programming language features to translate this, but what is
actually being expressed is quite simple:

- None of the doc's `roles` may begin with an underscore
- The doc's `name` must not begin with an underscore
- The doc's `name` must not contain a colon

These requirements can be translated very simply:

    "$newDoc": {
      "roles": { "$allMatch": { "$regex": "^[^_]" } },
      "name": { "$regex": "^[^_:][^:]*$" }
    }

If we want more detailed error messages for these checks, we could do something
like:

    "$newDoc": {
      "$all": [
        {
          "roles": { "$allMatch": { "$regex": "^[^_]" } },
          "name": { "$regex": "^[^_]" },
          "$error": "forbidden",
          "$reason": "Usernames and roles must not start with underscore"
        },
        {
          "name": { "$regex": "^[^:]*$" },
          "$error": "forbidden",
          "$reason": "Usernames must not contain the characters ':'"
        }
      ]
    }

Putting all this together, the full translation of the JavaScript VDU is:

    {
      "validate_doc_update": {
        "$if": { "$newDoc._deleted": true },
        "$then": {
          "$or": [
            { "$userCtx.roles": { "$all": ["_admin"] } },
            { "$userCtx.name": { "$data": "$oldDoc.name" } }
          ],
          "$error": "forbidden",
          "$reason": "Only admins may delete other user docs."
        },
        "$else": {
          {
            "$oldDoc": {
              "$or": [
                { "$exists": false },
                { "type": "user", "name": { "$data": "$newDoc.name" } }
              ]
            },
            "$newDoc": {
              "_id": { "$cat": ["org.couchdb.user:", { "$data": "$newDoc.name" }] },
              "type": "user",
              "name": { "$exists": true },
              "roles": { "$type": "array" },
              "$all": [
                {
                  "$if": { "password_sha": { "$exists": true } },
                  "$then": { "salt": { "$exists": true } },
                  "$error": "forbidden",
                  "$reason": "Users with password_sha must have a salt. See /_utils/script/couch.js for example code."
                },
                {
                  "roles": { "$allMatch": { "$regex": "^[^_]" } },
                  "name": { "$regex": "^[^_]" },
                  "$error": "forbidden",
                  "$reason": "Usernames and roles must not start with underscore"
                },
                {
                  "name": { "$regex": "^[^:]*$" },
                  "$error": "forbidden",
                  "$reason": "Usernames must not contain the characters ':'"
                }
              ]
            },
            {
              "$if": { "$not": { "$userCtx": { "$ref": "defs.is-server-or-database-admin" } } },
              "$then": {
                "$if": { "$oldDoc": { "$exists": true } },
                "$then": {
                  "$all": [
                    {
                      "$newDoc.name": { "$data": "$userCtx.name" },
                      "$error": "forbidden",
                      "$reason": "You may only update your own user document."
                    },
                    {
                      "$oldDoc.roles": { "$all": { "$data": "$newDoc.roles" } },
                      "$newDoc.roles": { "$all": { "$data": "$oldDoc.roles" } },
                      "$error": "forbidden",
                      "$reason": "Only _admin may edit roles"
                    }
                  ]
                },
                "$else": {
                  {
                    "$newDoc.roles": { "$size": 0 },
                    "$error": "forbidden",
                    "$reason": "Only _admin may set roles"
                  }
                }
              }
            }
          }
        }
      },
      "defs": {
        "is-server-or-database-admin": {
          "$or": [
            { "roles": { "$all": ["_admin"] } },
            { "name": { "$in": { "$data": "$secObj.admins.names" } } },
            {
              "roles": {
                "$elemMatch": { "$in": { "$data": "secObj.admins.roles" } }
              }
            }
          ]
        }
      }
    }

This example shows that translation of most common needs for VDUs into the Mango
language, with the additions we have suggested, is fairly straightforward. It
may benefit from the addition of further operators, for example some more set
operations. We already have `$all` which functionally checks if a value is a
superset of the given array, but checking that two lists are "set equal"
(contain the same elements in any order), or that an array contains no
duplicates, may be beneficial.

However, everything in the example VDU _can_ be expressed using just the
operators we've already suggested, and we would recommend deferring the addition
of any more operators until more production experience with this system has been
gathered.


# Key Changes

[TIP]: # ( If the changes will affect how a user interacts with CouchDB, explain. )

## Applications and Modules affected

[NOTE]: # ( List the OTP applications or functional modules in CouchDB affected by the proposal. )

## HTTP API additions

[NOTE]: # ( Provide *exact* detail on each new API endpoint, including: )
[NOTE]: # (   HTTP methods [HEAD, GET, PUT, POST, DELETE, etc.] )
[NOTE]: # (   Synopsis of functionality )
[NOTE]: # (   Headers and parameters accepted )
[NOTE]: # (   JSON in [if a PUT or POST type] )
[NOTE]: # (   JSON out )
[NOTE]: # (   Valid status codes and their definitions )
[NOTE]: # (   A proposed Request and Response block )

## HTTP API deprecations

[NOTE]: # ( Provide *exact* detail on the API endpoints to be deprecated. )
[NOTE]: # ( If these endpoints are replaced by new endpoints, list those as well. )
[NOTE]: # ( State the proposed version in which the deprecation and removal will occur. )

# Security Considerations

[NOTE]: # ( Include any impact to the security of CouchDB here. )

# References

[TIP]:  # ( Include any references to CouchDB documentation, mailing list discussion, )
[TIP]:  # ( external standards or other links here. )

# Acknowledgements

[TIP]:  # ( Who helped you write this RFC? )
