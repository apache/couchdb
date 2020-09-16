Mango
=====

A MongoDB inspired query language interface for Apache CouchDB.


Motivation
----------

Mango provides a single HTTP API endpoint that accepts JSON bodies via
HTTP POST. These bodies provide a set of instructions that will be
handled with the results being returned to the client in the same
order as they were specified. The general principle of this API is to
be simple to implement on the client side while providing users a more
natural conversion to Apache CouchDB than would otherwise exist using
the standard RESTful HTTP interface that already exists.


Actions
-------

The general API exposes a set of actions that are similar to what
MongoDB exposes (although not all of MongoDB's API is
supported). These are meant to be loosely and obviously inspired by
MongoDB but without too much attention to maintaining the exact
behavior.

Each action is specified as a JSON object with a number of keys that
affect the behavior. Each action object has at least one field named
"action" which must have a string value indicating the action to be
performed. For each action there are zero or more fields that will
affect behavior. Some of these fields are required and some are
optional.

For convenience, the HTTP API will accept a JSON body that is either a
single JSON object which specifies a single action or a JSON array
that specifies a list of actions that will then be invoked
serially. While multiple commands can be batched into a single HTTP
request, there are no guarantees about atomicity or isolation for a
batch of commands.

Activating Query on a cluster
--------------------------------------------

Query can be enabled by setting the following config:

```
rpc:multicall(config, set, ["native_query_servers", "query", "{mango_native_proc, start_link, []}"]).
```

HTTP API
========

This API adds a single URI endpoint to the existing CouchDB HTTP
API. Creating databases, authentication, Map/Reduce views, etc are all
still supported exactly as currently document. No existing behavior is
changed.

The endpoint added is for the URL pattern `/dbname/_query` and has the
following characteristics:

* The only HTTP method supported is `POST`.
* The request `Content-Type` must be `application/json`.
* The response status code will either be `200`, `4XX`, or `5XX`
* The response `Content-Type` will be `application/json`
* The response `Transfer-Encoding` will be `chunked`.
* The response is a single JSON object or array that matches to the
  single command or list of commands that exist in the request.

This is intended to be a significantly simpler use of HTTP than the
current APIs. This is motivated by the fact that this entire API is
aimed at customers who are not as savvy at HTTP or non-relational
document stores. Once a customer is comfortable using this API we hope
to expose any other "power features" through the existing HTTP API and
its adherence to HTTP semantics.


Supported Actions
=================

This is a list of supported actions that Mango understands. For the
time being it is limited to the four normal CRUD actions plus one meta
action to create indices on the database.

insert
------

Insert a document or documents into the database.

Keys:

* action - "insert"
* docs - The JSON document to insert
* w (optional) (default: 2) - An integer > 0 for the write quorum size

If the provided document or documents do not contain an "\_id" field
one will be added using an automatically generated UUID.

It is more performant to specify multiple documents in the "docs"
field than it is to specify multiple independent insert actions. Each
insert action is submitted as a single bulk update (ie, \_bulk\_docs
in CouchDB terminology). This, however, does not make any guarantees
on the isolation or atomicity of the bulk operation. It is merely a
performance benefit.


find
----

Retrieve documents from the database.

Keys:

* action - "find"
* selector - JSON object following selector syntax, described below
* limit (optional) (default: 25) - integer >= 0, Limit the number of
  rows returned
* skip (optional) (default: 0) - integer >= 0, Skip the specified
  number of rows
* sort (optional) (default: []) - JSON array following sort syntax,
  described below
* fields (optional) (default: null) - JSON array following the field
  syntax, described below
* r (optional) (default: 1) - By default a find will return the
  document that was found when traversing the index. Optionally there
  can be a quorum read for each document using `r` as the read
  quorum. This is obviously less performant than using the document
  local to the index.
* conflicts (optional) (default: false) - boolean, whether or not to
  include information about any existing conflicts for the document.

The important thing to note about the find command is that it must
execute over a generated index. If a selector is provided that cannot
be satisfied using an existing index the list of basic indices that
could be used will be returned.

For the most part, indices are generated in response to the
"create\_index" action (described below) although there are two
special indices that can be used as well. The "\_id" is automatically
indexed and is similar to every other index. There is also a special
"\_seq" index to retrieve documents in the order of their update
sequence.

Its also quite possible to generate a query that can't be satisfied by
any index. In this case an error will be returned stating that
fact. Generally speaking the easiest way to stumble onto this is to
attempt to OR two separate fields which would require a complete table
scan. In the future I expect to support these more complicated queries
using an extended indexing API (which deviates from the current
MongoDB model a bit).


update
------

Update an existing document in the database

Keys:

* action - "update"
* selector - JSON object following selector syntax, described below
* update - JSON object following update syntax, described below
* upsert - (optional) (default: false) - boolean, Whether or not to
  create a new document if the selector does not match any documents
  in the database
* limit (optional) (default: 1) - integer > 0, How many documents
  returned from the selector should be modified. Currently has a
  maximum value of 100
* sort - (optional) (default: []) - JSON array following sort syntax,
  described below
* r (optional) (default: 1) - integer > 0, read quorum constant
* w (optional) (default: 2) - integer > 0, write quorum constant

Updates are fairly straightforward other than to mention that the
selector (like find) must be satisifiable using an existing index.

On the update field, if the provided JSON object has one or more
update operator (described below) then the operation is applied onto
the existing document (if one exists) else the entire contents are
replaced with exactly the value of the `update` field.


delete
------

Remove a document from the database.

Keys:

* action - "delete"
* selector - JSON object following selector syntax, described below
* force (optional) (default: false) - Delete all conflicted versions
  of the document as well
* limit - (optional) (default: 1) - integer > 0, How many documents to
  delete from the database. Currently has a maximum value of 100
* sort - (optional) (default: []) - JSON array following sort syntax,
  described below
* r (optional) (default: 1) - integer > 1, read quorum constant
* w (optional) (default: 2) - integer > 0, write quorum constant

Deletes behave quite similarly to update except they attempt to remove
documents from the database. Its important to note that if a document
has conflicts it may "appear" that delete's aren't having an
effect. This is because the delete operation by default only removes a
single revision. Specify `"force":true` if you would like to attempt
to delete all live revisions.

If you wish to delete a specific revision of the document, you can
specify it in the selector using the special "\_rev" field.


create\_index
-------------

Create an index on the database

Keys:

* action - "create\_index"
* index - JSON array following sort syntax, described below
* type (optional) (default: "json") - string, specifying the index
  type to create. Currently only "json" indexes are supported but in
  the future we will provide full-text indexes as well as Geo spatial
  indexes
* name (optional) - string, optionally specify a name for the
  index. If a name is not provided one will be automatically generated
* ddoc (optional) - Indexes can be grouped into design documents
  underneath the hood for efficiency. This is an advanced
  feature. Don't specify a design document here unless you know the
  consequences of index invalidation. By default each index is placed
  in its own separate design document for isolation.

Anytime an operation is required to locate a document in the database
it is required that an index must exist that can be used to locate
it. By default the only two indices that exist are for the document
"\_id" and the special "\_seq" index.

Indices are created in the background. If you attempt to create an
index on a large database and then immediately utilize it, the request
may block for a considerable amount of time before the request
completes.

Indices can specify multiple fields to index simultaneously. This is
roughly analogous to a compound index in SQL with the corresponding
tradeoffs. For instance, an index may contain the (ordered set of)
fields "foo", "bar", and "baz". If a selector specifying "bar" is
received, it can not be answered. Although if a selector specifying
"foo" and "bar" is received, it can be answered more efficiently than
if there were only an index on "foo" and "bar" independently.

NB: while the index allows the ability to specify sort directions
these are currently not supported. The sort direction must currently
be specified as "asc" in the JSON. [INTERNAL]: This will require that
we patch the view engine as well as the cluster coordinators in Fabric
to follow the specified sort orders. The concepts are straightforward
but the implementation may need some thought to fit into the current
shape of things.


list\_indexes
-------------

List the indexes that exist in a given database.

Keys:

* action - "list\_indexes"


delete\_index
-------------

Delete the specified index from the database.

Keys:

* action - "delete\_index"
* name - string, the index to delete
* design\_doc - string, the design doc id from which to delete the
  index. For auto-generated index names and design docs, you can
  retrieve this information from the `list\_indexes` action

Indexes require resources to maintain. If you find that an index is no
longer necessary then it can be beneficial to remove it from the
database.


describe\_selector
------------------

Shows debugging information for a given selector

Keys:

* action - "describe\_selector"
* selector - JSON object in selector syntax, described below
* extended (optional) (default: false) - Show information on what
  existing indexes could be used with this selector

This is a useful debugging utility that will show how a given selector
is normalized before execution as well as information on what indexes
could be used to satisfy it.

If `"extended": true` is included then the list of existing indices
that could be used for this selector are also returned.



JSON Syntax Descriptions
========================

This API uses a few defined JSON structures for various
operations. Here we'll describe each in detail.


Selector Syntax
---------------

The Mango query language is expressed as a JSON object describing
documents of interest. Within this structure it is also possible to
express conditional logic using specially named fields. This is
inspired by and intended to maintain a fairly close parity to the
existing MongoDB behavior.

As an example, the simplest selector for Mango might look something like such:

```json
    {"_id": "Paul"}
```

Which would match the document named "Paul" (if one exists). Extending
this example using other fields might look like such:

```json
    {"_id": "Paul", "location": "Boston"}
```

This would match a document named "Paul" *AND* having a "location"
value of "Boston". Seeing as though I'm sitting in my basement in
Omaha, this is unlikely.

There are two special syntax elements for the object keys in a
selector. The first is that the period (full stop, or simply `.`)
character denotes subfields in a document. For instance, here are two
equivalent examples:

    {"location": {"city": "Omaha"}}
    {"location.city": "Omaha"}

If the object's key contains the period it could be escaped with backslash, i.e.

    {"location\\.city": "Omaha"}

Note that the double backslash here is necessary to encode an actual
single backslash.

The second important syntax element is the use of a dollar sign (`$`)
prefix to denote operators. For example:

    {"age": {"$gt": 21}}

In this example, we have created the boolean expression `age > 21`.

There are two core types of operators in the selector syntax:
combination operators and condition operators. In general, combination
operators contain groups of condition operators. We'll describe the
list of each below.

### Implicit Operators

For the most part every operator must be of the form `{"$operator":
argument}`. Though there are two implicit operators for selectors.

First, any JSON object that is not the argument to a condition
operator is an implicit `$and` operator on each field. For instance,
these two examples are identical:

    {"foo": "bar", "baz": true}
    {"$and": [{"foo": {"$eq": "bar"}}, {"baz": {"$eq": true}}]}

And as shown, any field that contains a JSON value that has no
operators in it is an equality condition. For instance, these are
equivalent:

    {"foo": "bar"}
    {"foo": {"$eq": "bar"}}

And to be clear, these are also equivalent:

    {"foo": {"bar": "baz"}}
    {"foo": {"$eq": {"bar": "baz"}}}

Although, the previous example would actually be normalized internally to this:

    {"foo.bar": {"$eq": "baz"}}


### Combination Operators

These operators are responsible for combining groups of condition
operators. Most familiar are the standard boolean operators plus a few
extra for working with JSON arrays.

Each of the combining operators take a single argument that is either
a condition operator or an array of condition operators.

The list of combining characters:

* "$and" - array argument
* "$or" - array argument
* "$not" - single argument
* "$nor" - array argument
* "$all" - array argument (special operator for array values)
* "$elemMatch" - single argument (special operator for array values)
* "$allMatch" - single argument (special operator for array values)

### Condition Operators

Condition operators are specified on a per field basis and apply to
the value indexed for that field. For instance, the basic "$eq"
operator matches when the indexed field is equal to its
argument. There is currently support for the basic equality and
inequality operators as well as a number of meta operators. Some of
these operators will accept any JSON argument while some require a
specific JSON formatted argument. Each is noted below.

The list of conditional arguments:

(In)equality operators

* "$lt" - any JSON
* "$lte" - any JSON
* "$eq" - any JSON
* "$ne" - any JSON
* "$gte" - any JSON
* "$gt" - any JSON

Object related operators

* "$exists" - boolean, check whether the field exists or not
  regardless of its value
* "$type" - string, check the document field's type

Array related operators

* "$in" - array of JSON values, the document field must exist in the
  list provided
* "$nin" - array of JSON values, the document field must not exist in
  the list provided
* "$size" - integer, special condition to match the length of an array
  field in a document. Non-array fields cannot match this condition.

Misc related operators

* "$mod" - [Divisor, Remainder], where Divisor and Remainder are both
  positive integers (ie, greater than 0). Matches documents where
  (field % Divisor == Remainder) is true. This is false for any
  non-integer field
* "$regex" - string, a regular expression pattern to match against the
  document field. Only matches when the field is a string value and
  matches the supplied matches


Update Syntax
-------------

Need to describe the syntax for update operators.


Sort Syntax
-----------

The sort syntax is a basic array of field name and direction pairs. It
looks like such:

    [{field1: dir1} | ...]

Where field1 can be any field (dotted notation is available for
sub-document fields) and dir1 can be "asc" or "desc".

Note that it is highly recommended that you specify a single key per
object in your sort ordering so that the order is not dependent on the
combination of JSON libraries between your application and the
internals of Mango's indexing engine.


Fields Syntax
-------------

When retrieving documents from the database you can specify that only
a subset of the fields are returned. This allows you to limit your
results strictly to the parts of the document that are interesting for
the local application logic. The fields returned are specified as an
array. Unlike MongoDB only the fields specified are included, there is
no automatic inclusion of the "\_id" or other metadata fields when a
field list is included.

A trivial example:

    ["foo", "bar", "baz"]


HTTP API
========

Short summary until the full documentation can be brought over.

POST /dbname/\_find
-------------------------

Issue a query.

Request body is a JSON object that has the selector and the various
options like limit/skip etc. Or we could post the selector and put the
other options into the query string. Though I'd probably prefer to
have it all in the body for consistency.

Response is streamed out like a view.

POST /dbname/\_index
--------------------------

Request body contains the index definition.

Response body is empty and the result is returned as the status code
(200 OK -> created, 3something for exists).

GET /dbname/\_index
-------------------------

Request body is empty.

Response body is all of the indexes that are available for use by find.

DELETE /dbname/\_index/ddocid/viewname
--------------------------------------------

Remove the specified index.

Request body is empty.

Response body is empty. The status code gives enough information.
