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


.. _views/json:

================
Joins With Views
================

Linked Documents
================

If your :ref:`map function <mapfun>` emits an object value which has
``{'_id': XXX}`` and you :ref:`query view <api/ddoc/view>` with
``include_docs=true`` parameter, then CouchDB will fetch the document with id
``XXX`` rather than the document which was processed to emit the key/value pair.

This means that if one document contains the ids of other documents, it can
cause those documents to be fetched in the view too, adjacent to the same key
if required.

For example, if you have the following hierarchically-linked documents:

.. code-block:: javascript

  [
    { "_id": "11111" },
    { "_id": "22222", "ancestors": ["11111"], "value": "hello" },
    { "_id": "33333", "ancestors": ["22222","11111"], "value": "world" }
  ]

You can emit the values with the ancestor documents adjacent to them in the view
like this:

.. code-block:: javascript

  function(doc) {
    if (doc.value) {
      emit([doc.value, 0], null);
      if (doc.ancestors) {
        for (var i in doc.ancestors) {
          emit([doc.value, Number(i)+1], {_id: doc.ancestors[i]});
        }
      }
    }
  }

The result you get is:

.. code-block:: javascript

  {
      "total_rows": 5,
      "offset": 0,
      "rows": [
          {
              "id": "22222",
              "key": [
                  "hello",
                  0
              ],
              "value": null,
              "doc": {
                  "_id": "22222",
                  "_rev": "1-0eee81fecb5aa4f51e285c621271ff02",
                  "ancestors": [
                      "11111"
                  ],
                  "value": "hello"
              }
          },
          {
              "id": "22222",
              "key": [
                  "hello",
                  1
              ],
              "value": {
                  "_id": "11111"
              },
              "doc": {
                  "_id": "11111",
                  "_rev": "1-967a00dff5e02add41819138abb3284d"
              }
          },
          {
              "id": "33333",
              "key": [
                  "world",
                  0
              ],
              "value": null,
              "doc": {
                  "_id": "33333",
                  "_rev": "1-11e42b44fdb3d3784602eca7c0332a43",
                  "ancestors": [
                      "22222",
                      "11111"
                  ],
                  "value": "world"
              }
          },
          {
              "id": "33333",
              "key": [
                  "world",
                  1
              ],
              "value": {
                  "_id": "22222"
              },
              "doc": {
                  "_id": "22222",
                  "_rev": "1-0eee81fecb5aa4f51e285c621271ff02",
                  "ancestors": [
                      "11111"
                  ],
                  "value": "hello"
              }
          },
          {
              "id": "33333",
              "key": [
                  "world",
                  2
              ],
              "value": {
                  "_id": "11111"
              },
              "doc": {
                  "_id": "11111",
                  "_rev": "1-967a00dff5e02add41819138abb3284d"
              }
          }
      ]
  }

which makes it very cheap to fetch a document plus all its ancestors in one
query.

Note that the ``"id"`` in the row is still that of the originating document.
The only difference is that ``include_docs`` fetches a different doc.

The current revision of the document is resolved at query time, not at the time
the view is generated. This means that if a new revision of the linked document
is added later, it will appear in view queries even though the view itself
hasn't changed. To force a specific revision of a linked document to be used,
emit a ``"_rev"`` property as well as ``"_id"``.


Using View Collation
====================

Just today, there was a discussion on IRC how you'd go about modeling a simple
blogging system with “post” and “comment” entities, where any blog post might
have N comments. If you'd be using an SQL database, you'd obviously have two
tables with foreign keys and you'd be using joins. (At least until you needed
to add some `denormalization`_).

.. _denormalization: http://en.wikipedia.org/wiki/Denormalization

But what would the “obvious” approach in CouchDB look like?

Approach #1: Comments Inlined
-----------------------------

A simple approach would be to have one document per blog post, and store the
comments inside that document:

.. code-block:: javascript

  {
    "_id": "myslug",
    "_rev": "123456",
    "author": "john",
    "title": "My blog post",
    "content": "Bla bla bla …",
    "comments": [
      {"author": "jack", "content": "…"},
      {"author": "jane", "content": "…"}
    ]
  }

.. note::
   Of course the model of an actual blogging system would be more extensive,
   you'd have tags, timestamps, etc etc. This is just to demonstrate the basics.

The obvious advantage of this approach is that the data that belongs together
is stored in one place. Delete the post, and you automatically delete the
corresponding comments, and so on.

You may be thinking that putting the comments inside the blog post document
would not allow us to query for the comments themselves, but you'd be wrong.
You could trivially write a CouchDB view that would return all comments across
all blog posts, keyed by author:

.. code-block:: javascript

  function(doc) {
    for (var i in doc.comments) {
      emit(doc.comments[i].author, doc.comments[i].content);
    }
  }

Now you could list all comments by a particular user by invoking the view and
passing it a ``?key="username"`` query string parameter.

However, this approach has a drawback that can be quite significant for many
applications: To add a comment to a post, you need to:

- Fetch the blog post document
- Add the new comment to the JSON structure
- Send the updated document to the server

Now if you have multiple client processes adding comments at roughly the same
time, some of them will get a `HTTP 409 Conflict` error on step 3 (that's
optimistic concurrency in action). For some applications this makes sense, but
in many other apps, you'd want to append new related data regardless of whether
other data has been added in the meantime.

The only way to allow non-conflicting addition of related data is by putting
that related data into separate documents.

Approach #2: Comments Separate
------------------------------

Using this approach you'd have one document per blog post, and one document per
comment. The comment documents would have a “backlink” to the post they belong
to.

The blog post document would look similar to the above, minus the comments
property. Also, we'd now have a type property on all our documents so that we
can tell the difference between posts and comments:

.. code-block:: javascript

  {
    "_id": "myslug",
    "_rev": "123456",
    "type": "post",
    "author": "john",
    "title": "My blog post",
    "content": "Bla bla bla …"
  }

The comments themselves are stored in separate documents, which also have a type
property (this time with the value “comment”), and in addition feature a post
property containing the ID of the post document they belong to:

.. code-block:: javascript

  {
    "_id": "ABCDEF",
    "_rev": "123456",
    "type": "comment",
    "post": "myslug",
    "author": "jack",
    "content": "…"
  }

.. code-block:: javascript

  {
    "_id": "DEFABC",
    "_rev": "123456",
    "type": "comment",
    "post": "myslug",
    "author": "jane",
    "content": "…"
  }

To list all comments per blog post, you'd add a simple view, keyed by blog post
ID:

.. code-block:: javascript

  function(doc) {
    if (doc.type == "comment") {
      emit(doc.post, {author: doc.author, content: doc.content});
    }
  }

And you'd invoke that view passing it a ``?key="post_id"`` query string
parameter.

Viewing all comments by author is just as easy as before:

.. code-block:: javascript

  function(doc) {
    if (doc.type == "comment") {
      emit(doc.author, {post: doc.post, content: doc.content});
    }
  }

So this is better in some ways, but it also has a disadvantage.
Imagine you want to display a blog post with all the associated comments on the
same web page. With our first approach, we needed just a single request to the
CouchDB server, namely a ``GET`` request to the document. With this second
approach, we need two requests: a ``GET`` request to the post document, and a
``GET`` request to the view that returns all comments for the post.

That is okay, but not quite satisfactory. Just imagine you wanted to added
threaded comments: you'd now need an additional fetch per comment. What we'd
probably want then would be a way to join the blog post and the various comments
together to be able to retrieve them with a single HTTP request.

This was when Damien Katz, the author of CouchDB, chimed in to the discussion
on IRC to show us the way.

Optimization: Using the Power of View Collation
-----------------------------------------------

Obvious to Damien, but not at all obvious to the rest of us: it's fairly simple
to make a view that includes both the content of the blog post document, and
the content of all the comments associated with that post. The way you do that
is by using `complex keys`. Until now we've been using simple string values for
the view keys, but in fact they can be arbitrary JSON values, so let's make
some use of that:

.. code-block:: javascript

  function(doc) {
    if (doc.type == "post") {
      emit([doc._id, 0], doc);
    } else if (doc.type == "comment") {
      emit([doc.post, 1], doc);
    }
  }

Okay, this may be confusing at first. Let's take a step back and look at what
views in CouchDB are really about.

CouchDB views are basically highly efficient on-disk dictionaries that map keys
to values, where the key is automatically indexed and can be used to filter
and/or sort the results you get back from your views. When you “invoke” a view,
you can say that you're only interested in a subset of the view rows by
specifying a ``?key=foo`` query string parameter. Or you can specify
``?startkey=foo`` and/or ``?endkey=bar`` query string parameters to fetch rows
over a range of keys.

It's also important to note that keys are always used for collating (i.e.
sorting) the rows. CouchDB has well defined (but as of yet undocumented) rules
for comparing arbitrary JSON objects for collation. For example, the JSON value
``["foo", 2]`` is sorted after (considered “greater than”) the values
``["foo"]`` or ``["foo", 1, "bar"]``, but before e.g. ``["foo", 2, "bar"]``.
This feature enables a whole class of tricks that are rather non-obvious...

.. seealso::

  :ref:`views/collation`

With that in mind, let's return to the view function above. First note that,
unlike the previous view functions we've used here, this view handles both
"post" and "comment" documents, and both of them end up as rows in the same
view. Also, the key in this view is not just a simple string, but an array.
The first element in that array is always the ID of the post, regardless of
whether we're processing an actual post document, or a comment associated with
a post. The second element is 0 for post documents, and 1 for comment documents.

Let's assume we have two blog posts in our database. Without limiting the view
results via ``key``, ``startkey``, or ``endkey``, we'd get back something like
the following:

.. code-block:: javascript

  {
    "total_rows": 5, "offset": 0, "rows": [{
        "id": "myslug",
        "key": ["myslug", 0],
        "value": {...}
      }, {
        "id": "ABCDEF",
        "key": ["myslug", 1],
        "value": {...}
      }, {
        "id": "DEFABC",
        "key": ["myslug", 1],
        "value": {...}
      }, {
        "id": "other_slug",
        "key": ["other_slug", 0],
        "value": {...}
      }, {
        "id": "CDEFAB",
        "key": ["other_slug", 1],
        "value": {...}
      },
    ]
  }

.. note::
   The ``...`` placeholder here would contain the complete JSON encoding of the
   corresponding document

Now, to get a specific blog post and all associated comments, we'd invoke that
view with the query string::

  ?startkey=["myslug"]&endkey;=["myslug", 2]

We'd get back the first three rows, those that belong to the ``myslug`` post,
but not the others. Et voila, we now have the data we need to display a post
with all associated comments, retrieved via a single ``GET`` request.

You may be asking what the 0 and 1 parts of the keys are for. They're simply
to ensure that the post document is always sorted before the the associated
comment documents. So when you get back the results from this view for a
specific post, you'll know that the first row contains the data for the blog
post itself, and the remaining rows contain the comment data.

One remaining problem with this model is that comments are not ordered, but
that's simply because we don't have date/time information associated with them.
If we had, we'd add the timestamp as third element of the key array, probably
as ISO date/time strings. Now we would continue using the query string
``?startkey=["myslug"]&endkey=["myslug", 2]`` to fetch the blog post and all
associated comments, only now they'd be in chronological order.
