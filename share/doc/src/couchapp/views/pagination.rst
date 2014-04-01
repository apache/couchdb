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


.. _views/pagination:

=================
Pagination Recipe
=================

This recipe explains how to paginate over view results.
Pagination is a user interface (UI) pattern that allows the display of a
large number of rows (`the result set`) without loading all the rows into the
UI at once. A fixed-size subset, the `page`, is displayed along with next and
previous links or buttons that can move the `viewport` over the result set to
an adjacent page.

We assume you’re familiar with creating and querying documents and views as
well as the multiple view query options.

Example Data
============

To have some data to work with, we’ll create a list of bands,
one document per band::

  { "name":"Biffy Clyro" }

  { "name":"Foo Fighters" }

  { "name":"Tool" }

  { "name":"Nirvana" }

  { "name":"Helmet" }

  { "name":"Tenacious D" }

  { "name":"Future of the Left" }

  { "name":"A Perfect Circle" }

  { "name":"Silverchair" }

  { "name":"Queens of the Stone Age" }

  { "name":"Kerub" }

A View
=======

We need a simple map function that gives us an alphabetical list of band
names. This should be easy, but we’re adding extra smarts to filter out “The”
and “A” in front of band names to put them into the right position:

.. code-block:: javascript

  function(doc) {
    if(doc.name) {
      var name = doc.name.replace(/^(A|The) /, "");
      emit(name, null);
    }
  }

The views result is an alphabetical list of band names. Now say we want to
display band names five at a time and have a link pointing to the next five
names that make up one page, and a link for the previous five,
if we’re not on the first page.

We learned how to use the ``startkey``, ``limit``, and ``skip`` parameters in
earlier documents. We’ll use these again here. First, let’s have a look at
the full result set:

.. code-block:: javascript

  {"total_rows":11,"offset":0,"rows":[
    {"id":"a0746072bba60a62b01209f467ca4fe2","key":"Biffy Clyro","value":null},
    {"id":"b47d82284969f10cd1b6ea460ad62d00","key":"Foo Fighters","value":null},
    {"id":"45ccde324611f86ad4932555dea7fce0","key":"Tenacious D","value":null},
    {"id":"d7ab24bb3489a9010c7d1a2087a4a9e4","key":"Future of the Left","value":null},
    {"id":"ad2f85ef87f5a9a65db5b3a75a03cd82","key":"Helmet","value":null},
    {"id":"a2f31cfa68118a6ae9d35444fcb1a3cf","key":"Nirvana","value":null},
    {"id":"67373171d0f626b811bdc34e92e77901","key":"Kerub","value":null},
    {"id":"3e1b84630c384f6aef1a5c50a81e4a34","key":"Perfect Circle","value":null},
    {"id":"84a371a7b8414237fad1b6aaf68cd16a","key":"Queens of the Stone Age","value":null},
    {"id":"dcdaf08242a4be7da1a36e25f4f0b022","key":"Silverchair","value":null},
    {"id":"fd590d4ad53771db47b0406054f02243","key":"Tool","value":null}
  ]}

Setup
=====

The mechanics of paging are very simple:

- Display first page
- If there are more rows to show, show next link
- Draw subsequent page
- If this is not the first page, show a previous link
- If there are more rows to show, show next link

Or in a pseudo-JavaScript snippet:

.. code-block:: javascript


  var result = new Result();
  var page = result.getPage();

  page.display();

  if(result.hasPrev()) {
    page.display_link('prev');
  }

  if(result.hasNext()) {
    page.display_link('next');
  }

Paging
======

To get the first five rows from the view result, you use the ``?limit=5``
query parameter::

  curl -X GET http://127.0.0.1:5984/artists/_design/artists/_view/by-name?limit=5

The result:

.. code-block:: javascript

  {"total_rows":11,"offset":0,"rows":[
    {"id":"a0746072bba60a62b01209f467ca4fe2","key":"Biffy Clyro","value":null},
    {"id":"b47d82284969f10cd1b6ea460ad62d00","key":"Foo Fighters","value":null},
    {"id":"45ccde324611f86ad4932555dea7fce0","key":"Tenacious D","value":null},
    {"id":"d7ab24bb3489a9010c7d1a2087a4a9e4","key":"Future of the Left","value":null},
    {"id":"ad2f85ef87f5a9a65db5b3a75a03cd82","key":"Helmet","value":null}
  ]}

By comparing the ``total_rows`` value to our ``limit`` value,
we can determine if there are more pages to display. We also know by the
`offset` member that we are on the first page. We can calculate the value for
``skip=`` to get the results for the next page:

.. code-block:: javascript

  var rows_per_page = 5;
  var page = (offset / rows_per_page) + 1; // == 1
  var skip = page * rows_per_page; // == 5 for the first page, 10 for the second ...

So we query CouchDB with::

  curl -X GET 'http://127.0.0.1:5984/artists/_design/artists/_view/by-name?limit=5&skip=5'

Note we have to use ``'`` (single quotes) to escape the ``&`` character that is
special to the shell we execute curl in.

The result:

.. code-block:: javascript

  {"total_rows":11,"offset":5,"rows":[
    {"id":"a2f31cfa68118a6ae9d35444fcb1a3cf","key":"Nirvana","value":null},
    {"id":"67373171d0f626b811bdc34e92e77901","key":"Kerub","value":null},
    {"id":"3e1b84630c384f6aef1a5c50a81e4a34","key":"Perfect Circle","value":null},
    {"id":"84a371a7b8414237fad1b6aaf68cd16a","key":"Queens of the Stone Age",
    "value":null},
    {"id":"dcdaf08242a4be7da1a36e25f4f0b022","key":"Silverchair","value":null}
  ]}

Implementing the ``hasPrev()`` and ``hasNext()`` method is pretty
straightforward:

.. code-block:: javascript

  function hasPrev()
  {
    return page > 1;
  }

  function hasNext()
  {
    var last_page = Math.floor(total_rows / rows_per_page) +
      (total_rows % rows_per_page);
    return page != last_page;
  }

Paging (Alternate Method)
=========================

The method described above performed poorly with large skip values until
CouchDB 1.2. Additionally, some use cases may call for the following
alternate method even with newer versions of CouchDB. One such case is when
duplicate results should be prevented. Using skip alone it is possible for
new documents to be inserted during pagination which could change the offset
of the start of the subsequent page.

A correct solution is not much harder. Instead of slicing the result set
into equally sized pages, we look at 10 rows at a time and use ``startkey`` to
jump to the next 10 rows. We even use skip, but only with the value 1.

Here is how it works:

- Request `rows_per_page + 1` rows from the view
- Display `rows_per_page` rows, `store + 1` row as `next_startkey` and
  `next_startkey_docid`
- As page information, keep ``startkey`` and `next_startkey`
- Use the `next_*` values to create the next link, and use the others to
  create the previous link

The trick to finding the next page is pretty simple. Instead of requesting 10
rows for a page, you request 11 rows, but display only 10 and use the values
in the 11th row as the ``startkey`` for the next page. Populating the link to
the previous page is as simple as carrying the current ``startkey`` over to the
next page. If there’s no previous ``startkey``, we are on the first page. We
stop displaying the link to the next page if we get `rows_per_page` or less
rows back. This is called linked list pagination, as we go from page to
page, or list item to list item, instead of jumping directly to a
pre-computed page. There is one caveat, though. Can you spot it?

CouchDB view keys do not have to be unique; you can have multiple index
entries read. What if you have more index entries for a key than rows that
should be on a page? ``startkey`` jumps to the first row, and you’d be screwed
if CouchDB didn’t have an additional parameter for you to use. All view keys
with the same value are internally sorted by `docid`, that is, the ID of
the document that created that view row. You can use the ``startkey_docid``
and ``endkey_docid`` parameters to get subsets of these rows. For
pagination, we still don’t need ``endkey_docid``, but ``startkey_docid`` is very
handy. In addition to ``startkey`` and ``limit``, you also use
``startkey_docid`` for pagination if, and only if, the extra row you fetch to
find the next page has the same key as the current ``startkey``.

It is important to note that the `*_docid` parameters only work in addition to
the `*key` parameters and are only useful to further narrow down the result set
of a view for a single key. They do not work on their own (the one exception
being the built-in :ref:`_all_docs view <api/db/all_docs>`  that already sorts
by document ID).

The advantage of this approach is that all the key operations can be
performed on the super-fast B-tree index behind the view. Looking up a page
doesn’t include scanning through hundreds and thousands of rows unnecessarily.

Jump to Page
============

One drawback of the linked list style pagination is that you can’t
pre-compute the rows for a particular page from the page number and the rows
per page. Jumping to a specific page doesn’t really work. Our gut reaction,
if that concern is raised, is, “Not even Google is doing that!” and we tend
to get away with it. Google always pretends on the first page to find 10 more
pages of results. Only if you click on the second page (something very few
people actually do) might Google display a reduced set of pages. If you page
through the results, you get links for the previous and next 10 pages,
but no more. Pre-computing the necessary ``startkey`` and ``startkey_docid``
for 20 pages is a feasible operation and a pragmatic optimization to know the
rows for every page in a result set that is potentially tens of thousands
of rows long, or more.

If you really do need to jump to a page over the full range of documents (we
have seen applications that require that), you can still maintain an integer
value index as the view index and take a hybrid approach at solving pagination.
