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

.. _best-practices/views:

====================
View recommendations
====================

Here are some tips and tricks for working with CouchDB's (JavaScript-based)
views.

Deploying a view change in a live environment
---------------------------------------------

It is possible to change the definition of a view, build the index, then make
those changes go live without causing downtime for your application. The trick
to making this work is that CouchDB's JavaScript view index files are based on
the contents of the design document - not its name, ``_id`` or revision. This
means that two design documents with identical view code will share the same
on-disk view index files.

Here is a worked example, assuming your ``/db/_design/ddoc`` needs tobe updated.

1. Upload the old design doc to ``/db/_design/ddoc-old`` (or copy the document)
   if you want an easy way to rollback in case of problems. The ``ddoc-old``
   document will reference the same view indexes already built for ``_design/ddoc``.

2. Upload the updated design doc to ``/db/_design/ddoc-new``.

3. Query a view in the new design document to trigger secondary index generation.
   You can track the indexing progress via the ``/_active_tasks`` endpoint, or
   through the :ref:`fauxton` web interface.

4. When the index is done being built, re-upload the updated design document to
   ``/db/_design/ddoc`` (or copy the document). The ``ddoc`` document will now
   reference the same view indexes already built for ``_design/ddoc-new``.

5. Delete ``/db/_design/ddoc-new`` and/or ``/db/_design/ddoc-old`` at your
   discretion. Don't forget to trigger :ref:`compact/views/cleanup` to reclaim
   disk space after deleting ``ddoc-old``.

The :ref:`COPY <api/doc/copy>` HTTP verb can be used to copy the design document
with a single command:

.. code-block:: bash

    curl -X COPY <URL of source design document> -H "Destination: <ID of destination design document>"
