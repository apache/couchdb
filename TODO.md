
* Patch the view engine to do alternative sorts. This will include both the lower level couch\_view* modules as well as the fabric coordinators.

* Patch the view engine so we can specify options when returning docs from cursors. We'll want this so that we can delete specific revisions from a document.

* Need to figure out how to do raw collation on some indices because at
least the _id index uses it forcefully.
