
* Patch the view engine to do alternative sorts. This will include
  both the lower level couch\_view* modules as well as the fabric
  coordinators.

* Patch the view engine so we can specify options when returning docs
  from cursors. We'll want this so that we can delete specific
  revisions from a document.

* Need to figure out how to do raw collation on some indices because
  at least the _id index uses it forcefully.

* Add lots more to the update API. Mongo appears to be missing some
  pretty obvious easy functionality here. Things like managing values
  doing things like multiplying numbers, or common string mutations
  would be obvious examples. Also it could be interesting to add to
  the language so that you can do conditional updates based on other
  document attributes. Definitely not a V1 endeavor.
