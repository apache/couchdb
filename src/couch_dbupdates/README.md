# couch_dbupdates

`couch_dbupdates` is a simple couchdb modules to receive databases
events in couchdb node.

It's actually **supported by all the [refuge](http://refuge.io) projects**:

- [refuge](https://github.com/refuge/refuge)
- [rcouch](https://github.com/refuge/rcouch)
- [rcouch_template](https://github.com/refuge/rcouch_template)


## HTTP API

To get db events, do a GET to `/_db_updates` .

You can pass an optional query parameters:

* `feed` The feed can be `longpoll` (default) for longpolling, `eventsource`
  for event stream or `continuous` for continuous feed.
* `timeout`: timeout before the longpolling connection close or when the
  heartbeat is emitted.
* `heartbeat`: true, or false. an empty line is emittend when the
  timeout occurs to maintain the connection active.


## Example of usage

    $ curl http://127.0.0.1:5984/_db_updates?feed=continuous
    {"type":"created","db_name":"testdb"}
    {"type":"updated","db_name":"testdb"}
    {"type":"deleted","db_name":"testdb"}
