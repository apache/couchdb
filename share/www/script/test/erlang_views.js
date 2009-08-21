// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

couchTests.erlang_views = function(debug) {
  var db = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"false"});
  db.deleteDb();
  db.createDb();
  if (debug) debugger;



  run_on_modified_server(
    [{section: "native_query_servers",
      key: "erlang",
      value: "{couch_native_process, start_link, []}"}],
    function() {
      // Note we just do some basic 'smoke tests' here - the
      // test/query_server_spec.rb tests have more comprehensive tests
      var doc = {_id: "1", integer: 1, string: "str1", array: [1, 2, 3]};
      T(db.save(doc).ok);

      var mfun = 'fun({Doc}) -> ' +
                 ' K = proplists:get_value(<<"integer">>, Doc, null), ' +
                 ' V = proplists:get_value(<<"string">>, Doc, null), ' +
                 ' Emit(K, V) ' +
                 'end.';

      // emitting a key value that is undefined should result in that row not
      // being included in the view results
      var results = db.query(mfun, null, null, null, "erlang");
      T(results.total_rows == 1);
      T(results.rows[0].key == 1);
      T(results.rows[0].value == "str1");
      
      // check simple reduction - another doc with same key.
      var doc = {_id: "2", integer: 1, string: "str2"};
      T(db.save(doc).ok);
      rfun = "fun(Keys, Values, ReReduce) -> length(Values) end."
      results = db.query(mfun, rfun, null, null, "erlang");
      T(results.rows[0].value == 2);

      // simple 'list' tests
      var designDoc = {
        _id:"_design/erlview",
        language: "erlang",
        shows: {
          simple:
            'fun(Doc, {Req}) -> ' +
            '  {Info} = proplists:get_value(<<"info">>, Req, {[]}), ' +
            '  Purged = proplists:get_value(<<"purge_seq">>, Info, -1), ' +
            '  Verb = proplists:get_value(<<"verb">>, Req, <<"not_get">>), ' +
            '  R = list_to_binary(io_lib:format("~b - ~s", [Purged, Verb])), ' +
            '  {[{<<"code">>, 200}, {<<"headers">>, {[]}}, {<<"body">>, R}]} ' +
            'end.'
        },
        lists: {
          simple_list :
            'fun(Head, {Req}) -> ' +
            '  Send(<<"head">>), ' +
            '  Fun = fun({Row}, _) -> ' +
            '    Val = proplists:get_value(<<"value">>, Row, -1), ' +
            '    Send(list_to_binary(integer_to_list(Val))), ' +
            '    {ok, nil} ' +
            '  end, ' +
            '  {ok, _} = FoldRows(Fun, nil), ' +
            '  <<"tail">> ' +
            'end. '
        },
        views: {
          simple_view : {
            map: mfun,
            reduce: rfun
          }
        }
      };
      T(db.save(designDoc).ok);

      var url = "/test_suite_db/_design/erlview/_show/simple/1";
      var xhr = CouchDB.request("GET", url);
      T(xhr.status == 200, "standard get should be 200");
      T(xhr.responseText == "0 - GET");

      var url = "/test_suite_db/_design/erlview/_list/simple_list/simple_view";
      var xhr = CouchDB.request("GET", url);
      T(xhr.status == 200, "standard get should be 200");
      T(xhr.responseText == "head2tail");
    });
};
