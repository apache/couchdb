-module(mochiweb_request_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

accepts_content_type_test() ->
    Req1 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "multipart/related"}])),
    ?assertEqual(true, Req1:accepts_content_type("multipart/related")),
    ?assertEqual(true, Req1:accepts_content_type(<<"multipart/related">>)),

    Req2 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html"}])),
    ?assertEqual(false, Req2:accepts_content_type("multipart/related")),

    Req3 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html, multipart/*"}])),
    ?assertEqual(true, Req3:accepts_content_type("multipart/related")),

    Req4 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html, multipart/*; q=0.0"}])),
    ?assertEqual(false, Req4:accepts_content_type("multipart/related")),

    Req5 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html, multipart/*; q=0"}])),
    ?assertEqual(false, Req5:accepts_content_type("multipart/related")),

    Req6 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html, */*; q=0.0"}])),
    ?assertEqual(false, Req6:accepts_content_type("multipart/related")),

    Req7 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "multipart/*; q=0.0, */*"}])),
    ?assertEqual(false, Req7:accepts_content_type("multipart/related")),

    Req8 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "*/*; q=0.0, multipart/*"}])),
    ?assertEqual(true, Req8:accepts_content_type("multipart/related")),

    Req9 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "*/*; q=0.0, multipart/related"}])),
    ?assertEqual(true, Req9:accepts_content_type("multipart/related")),

    Req10 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html; level=1"}])),
    ?assertEqual(true, Req10:accepts_content_type("text/html;level=1")),

    Req11 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html; level=1, text/html"}])),
    ?assertEqual(true, Req11:accepts_content_type("text/html")),

    Req12 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html; level=1; q=0.0, text/html"}])),
    ?assertEqual(false, Req12:accepts_content_type("text/html;level=1")),

    Req13 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html; level=1; q=0.0, text/html"}])),
    ?assertEqual(false, Req13:accepts_content_type("text/html; level=1")),

    Req14 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html;level=1;q=0.1, text/html"}])),
    ?assertEqual(true, Req14:accepts_content_type("text/html; level=1")).

accepted_encodings_test() ->
    Req1 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
                                mochiweb_headers:make([])),
    ?assertEqual(["identity"],
                 Req1:accepted_encodings(["gzip", "identity"])),

    Req2 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "gzip, deflate"}])),
    ?assertEqual(["gzip", "identity"],
                 Req2:accepted_encodings(["gzip", "identity"])),

    Req3 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "gzip;q=0.5, deflate"}])),
    ?assertEqual(["deflate", "gzip", "identity"],
                 Req3:accepted_encodings(["gzip", "deflate", "identity"])),

    Req4 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "identity, *;q=0"}])),
    ?assertEqual(["identity"],
                 Req4:accepted_encodings(["gzip", "deflate", "identity"])),

    Req5 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "gzip; q=0.1, *;q=0"}])),
    ?assertEqual(["gzip"],
                 Req5:accepted_encodings(["gzip", "deflate", "identity"])),

    Req6 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "gzip; q=, *;q=0"}])),
    ?assertEqual(bad_accept_encoding_value,
                 Req6:accepted_encodings(["gzip", "deflate", "identity"])),

    Req7 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "gzip;q=2.0, *;q=0"}])),
    ?assertEqual(bad_accept_encoding_value,
                 Req7:accepted_encodings(["gzip", "identity"])),

    Req8 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "deflate, *;q=0.0"}])),
    ?assertEqual([],
                 Req8:accepted_encodings(["gzip", "identity"])).

accepted_content_types_test() ->
    Req1 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html"}])),
    ?assertEqual(["text/html"],
        Req1:accepted_content_types(["text/html", "application/json"])),

    Req2 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html, */*;q=0"}])),
    ?assertEqual(["text/html"],
        Req2:accepted_content_types(["text/html", "application/json"])),

    Req3 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/*, */*;q=0"}])),
    ?assertEqual(["text/html"],
        Req3:accepted_content_types(["text/html", "application/json"])),

    Req4 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/*;q=0.8, */*;q=0.5"}])),
    ?assertEqual(["text/html", "application/json"],
        Req4:accepted_content_types(["application/json", "text/html"])),

    Req5 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/*;q=0.8, */*;q=0.5"}])),
    ?assertEqual(["text/html", "application/json"],
        Req5:accepted_content_types(["text/html", "application/json"])),

    Req6 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/*;q=0.5, */*;q=0.5"}])),
    ?assertEqual(["application/json", "text/html"],
        Req6:accepted_content_types(["application/json", "text/html"])),

    Req7 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make(
            [{"Accept", "text/html;q=0.5, application/json;q=0.5"}])),
    ?assertEqual(["application/json", "text/html"],
        Req7:accepted_content_types(["application/json", "text/html"])),

    Req8 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html"}])),
    ?assertEqual([],
        Req8:accepted_content_types(["application/json"])),

    Req9 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/*;q=0.9, text/html;q=0.5, */*;q=0.7"}])),
    ?assertEqual(["application/json", "text/html"],
        Req9:accepted_content_types(["text/html", "application/json"])).

should_close_test() ->
    F = fun (V, H) ->
                (mochiweb_request:new(
                   nil, 'GET', "/", V,
                   mochiweb_headers:make(H)
                  )):should_close()
        end,
    ?assertEqual(
       true,
       F({1, 1}, [{"Connection", "close"}])),
    ?assertEqual(
       true,
       F({1, 0}, [{"Connection", "close"}])),
    ?assertEqual(
       true,
       F({1, 1}, [{"Connection", "ClOSe"}])),
    ?assertEqual(
       false,
       F({1, 1}, [{"Connection", "closer"}])),
    ?assertEqual(
       false,
       F({1, 1}, [])),
    ?assertEqual(
       true,
       F({1, 0}, [])),
    ?assertEqual(
       false,
       F({1, 0}, [{"Connection", "Keep-Alive"}])),
    ok.

-endif.
