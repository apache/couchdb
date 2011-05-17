%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Gives a good MIME type guess based on file extension.

-module(mochiweb_mime).
-author('bob@mochimedia.com').
-export([from_extension/1]).

%% @spec from_extension(S::string()) -> string() | undefined
%% @doc Given a filename extension (e.g. ".html") return a guess for the MIME
%%      type such as "text/html". Will return the atom undefined if no good
%%      guess is available.
from_extension(".html") ->
    "text/html";
from_extension(".xhtml") ->
    "application/xhtml+xml";
from_extension(".xml") ->
    "application/xml";
from_extension(".css") ->
    "text/css";
from_extension(".js") ->
    "application/x-javascript";
from_extension(".jpg") ->
    "image/jpeg";
from_extension(".gif") ->
    "image/gif";
from_extension(".png") ->
    "image/png";
from_extension(".swf") ->
    "application/x-shockwave-flash";
from_extension(".zip") ->
    "application/zip";
from_extension(".bz2") ->
    "application/x-bzip2";
from_extension(".gz") ->
    "application/x-gzip";
from_extension(".tar") ->
    "application/x-tar";
from_extension(".tgz") ->
    "application/x-gzip";
from_extension(".txt") ->
    "text/plain";
from_extension(".doc") ->
    "application/msword";
from_extension(".pdf") ->
    "application/pdf";
from_extension(".xls") ->
    "application/vnd.ms-excel";
from_extension(".rtf") ->
    "application/rtf";
from_extension(".mov") ->
    "video/quicktime";
from_extension(".mp3") ->
    "audio/mpeg";
from_extension(".z") ->
    "application/x-compress";
from_extension(".wav") ->
    "audio/x-wav";
from_extension(".ico") ->
    "image/x-icon";
from_extension(".bmp") ->
    "image/bmp";
from_extension(".m4a") ->
    "audio/mpeg";
from_extension(".m3u") ->
    "audio/x-mpegurl";
from_extension(".exe") ->
    "application/octet-stream";
from_extension(".csv") ->
    "text/csv";
from_extension(_) ->
    undefined.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

exhaustive_from_extension_test() ->
    T = mochiweb_cover:clause_lookup_table(?MODULE, from_extension),
    [?assertEqual(V, from_extension(K)) || {K, V} <- T].

from_extension_test() ->
    ?assertEqual("text/html",
                 from_extension(".html")),
    ?assertEqual(undefined,
                 from_extension("")),
    ?assertEqual(undefined,
                 from_extension(".wtf")),
    ok.

-endif.
