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

from_extension(".stl") ->
    "application/SLA";
from_extension(".stp") ->
    "application/STEP";
from_extension(".step") ->
    "application/STEP";
from_extension(".dwg") ->
    "application/acad";
from_extension(".ez") ->
    "application/andrew-inset";
from_extension(".ccad") ->
    "application/clariscad";
from_extension(".drw") ->
    "application/drafting";
from_extension(".tsp") ->
    "application/dsptype";
from_extension(".dxf") ->
    "application/dxf";
from_extension(".xls") ->
    "application/excel";
from_extension(".unv") ->
    "application/i-deas";
from_extension(".jar") ->
    "application/java-archive";
from_extension(".hqx") ->
    "application/mac-binhex40";
from_extension(".cpt") ->
    "application/mac-compactpro";
from_extension(".pot") ->
    "application/vnd.ms-powerpoint";
from_extension(".ppt") ->
    "application/vnd.ms-powerpoint";
from_extension(".dms") ->
    "application/octet-stream";
from_extension(".lha") ->
    "application/octet-stream";
from_extension(".lzh") ->
    "application/octet-stream";
from_extension(".oda") ->
    "application/oda";
from_extension(".ogg") ->
    "application/ogg";
from_extension(".ogm") ->
    "application/ogg";
from_extension(".pdf") ->
    "application/pdf";
from_extension(".pgp") ->
    "application/pgp";
from_extension(".ai") ->
    "application/postscript";
from_extension(".eps") ->
    "application/postscript";
from_extension(".ps") ->
    "application/postscript";
from_extension(".prt") ->
    "application/pro_eng";
from_extension(".rtf") ->
    "application/rtf";
from_extension(".smi") ->
    "application/smil";
from_extension(".smil") ->
    "application/smil";
from_extension(".sol") ->
    "application/solids";
from_extension(".vda") ->
    "application/vda";
from_extension(".xlm") ->
    "application/vnd.ms-excel";
from_extension(".cod") ->
    "application/vnd.rim.cod";
from_extension(".pgn") ->
    "application/x-chess-pgn";
from_extension(".cpio") ->
    "application/x-cpio";
from_extension(".csh") ->
    "application/x-csh";
from_extension(".deb") ->
    "application/x-debian-package";
from_extension(".dcr") ->
    "application/x-director";
from_extension(".dir") ->
    "application/x-director";
from_extension(".dxr") ->
    "application/x-director";
from_extension(".gz") ->
    "application/x-gzip";
from_extension(".hdf") ->
    "application/x-hdf";
from_extension(".ipx") ->
    "application/x-ipix";
from_extension(".ips") ->
    "application/x-ipscript";
from_extension(".js") ->
    "application/x-javascript";
from_extension(".skd") ->
    "application/x-koan";
from_extension(".skm") ->
    "application/x-koan";
from_extension(".skp") ->
    "application/x-koan";
from_extension(".skt") ->
    "application/x-koan";
from_extension(".latex") ->
    "application/x-latex";
from_extension(".lsp") ->
    "application/x-lisp";
from_extension(".scm") ->
    "application/x-lotusscreencam";
from_extension(".mif") ->
    "application/x-mif";
from_extension(".com") ->
    "application/x-msdos-program";
from_extension(".exe") ->
    "application/octet-stream";
from_extension(".cdf") ->
    "application/x-netcdf";
from_extension(".nc") ->
    "application/x-netcdf";
from_extension(".pl") ->
    "application/x-perl";
from_extension(".pm") ->
    "application/x-perl";
from_extension(".rar") ->
    "application/x-rar-compressed";
from_extension(".sh") ->
    "application/x-sh";
from_extension(".shar") ->
    "application/x-shar";
from_extension(".swf") ->
    "application/x-shockwave-flash";
from_extension(".sit") ->
    "application/x-stuffit";
from_extension(".sv4cpio") ->
    "application/x-sv4cpio";
from_extension(".sv4crc") ->
    "application/x-sv4crc";
from_extension(".tar.gz") ->
    "application/x-tar-gz";
from_extension(".tgz") ->
    "application/x-tar-gz";
from_extension(".tar") ->
    "application/x-tar";
from_extension(".tcl") ->
    "application/x-tcl";
from_extension(".texi") ->
    "application/x-texinfo";
from_extension(".texinfo") ->
    "application/x-texinfo";
from_extension(".man") ->
    "application/x-troff-man";
from_extension(".me") ->
    "application/x-troff-me";
from_extension(".ms") ->
    "application/x-troff-ms";
from_extension(".roff") ->
    "application/x-troff";
from_extension(".t") ->
    "application/x-troff";
from_extension(".tr") ->
    "application/x-troff";
from_extension(".ustar") ->
    "application/x-ustar";
from_extension(".src") ->
    "application/x-wais-source";
from_extension(".zip") ->
    "application/zip";
from_extension(".tsi") ->
    "audio/TSP-audio";
from_extension(".au") ->
    "audio/basic";
from_extension(".snd") ->
    "audio/basic";
from_extension(".kar") ->
    "audio/midi";
from_extension(".mid") ->
    "audio/midi";
from_extension(".midi") ->
    "audio/midi";
from_extension(".mp2") ->
    "audio/mpeg";
from_extension(".mp3") ->
    "audio/mpeg";
from_extension(".mpga") ->
    "audio/mpeg";
from_extension(".aif") ->
    "audio/x-aiff";
from_extension(".aifc") ->
    "audio/x-aiff";
from_extension(".aiff") ->
    "audio/x-aiff";
from_extension(".m3u") ->
    "audio/x-mpegurl";
from_extension(".wax") ->
    "audio/x-ms-wax";
from_extension(".wma") ->
    "audio/x-ms-wma";
from_extension(".rpm") ->
    "audio/x-pn-realaudio-plugin";
from_extension(".ram") ->
    "audio/x-pn-realaudio";
from_extension(".rm") ->
    "audio/x-pn-realaudio";
from_extension(".ra") ->
    "audio/x-realaudio";
from_extension(".wav") ->
    "audio/x-wav";
from_extension(".pdb") ->
    "chemical/x-pdb";
from_extension(".ras") ->
    "image/cmu-raster";
from_extension(".gif") ->
    "image/gif";
from_extension(".ief") ->
    "image/ief";
from_extension(".jpe") ->
    "image/jpeg";
from_extension(".jpeg") ->
    "image/jpeg";
from_extension(".jpg") ->
    "image/jpeg";
from_extension(".jp2") ->
    "image/jp2";
from_extension(".png") ->
    "image/png";
from_extension(".tif") ->
    "image/tiff";
from_extension(".tiff") ->
    "image/tiff";
from_extension(".pnm") ->
    "image/x-portable-anymap";
from_extension(".pbm") ->
    "image/x-portable-bitmap";
from_extension(".pgm") ->
    "image/x-portable-graymap";
from_extension(".ppm") ->
    "image/x-portable-pixmap";
from_extension(".rgb") ->
    "image/x-rgb";
from_extension(".xbm") ->
    "image/x-xbitmap";
from_extension(".xwd") ->
    "image/x-xwindowdump";
from_extension(".iges") ->
    "model/iges";
from_extension(".igs") ->
    "model/iges";
from_extension(".mesh") ->
    "model/mesh";
from_extension(".") ->
    "";
from_extension(".msh") ->
    "model/mesh";
from_extension(".silo") ->
    "model/mesh";
from_extension(".vrml") ->
    "model/vrml";
from_extension(".wrl") ->
    "model/vrml";
from_extension(".css") ->
    "text/css";
from_extension(".htm") ->
    "text/html";
from_extension(".html") ->
    "text/html";
from_extension(".asc") ->
    "text/plain";
from_extension(".c") ->
    "text/plain";
from_extension(".cc") ->
    "text/plain";
from_extension(".f90") ->
    "text/plain";
from_extension(".f") ->
    "text/plain";
from_extension(".hh") ->
    "text/plain";
from_extension(".m") ->
    "text/plain";
from_extension(".txt") ->
    "text/plain";
from_extension(".rtx") ->
    "text/richtext";
from_extension(".sgm") ->
    "text/sgml";
from_extension(".sgml") ->
    "text/sgml";
from_extension(".tsv") ->
    "text/tab-separated-values";
from_extension(".jad") ->
    "text/vnd.sun.j2me.app-descriptor";
from_extension(".etx") ->
    "text/x-setext";
from_extension(".xml") ->
    "application/xml";
from_extension(".dl") ->
    "video/dl";
from_extension(".fli") ->
    "video/fli";
from_extension(".flv") ->
    "video/x-flv";
from_extension(".gl") ->
    "video/gl";
from_extension(".mp4") ->
    "video/mp4";
from_extension(".mpe") ->
    "video/mpeg";
from_extension(".mpeg") ->
    "video/mpeg";
from_extension(".mpg") ->
    "video/mpeg";
from_extension(".mov") ->
    "video/quicktime";
from_extension(".qt") ->
    "video/quicktime";
from_extension(".viv") ->
    "video/vnd.vivo";
from_extension(".vivo") ->
    "video/vnd.vivo";
from_extension(".asf") ->
    "video/x-ms-asf";
from_extension(".asx") ->
    "video/x-ms-asx";
from_extension(".wmv") ->
    "video/x-ms-wmv";
from_extension(".wmx") ->
    "video/x-ms-wmx";
from_extension(".wvx") ->
    "video/x-ms-wvx";
from_extension(".avi") ->
    "video/x-msvideo";
from_extension(".movie") ->
    "video/x-sgi-movie";
from_extension(".mime") ->
    "www/mime";
from_extension(".ice") ->
    "x-conference/x-cooltalk";
from_extension(".vrm") ->
    "x-world/x-vrml";
from_extension(".spx") ->
    "audio/ogg";
from_extension(".xhtml") ->
    "application/xhtml+xml";
from_extension(".bz2") ->
    "application/x-bzip2";
from_extension(".doc") ->
    "application/msword";
from_extension(".z") ->
    "application/x-compress";
from_extension(".ico") ->
    "image/x-icon";
from_extension(".bmp") ->
    "image/bmp";
from_extension(".m4a") ->
    "audio/mpeg";
from_extension(".csv") ->
    "text/csv";
from_extension(".eot") ->
    "application/vnd.ms-fontobject";
from_extension(".m4v") ->
    "video/mp4";
from_extension(".svg") ->
    "image/svg+xml";
from_extension(".svgz") ->
    "image/svg+xml";
from_extension(".ttc") ->
    "application/x-font-ttf";
from_extension(".ttf") ->
    "application/x-font-ttf";
from_extension(".vcf") ->
    "text/x-vcard";
from_extension(".webm") ->
    "video/web";
from_extension(".webp") ->
    "image/web";
from_extension(".woff") ->
    "application/x-font-woff";
from_extension(".otf") ->
    "font/opentype";
from_extension(_) ->
    undefined.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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
