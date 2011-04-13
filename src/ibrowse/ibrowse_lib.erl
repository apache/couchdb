%%% File    : ibrowse_lib.erl
%%% Author  : Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>
%%% Description : 
%%% Created : 27 Feb 2004 by Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>
%% @doc Module with a few useful functions

-module(ibrowse_lib).
-author('chandru').
-ifdef(debug).
-compile(export_all).
-endif.

-include("ibrowse.hrl").

-export([
         get_trace_status/2,
         do_trace/2,
         do_trace/3,
         url_encode/1,
         decode_rfc822_date/1,
         status_code/1,
         encode_base64/1,
         decode_base64/1,
         get_value/2,
         get_value/3,
         parse_url/1,
         printable_date/0
        ]).

get_trace_status(Host, Port) ->
    ibrowse:get_config_value({trace, Host, Port}, false).

%% @doc URL-encodes a string based on RFC 1738. Returns a flat list.
%% @spec url_encode(Str) -> UrlEncodedStr
%% Str = string()
%% UrlEncodedStr = string()
url_encode(Str) when is_list(Str) ->
    url_encode_char(lists:reverse(Str), []).

url_encode_char([X | T], Acc) when X >= $0, X =< $9 ->
    url_encode_char(T, [X | Acc]);
url_encode_char([X | T], Acc) when X >= $a, X =< $z ->
    url_encode_char(T, [X | Acc]);
url_encode_char([X | T], Acc) when X >= $A, X =< $Z ->
    url_encode_char(T, [X | Acc]);
url_encode_char([X | T], Acc) when X == $-; X == $_; X == $. ->
    url_encode_char(T, [X | Acc]);
url_encode_char([32 | T], Acc) ->
    url_encode_char(T, [$+ | Acc]);
url_encode_char([X | T], Acc) ->
    url_encode_char(T, [$%, d2h(X bsr 4), d2h(X band 16#0f) | Acc]);
url_encode_char([], Acc) ->
    Acc.

d2h(N) when N<10 -> N+$0;
d2h(N) -> N+$a-10.

decode_rfc822_date(String) when is_list(String) ->
    case catch decode_rfc822_date_1(string:tokens(String, ", \t\r\n")) of
        {'EXIT', _} ->
            {error, invalid_date};
        Res ->
            Res
    end.

% TODO: Have to handle the Zone
decode_rfc822_date_1([_,DayInt,Month,Year, Time,Zone]) ->
    decode_rfc822_date_1([DayInt,Month,Year, Time,Zone]);
decode_rfc822_date_1([Day,Month,Year, Time,_Zone]) ->
    DayI = list_to_integer(Day),
    MonthI = month_int(Month),
    YearI = list_to_integer(Year),
    TimeTup = case string:tokens(Time, ":") of
                  [H,M] ->
                      {list_to_integer(H),
                       list_to_integer(M),
                       0};
                  [H,M,S] ->
                      {list_to_integer(H),
                       list_to_integer(M),
                       list_to_integer(S)}
              end,
    {{YearI,MonthI,DayI}, TimeTup}.

month_int("Jan") -> 1;
month_int("Feb") -> 2;
month_int("Mar") -> 3;
month_int("Apr") -> 4;
month_int("May") -> 5;
month_int("Jun") -> 6;
month_int("Jul") -> 7;
month_int("Aug") -> 8;
month_int("Sep") -> 9;
month_int("Oct") -> 10;
month_int("Nov") -> 11;
month_int("Dec") -> 12.

%% @doc Given a status code, returns an atom describing the status code. 
%% @spec status_code(StatusCode::status_code()) -> StatusDescription
%% status_code() = string() | integer()
%% StatusDescription = atom()
status_code(100) -> continue;
status_code(101) -> switching_protocols;
status_code(102) -> processing;
status_code(200) -> ok;
status_code(201) -> created;
status_code(202) -> accepted;
status_code(203) -> non_authoritative_information;
status_code(204) -> no_content;
status_code(205) -> reset_content;
status_code(206) -> partial_content;
status_code(207) -> multi_status;
status_code(300) -> multiple_choices;
status_code(301) -> moved_permanently;
status_code(302) -> found;
status_code(303) -> see_other;
status_code(304) -> not_modified;
status_code(305) -> use_proxy;
status_code(306) -> unused;
status_code(307) -> temporary_redirect;
status_code(400) -> bad_request;
status_code(401) -> unauthorized;
status_code(402) -> payment_required;
status_code(403) -> forbidden;
status_code(404) -> not_found;
status_code(405) -> method_not_allowed;
status_code(406) -> not_acceptable;
status_code(407) -> proxy_authentication_required;
status_code(408) -> request_timeout;
status_code(409) -> conflict;
status_code(410) -> gone;
status_code(411) -> length_required;
status_code(412) -> precondition_failed;
status_code(413) -> request_entity_too_large;
status_code(414) -> request_uri_too_long;
status_code(415) -> unsupported_media_type;
status_code(416) -> requested_range_not_satisfiable;
status_code(417) -> expectation_failed;
status_code(422) -> unprocessable_entity;
status_code(423) -> locked;
status_code(424) -> failed_dependency;
status_code(500) -> internal_server_error;
status_code(501) -> not_implemented;
status_code(502) -> bad_gateway;
status_code(503) -> service_unavailable;
status_code(504) -> gateway_timeout;
status_code(505) -> http_version_not_supported;
status_code(507) -> insufficient_storage;
status_code(X) when is_list(X) -> status_code(list_to_integer(X));
status_code(_)   -> unknown_status_code.

%% @doc Implements the base64 encoding algorithm. The output data type matches in the input data type.
%% @spec encode_base64(In) -> Out
%% In = string() | binary()
%% Out = string() | binary()
encode_base64(List) when is_list(List) ->
    binary_to_list(base64:encode(List));
encode_base64(Bin) when is_binary(Bin) ->
    base64:encode(Bin).

%% @doc Implements the base64 decoding algorithm. The output data type matches in the input data type.
%% @spec decode_base64(In) -> Out | exit({error, invalid_input})
%% In = string() | binary()
%% Out = string() | binary()
decode_base64(List) when is_list(List) ->
    binary_to_list(base64:decode(List));
decode_base64(Bin) when is_binary(Bin) ->
    base64:decode(Bin).

get_value(Tag, TVL, DefVal) ->
    case lists:keysearch(Tag, 1, TVL) of
        false ->
            DefVal;
        {value, {_, Val}} ->
            Val
    end.

get_value(Tag, TVL) ->
    {value, {_, V}} = lists:keysearch(Tag,1,TVL),
    V.

parse_url(Url) ->
    case parse_url(Url, get_protocol, #url{abspath=Url}, []) of
        #url{host_type = undefined, host = Host} = UrlRec ->
            case inet_parse:address(Host) of
                {ok, {_, _, _, _, _, _, _, _}} ->
                    UrlRec#url{host_type = ipv6_address};
                {ok, {_, _, _, _}} ->
                    UrlRec#url{host_type = ipv4_address};
                _ ->
                    UrlRec#url{host_type = hostname}
            end;
        Else ->
            Else
    end.

parse_url([$:, $/, $/ | _], get_protocol, Url, []) ->
    {invalid_uri_1, Url};
parse_url([$:, $/, $/ | T], get_protocol, Url, TmpAcc) ->
    Prot = list_to_existing_atom(lists:reverse(TmpAcc)),
    parse_url(T, get_username, 
              Url#url{protocol = Prot},
              []);
parse_url([H | T], get_username, Url, TmpAcc) when H == $/;
                                                   H == $? ->
    Path = case H of
               $/ ->
                   [$/ | T];
               $? ->
                   [$/, $? | T]
           end,
    %% No username/password. No  port number
    Url#url{host = lists:reverse(TmpAcc),
            port = default_port(Url#url.protocol),
           path = Path};
parse_url([$: | T], get_username, Url, TmpAcc) ->
    %% It is possible that no username/password has been
    %% specified. But we'll continue with the assumption that there is
    %% a username/password. If we encounter a '@' later on, there is a
    %% username/password indeed. If we encounter a '/', it was
    %% actually the hostname
    parse_url(T, get_password, 
              Url#url{username = lists:reverse(TmpAcc)},
              []);
parse_url([$@ | T], get_username, Url, TmpAcc) ->
    parse_url(T, get_host, 
              Url#url{username = lists:reverse(TmpAcc),
                      password = ""},
              []);
parse_url([$[ | T], get_username, Url, []) ->
    % IPv6 address literals are enclosed by square brackets:
    %     http://www.ietf.org/rfc/rfc2732.txt
    parse_url(T, get_ipv6_address, Url#url{host_type = ipv6_address}, []);
parse_url([$[ | T], get_username, _Url, TmpAcc) ->
    {error, {invalid_username_or_host, lists:reverse(TmpAcc) ++ "[" ++ T}};
parse_url([$[ | _], get_password, _Url, []) ->
    {error, missing_password};
parse_url([$[ | T], get_password, Url, TmpAcc) ->
    % IPv6 address literals are enclosed by square brackets:
    %     http://www.ietf.org/rfc/rfc2732.txt
    parse_url(T, get_ipv6_address,
              Url#url{host_type = ipv6_address,
                      password = lists:reverse(TmpAcc)},
              []);
parse_url([$@ | T], get_password, Url, TmpAcc) ->
    parse_url(T, get_host, 
              Url#url{password = lists:reverse(TmpAcc)},
              []);
parse_url([H | T], get_password, Url, TmpAcc) when H == $/;
                                                   H == $? ->
    %% Ok, what we thought was the username/password was the hostname
    %% and portnumber
    #url{username=User} = Url,
    Port = list_to_integer(lists:reverse(TmpAcc)),
    Path = case H of
               $/ ->
                   [$/ | T];
               $? ->
                   [$/, $? | T]
           end,
    Url#url{host = User,
            port = Port,
            username = undefined,
            password = undefined,
           path = Path};
parse_url([$] | T], get_ipv6_address, #url{protocol = Prot} = Url, TmpAcc) ->
    Addr = lists:reverse(TmpAcc),
    case inet_parse:address(Addr) of
        {ok, {_, _, _, _, _, _, _, _}} ->
            Url2 = Url#url{host = Addr, port = default_port(Prot)},
            case T of
                [$: | T2] ->
                    parse_url(T2, get_port, Url2, []);
                [$/ | T2] ->
                    Url2#url{path = [$/ | T2]};
                [$? | T2] ->
                    Url2#url{path = [$/, $? | T2]};
                [] ->
                    Url2#url{path = "/"};
                _ ->
                    {error, {invalid_host, "[" ++ Addr ++ "]" ++ T}}
            end;
        _ ->
            {error, {invalid_ipv6_address, Addr}}
    end;
parse_url([$[ | T], get_host, #url{} = Url, []) ->
    parse_url(T, get_ipv6_address, Url#url{host_type = ipv6_address}, []);
parse_url([$: | T], get_host, #url{} = Url, TmpAcc) ->
    parse_url(T, get_port, 
              Url#url{host = lists:reverse(TmpAcc)},
              []);
parse_url([H | T], get_host, #url{protocol=Prot} = Url, TmpAcc) when H == $/;
                                                                     H == $? ->
    Path = case H of
               $/ ->
                   [$/ | T];
               $? ->
                   [$/, $? | T]
           end,
    Url#url{host = lists:reverse(TmpAcc),
            port = default_port(Prot),
           path = Path};
parse_url([H | T], get_port, #url{protocol=Prot} = Url, TmpAcc) when H == $/;
                                                                     H == $? ->
    Path = case H of
               $/ ->
                   [$/ | T];
               $? ->
                   [$/, $? | T]
           end,
    Port = case TmpAcc of
               [] ->
                   default_port(Prot);
               _ ->
                   list_to_integer(lists:reverse(TmpAcc))
           end,
    Url#url{port = Port, path = Path};
parse_url([H | T], State, Url, TmpAcc) ->
    parse_url(T, State, Url, [H | TmpAcc]);
parse_url([], get_host, Url, TmpAcc) when TmpAcc /= [] ->
    Url#url{host = lists:reverse(TmpAcc),
            port = default_port(Url#url.protocol),
            path = "/"};
parse_url([], get_username, Url, TmpAcc) when TmpAcc /= [] ->
    Url#url{host = lists:reverse(TmpAcc),
            port = default_port(Url#url.protocol),
            path = "/"};
parse_url([], get_port, #url{protocol=Prot} = Url, TmpAcc) ->
    Port = case TmpAcc of
               [] ->
                   default_port(Prot);
               _ ->
                   list_to_integer(lists:reverse(TmpAcc))
           end,
    Url#url{port = Port, 
            path = "/"};
parse_url([], get_password, Url, TmpAcc) ->
    %% Ok, what we thought was the username/password was the hostname
    %% and portnumber
    #url{username=User} = Url,
    Port = case TmpAcc of
               [] ->
                   default_port(Url#url.protocol);
               _ ->
                   list_to_integer(lists:reverse(TmpAcc))
           end,
    Url#url{host = User,
            port = Port,
            username = undefined,
            password = undefined,
            path = "/"};
parse_url([], State, Url, TmpAcc) ->
    {invalid_uri_2, State, Url, TmpAcc}.

default_port(http)  -> 80;
default_port(https) -> 443;
default_port(ftp)   -> 21.

printable_date() ->
    {{Y,Mo,D},{H, M, S}} = calendar:local_time(),
    {_,_,MicroSecs} = now(),
    [integer_to_list(Y),
     $-,
     integer_to_list(Mo),
     $-,
     integer_to_list(D),
     $_,
     integer_to_list(H),
     $:,
     integer_to_list(M),
     $:,
     integer_to_list(S),
     $:,
     integer_to_list(MicroSecs div 1000)].

do_trace(Fmt, Args) ->
    do_trace(get(my_trace_flag), Fmt, Args).

-ifdef(DEBUG).
do_trace(_, Fmt, Args) ->
    io:format("~s -- (~s) - "++Fmt,
              [printable_date(), 
               get(ibrowse_trace_token) | Args]).
-else.
do_trace(true, Fmt, Args) ->
    io:format("~s -- (~s) - "++Fmt,
              [printable_date(), 
               get(ibrowse_trace_token) | Args]);
do_trace(_, _, _) ->
    ok.
-endif.
