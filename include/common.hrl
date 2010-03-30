
-include_lib("eunit/include/eunit.hrl").

-define(fmt(Msg, Args), lists:flatten(io_lib:format(Msg, Args))).
-define(infoFmt(Msg, Args), error_logger:info_msg(Msg, Args)).
-define(infoMsg(Msg), error_logger:info_msg(Msg)).


%% from couch_db.hrl
-ifndef(LOG_DEBUG).
-define(LOG_DEBUG(Format, Args),
    showroom_log:message(debug, Format, Args)).
-endif.

-ifndef(LOG_INFO).
-define(LOG_INFO(Format, Args),
    showroom_log:message(info, Format, Args)).
-endif.

-ifndef(LOG_ERROR).
-define(LOG_ERROR(Format, Args),
    showroom_log:message(error, Format, Args)).
-endif.

%% -define(PMAP(F,L), lists:map(F,L)).
-define(PMAP(F,L), showroom_utils:pmap(F,L)).


%%
%% membership2 (in here for separate testing module)
%%

-define(VERSION,2).

-record(membership, {header=?VERSION,
                     node,
                     nodes,
                     partitions,
                     version,
                     fullmap
                    }).

%% version 3 of membership state
-record(mem, {header=3,
              node,
              nodes,
              clock,
              cache,
              args
             }).
