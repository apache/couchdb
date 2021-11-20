% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_partition).

-export([
    extract/1,
    from_docid/1,
    is_member/2,

    start_key/1,
    end_key/1,
    shard_key/1,

    validate_dbname/2,
    validate_docid/1,
    validate_partition/1,

    hash/1
]).

-include_lib("couch/include/couch_db.hrl").

extract(Value) when is_binary(Value) ->
    case binary:split(Value, <<":">>) of
        [Partition, Rest] ->
            {Partition, Rest};
        _ ->
            undefined
    end;
extract(_) ->
    undefined.

from_docid(DocId) ->
    case extract(DocId) of
        undefined ->
            throw({illegal_docid, <<"Doc id must be of form partition:id">>});
        {Partition, _} ->
            Partition
    end.

is_member(DocId, Partition) ->
    case extract(DocId) of
        {Partition, _} ->
            true;
        _ ->
            false
    end.

start_key(Partition) ->
    <<Partition/binary, ":">>.

end_key(Partition) ->
    <<Partition/binary, ";">>.

shard_key(Partition) ->
    <<Partition/binary, ":foo">>.

validate_dbname(DbName, Options) when is_list(DbName) ->
    validate_dbname(?l2b(DbName), Options);
validate_dbname(DbName, Options) when is_binary(DbName) ->
    Props = couch_util:get_value(props, Options, []),
    IsPartitioned = couch_util:get_value(partitioned, Props, false),

    if
        not IsPartitioned ->
            ok;
        true ->
            DbsDbName = config:get("mem3", "shards_db", "_dbs"),
            NodesDbName = config:get("mem3", "nodes_db", "_nodes"),
            UsersDbSuffix = config:get("couchdb", "users_db_suffix", "_users"),
            Suffix = couch_db:dbname_suffix(DbName),

            SysDbNames = [
                iolist_to_binary(DbsDbName),
                iolist_to_binary(NodesDbName)
                | ?SYSTEM_DATABASES
            ],

            Suffices = [
                <<"_replicator">>,
                <<"_users">>,
                iolist_to_binary(UsersDbSuffix)
            ],

            IsSysDb =
                lists:member(DbName, SysDbNames) orelse
                    lists:member(Suffix, Suffices),

            if
                not IsSysDb -> ok;
                true -> throw({bad_request, <<"Cannot partition a system database">>})
            end
    end.

validate_docid(<<"_design/", _/binary>>) ->
    ok;
validate_docid(<<"_local/", _/binary>>) ->
    ok;
validate_docid(DocId) when is_binary(DocId) ->
    % When this function is called we already know that
    % DocId is already valid thus we only need to
    % ensure that the partition exists and is not empty.
    case extract(DocId) of
        undefined ->
            throw({illegal_docid, <<"Doc id must be of form partition:id">>});
        {Partition, PartitionedDocId} ->
            validate_partition(Partition),
            couch_doc:validate_docid(PartitionedDocId)
    end.

validate_partition(<<>>) ->
    throw({illegal_partition, <<"Partition must not be empty">>});
validate_partition(Partition) when is_binary(Partition) ->
    case Partition of
        <<"_", _/binary>> ->
            Msg1 = <<"Partition must not start with an underscore">>,
            throw({illegal_partition, Msg1});
        _ ->
            ok
    end,
    case couch_util:validate_utf8(Partition) of
        true ->
            ok;
        false ->
            Msg2 = <<"Partition must be valid UTF-8">>,
            throw({illegal_partition, Msg2})
    end,
    case extract(Partition) of
        {_, _} ->
            Msg3 = <<"Partition must not contain a colon">>,
            throw({illegal_partition, Msg3});
        undefined ->
            ok
    end;
validate_partition(_) ->
    throw({illegal_partition, <<"Partition must be a string">>}).

% Document ids that start with an underscore
% (i.e., _local and _design) do not contain a
% partition and thus do not use the partition
% hashing.
hash(<<"_", _/binary>> = DocId) ->
    erlang:crc32(DocId);
hash(DocId) when is_binary(DocId) ->
    erlang:crc32(from_docid(DocId)).
