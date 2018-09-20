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

-module(couch_bt_engine_header).


-export([
    new/0,
    from/1,
    is_header/1,
    upgrade/1,
    get/2,
    get/3,
    set/2,
    set/3
]).

-export([
    disk_version/1,
    update_seq/1,
    id_tree_state/1,
    seq_tree_state/1,
    latest/1,
    local_tree_state/1,
    purge_tree_state/1,
    purge_seq_tree_state/1,
    purge_infos_limit/1,
    security_ptr/1,
    revs_limit/1,
    uuid/1,
    epochs/1,
    compacted_seq/1
]).


% This should be updated anytime a header change happens that requires more
% than filling in new defaults.
%
% As long the changes are limited to new header fields (with inline
% defaults) added to the end of the record, then there is no need to increment
% the disk revision number.
%
% if the disk revision is incremented, then new upgrade logic will need to be
% added to couch_db_updater:init_db.

-define(LATEST_DISK_VERSION, 7).

-record(db_header, {
    disk_version = ?LATEST_DISK_VERSION,
    update_seq = 0,
    unused = 0,
    id_tree_state = nil,
    seq_tree_state = nil,
    local_tree_state = nil,
    purge_tree_state = nil,
    purge_seq_tree_state = nil, %purge tree: purge_seq -> uuid
    security_ptr = nil,
    revs_limit = 1000,
    uuid,
    epochs,
    compacted_seq,
    purge_infos_limit = 1000
}).


new() ->
    #db_header{
        uuid = couch_uuids:random(),
        epochs = [{node(), 0}]
    }.


from(Header0) ->
    Header = upgrade(Header0),
    #db_header{
        uuid = Header#db_header.uuid,
        epochs = Header#db_header.epochs,
        compacted_seq = Header#db_header.compacted_seq
    }.


is_header(Header) ->
    try
        upgrade(Header),
        true
    catch _:_ ->
        false
    end.


upgrade(Header) ->
    Funs = [
        fun upgrade_tuple/1,
        fun upgrade_disk_version/1,
        fun upgrade_uuid/1,
        fun upgrade_epochs/1,
        fun upgrade_compacted_seq/1
    ],
    lists:foldl(fun(F, HdrAcc) ->
        F(HdrAcc)
    end, Header, Funs).


get(Header, Key) ->
    ?MODULE:get(Header, Key, undefined).


get(Header, Key, Default) ->
    get_field(Header, Key, Default).


set(Header, Key, Value) ->
    ?MODULE:set(Header, [{Key, Value}]).


set(Header0, Fields) ->
    % A subtlety here is that if a database was open during
    % the release upgrade that updates to uuids and epochs then
    % this dynamic upgrade also assigns a uuid and epoch.
    Header = upgrade(Header0),
    lists:foldl(fun({Field, Value}, HdrAcc) ->
        set_field(HdrAcc, Field, Value)
    end, Header, Fields).


disk_version(Header) ->
    get_field(Header, disk_version).


update_seq(Header) ->
    get_field(Header, update_seq).


id_tree_state(Header) ->
    get_field(Header, id_tree_state).


seq_tree_state(Header) ->
    get_field(Header, seq_tree_state).


local_tree_state(Header) ->
    get_field(Header, local_tree_state).


purge_tree_state(Header) ->
    get_field(Header, purge_tree_state).


purge_seq_tree_state(Header) ->
    get_field(Header, purge_seq_tree_state).


security_ptr(Header) ->
    get_field(Header, security_ptr).


revs_limit(Header) ->
    get_field(Header, revs_limit).


uuid(Header) ->
    get_field(Header, uuid).


epochs(Header) ->
    get_field(Header, epochs).


compacted_seq(Header) ->
    get_field(Header, compacted_seq).


purge_infos_limit(Header) ->
    get_field(Header, purge_infos_limit).


get_field(Header, Field) ->
    get_field(Header, Field, undefined).


get_field(Header, Field, Default) ->
    Idx = index(Field),
    case Idx > tuple_size(Header) of
        true -> Default;
        false -> element(index(Field), Header)
    end.


set_field(Header, Field, Value) ->
    setelement(index(Field), Header, Value).


index(Field) ->
    couch_util:get_value(Field, indexes()).


indexes() ->
    Fields = record_info(fields, db_header),
    Indexes = lists:seq(2, record_info(size, db_header)),
    lists:zip(Fields, Indexes).


upgrade_tuple(Old) when is_record(Old, db_header) ->
    Old;
upgrade_tuple(Old) when is_tuple(Old) ->
    NewSize = record_info(size, db_header),
    if tuple_size(Old) < NewSize -> ok; true ->
        erlang:error({invalid_header_size, Old})
    end,
    {_, New} = lists:foldl(fun(Val, {Idx, Hdr}) ->
        {Idx+1, setelement(Idx, Hdr, Val)}
    end, {1, #db_header{}}, tuple_to_list(Old)),
    if is_record(New, db_header) -> ok; true ->
        erlang:error({invalid_header_extension, {Old, New}})
    end,
    New.

-define(OLD_DISK_VERSION_ERROR,
    "Database files from versions smaller than 0.10.0 are no longer supported").

upgrade_disk_version(#db_header{}=Header) ->
    case element(2, Header) of
        1 -> throw({database_disk_version_error, ?OLD_DISK_VERSION_ERROR});
        2 -> throw({database_disk_version_error, ?OLD_DISK_VERSION_ERROR});
        3 -> throw({database_disk_version_error, ?OLD_DISK_VERSION_ERROR});
        4 -> Header#db_header{security_ptr = nil}; % [0.10 - 0.11)
        5 -> Header; % pre 1.2
        6 -> Header; % pre clustered purge
        ?LATEST_DISK_VERSION -> Header;
        _ ->
            Reason = "Incorrect disk header version",
            throw({database_disk_version_error, Reason})
    end.


upgrade_uuid(#db_header{}=Header) ->
    case Header#db_header.uuid of
        undefined ->
            % Upgrading this old db file to a newer
            % on disk format that includes a UUID.
            Header#db_header{uuid=couch_uuids:random()};
        _ ->
            Header
    end.


upgrade_epochs(#db_header{}=Header) ->
    NewEpochs = case Header#db_header.epochs of
        undefined ->
            % This node is taking over ownership of shard with
            % and old version of couch file. Before epochs there
            % was always an implicit assumption that a file was
            % owned since eternity by the node it was on. This
            % just codifies that assumption.
            [{node(), 0}];
        [{Node, _} | _] = Epochs0 when Node == node() ->
            % Current node is the current owner of this db
            Epochs0;
        Epochs1 ->
            % This node is taking over ownership of this db
            % and marking the update sequence where it happened.
            [{node(), Header#db_header.update_seq} | Epochs1]
    end,
    % Its possible for a node to open a db and claim
    % ownership but never make a write to the db. This
    % removes nodes that claimed ownership but never
    % changed the database.
    DedupedEpochs = remove_dup_epochs(NewEpochs),
    Header#db_header{epochs=DedupedEpochs}.


% This is slightly relying on the udpate_seq's being sorted
% in epochs due to how we only ever push things onto the
% front. Although if we ever had a case where the update_seq
% is not monotonically increasing I don't know that we'd
% want to remove dupes (by calling a sort on the input to this
% function). So for now we don't sort but are relying on the
% idea that epochs is always sorted.
remove_dup_epochs([_]=Epochs) ->
    Epochs;
remove_dup_epochs([{N1, S}, {_N2, S}]) ->
    % Seqs match, keep the most recent owner
    [{N1, S}];
remove_dup_epochs([_, _]=Epochs) ->
    % Seqs don't match.
    Epochs;
remove_dup_epochs([{N1, S}, {_N2, S} | Rest]) ->
    % Seqs match, keep the most recent owner
    remove_dup_epochs([{N1, S} | Rest]);
remove_dup_epochs([{N1, S1}, {N2, S2} | Rest]) ->
    % Seqs don't match, recurse to check others
    [{N1, S1} | remove_dup_epochs([{N2, S2} | Rest])].


upgrade_compacted_seq(#db_header{}=Header) ->
    case Header#db_header.compacted_seq of
        undefined ->
            Header#db_header{compacted_seq=0};
        _ ->
            Header
    end.

latest(?LATEST_DISK_VERSION) ->
    true;
latest(N) when is_integer(N), N < ?LATEST_DISK_VERSION ->
    false;
latest(_Else) ->
    undefined.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

mk_header(Vsn) ->
    {
        db_header, % record name
        Vsn, % disk version
        100, % update_seq
        0, % unused
        foo, % id_tree_state
        bar, % seq_tree_state
        bam, % local_tree_state
        flam, % was purge_seq - now purge_tree_state
        baz, % was purged_docs - now purge_seq_tree_state
        bang, % security_ptr
        999 % revs_limit
    }.


-ifdef(run_broken_tests).

upgrade_v3_test() ->
    Vsn3Header = mk_header(3),
    NewHeader = upgrade_tuple(Vsn3Header),

    % Tuple upgrades don't change
    ?assert(is_record(NewHeader, db_header)),
    ?assertEqual(3, disk_version(NewHeader)),
    ?assertEqual(100, update_seq(NewHeader)),
    ?assertEqual(foo, id_tree_state(NewHeader)),
    ?assertEqual(bar, seq_tree_state(NewHeader)),
    ?assertEqual(bam, local_tree_state(NewHeader)),
    ?assertEqual(flam, purge_tree_state(NewHeader)),
    ?assertEqual(baz, purge_seq_tree_state(NewHeader)),
    ?assertEqual(bang, security_ptr(NewHeader)),
    ?assertEqual(999, revs_limit(NewHeader)),
    ?assertEqual(undefined, uuid(NewHeader)),
    ?assertEqual(undefined, epochs(NewHeader)),

    % Security ptr isn't changed until upgrade_disk_version/1
    NewNewHeader = upgrade_disk_version(NewHeader),
    ?assert(is_record(NewNewHeader, db_header)),
    ?assertEqual(nil, security_ptr(NewNewHeader)),

    % Assert upgrade works on really old headers
    NewestHeader = upgrade(Vsn3Header),
    ?assertMatch(<<_:32/binary>>, uuid(NewestHeader)),
    ?assertEqual([{node(), 0}], epochs(NewestHeader)).

-endif.

upgrade_v5_test() ->
    Vsn5Header = mk_header(5),
    NewHeader = upgrade_disk_version(upgrade_tuple(Vsn5Header)),

    ?assert(is_record(NewHeader, db_header)),
    ?assertEqual(5, disk_version(NewHeader)),

    % Security ptr isn't changed for v5 headers
    ?assertEqual(bang, security_ptr(NewHeader)).


upgrade_uuid_test() ->
    Vsn5Header = mk_header(5),

    % Upgraded headers get a new UUID
    NewHeader = upgrade_uuid(upgrade_disk_version(upgrade_tuple(Vsn5Header))),
    ?assertMatch(<<_:32/binary>>, uuid(NewHeader)),

    % Headers with a UUID don't have their UUID changed
    NewNewHeader = upgrade_uuid(upgrade_disk_version(upgrade_tuple(NewHeader))),
    ?assertEqual(uuid(NewHeader), uuid(NewNewHeader)),

    % Derived empty headers maintain the same UUID
    ResetHeader = from(NewNewHeader),
    ?assertEqual(uuid(NewHeader), uuid(ResetHeader)).


upgrade_epochs_test() ->
    Vsn5Header = mk_header(5),

    % Upgraded headers get a default epochs set
    NewHeader = upgrade(Vsn5Header),
    ?assertEqual([{node(), 0}], epochs(NewHeader)),

    % Fake an old entry in epochs
    FakeFields = [
        {update_seq, 20},
        {epochs, [{'someothernode@someotherhost', 0}]}
    ],
    NotOwnedHeader = set(NewHeader, FakeFields),

    OwnedEpochs = [
        {node(), 20},
        {'someothernode@someotherhost', 0}
    ],

    % Upgrading a header not owned by the local node updates
    % the epochs appropriately.
    NowOwnedHeader = upgrade(NotOwnedHeader),
    ?assertEqual(OwnedEpochs, epochs(NowOwnedHeader)),

    % Headers with epochs stay the same after upgrades
    NewNewHeader = upgrade(NowOwnedHeader),
    ?assertEqual(OwnedEpochs, epochs(NewNewHeader)),

    % Getting a reset header maintains the epoch data
    ResetHeader = from(NewNewHeader),
    ?assertEqual(OwnedEpochs, epochs(ResetHeader)).


get_uuid_from_old_header_test() ->
    Vsn5Header = mk_header(5),
    ?assertEqual(undefined, uuid(Vsn5Header)).


get_epochs_from_old_header_test() ->
    Vsn5Header = mk_header(5),
    ?assertEqual(undefined, epochs(Vsn5Header)).


-endif.
