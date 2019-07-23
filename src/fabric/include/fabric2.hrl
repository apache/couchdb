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


-define(uint2bin(I), binary:encode_unsigned(I, little)).
-define(bin2uint(I), binary:decode_unsigned(I, little)).

% This will eventually be the `\xFFmetadataVersion` key that is
% currently only available in FoundationDB master.
%
%  https://forums.foundationdb.org/t/a-new-tool-for-managing-layer-metadata/1191
%
% Until then we'll fake the same behavior using a randomish
% key for tracking metadata changse. Once we get to the
% new feature this will be more performant by updating
% this define.
-define(METADATA_VERSION_KEY, <<"$metadata_version_key$">>).


% Prefix Definitions

% Layer Level: (LayerPrefix, X, ...)

-define(CLUSTER_CONFIG, 0).
-define(ALL_DBS, 1).
-define(DBS, 15).
-define(TX_IDS, 255).

% Database Level: (LayerPrefix, ?DBS, DbPrefix, X, ...)

-define(DB_VERSION, 0).
-define(DB_CONFIG, 16).
-define(DB_STATS, 17).
-define(DB_ALL_DOCS, 18).
-define(DB_CHANGES, 19).
-define(DB_REVS, 20).
-define(DB_DOCS, 21).
-define(DB_LOCAL_DOCS, 22).
-define(DB_ATTS, 23).


% Versions

-define(CURR_REV_FORMAT, 0).


% Misc constants

-define(PDICT_DB_KEY, '$fabric_db_handle').
-define(PDICT_LAYER_CACHE, '$fabric_layer_id').
-define(PDICT_CHECKED_DB_IS_CURRENT, '$fabric_checked_db_is_current').
-define(PDICT_TX_ID_KEY, '$fabric_tx_id').
-define(PDICT_TX_RES_KEY, '$fabric_tx_result').
-define(COMMIT_UNKNOWN_RESULT, 1021).


-define(ATTACHMENT_CHUNK_SIZE, 100000).
