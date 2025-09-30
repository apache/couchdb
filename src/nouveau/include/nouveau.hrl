%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-define(LEGACY_LUCENE_VERSION, 9).
-define(TARGET_LUCENE_VERSION, 10).

-record(index, {
    dbname,
    ddoc_id,
    lucene_version,
    default_analyzer,
    field_analyzers,
    def,
    def_lang,
    name,
    sig=nil
}).
