% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-define(MANGO_ERROR(R), throw({mango_error, ?MODULE, R})).

-type abstract_text_selector() :: {'op_and', [abstract_text_selector()]}
				| {'op_or', [abstract_text_selector()]}
				| {'op_not', {abstract_text_selector(), abstract_text_selector()}}
				| {'op_not', {_, 'false'}}
				| {'op_field', {iolist() | binary(), _}}
				| {'op_fieldname', {_, _}}
				| {'op_null', {_, _}}
				| {'op_default', _}.
