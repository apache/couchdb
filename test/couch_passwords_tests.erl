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

-module(couch_passwords_tests).

-include("couch_eunit.hrl").


pbkdf2_test_()->
    {"PBKDF2",
     [
         {"Iterations: 1, length: 20",
          ?_assertEqual(
              {ok, <<"0c60c80f961f0e71f3a9b524af6012062fe037a6">>},
              couch_passwords:pbkdf2(<<"password">>, <<"salt">>, 1, 20))},

         {"Iterations: 2, length: 20",
          ?_assertEqual(
              {ok, <<"ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957">>},
              couch_passwords:pbkdf2(<<"password">>, <<"salt">>, 2, 20))},

         {"Iterations: 4096, length: 20",
          ?_assertEqual(
              {ok, <<"4b007901b765489abead49d926f721d065a429c1">>},
              couch_passwords:pbkdf2(<<"password">>, <<"salt">>, 4096, 20))},

         {"Iterations: 4096, length: 25",
          ?_assertEqual(
              {ok, <<"3d2eec4fe41c849b80c8d83662c0e44a8b291a964cf2f07038">>},
              couch_passwords:pbkdf2(<<"passwordPASSWORDpassword">>,
                                     <<"saltSALTsaltSALTsaltSALTsaltSALTsalt">>,
                                     4096, 25))},
         {"Null byte",
          ?_assertEqual(
              {ok, <<"56fa6aa75548099dcc37d7f03425e0c3">>},
              couch_passwords:pbkdf2(<<"pass\0word">>,
                                     <<"sa\0lt">>,
                                     4096, 16))},

         {timeout, 180,  %% this may runs too long on slow hosts
          {"Iterations: 16777216 - this may take some time",
           ?_assertEqual(
               {ok, <<"eefe3d61cd4da4e4e9945b3d6ba2158c2634e984">>},
               couch_passwords:pbkdf2(<<"password">>, <<"salt">>, 16777216, 20)
           )}}]}.
