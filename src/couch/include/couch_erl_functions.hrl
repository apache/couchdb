% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-define(AUTH_DB_DOC_VALIDATE_FUNCTION,
    % if operation is not update a OldDoc param is null atom
    fun({NewDoc}, OldDoc, {UserCtx}, {SecObj}) ->
        OldDocName = case OldDoc of
            {Doc} when is_list(Doc) -> couch_util:get_value(<<"name">>, Doc);
            _ -> undefined
        end,
        % see if the user is a server admin
        IsAdmin = lists:member(
            <<"_admin">>,
            couch_util:get_value(<<"roles">>, UserCtx)
        ),
        NewUserCxtName = couch_util:get_value(<<"name">>, UserCtx),
        case couch_util:get_value(<<"_deleted">>, NewDoc) =:= true of
            true ->
                if
                    % allow deletes by admins and matching users
                    % without checking the other fields
                    IsAdmin or NewUserCxtName =:= OldDocName -> 1;
                    true -> throw({
                        forbidden,
                        <<"Only admins may delete other user docs.">>
                    })
                end;
            _ ->
                NewDocName = couch_util:get_value(<<"name">>, NewDoc),
                NewDocRoles = couch_util:get_value(<<"roles">>, NewDoc),
                RolesIsBinary = lists:foldl(fun
                    (_, false) -> false;
                    (E, _) when is_binary(E) -> true;
                    (_, _) -> false
                end, true, NewDocRoles),
                NewDocId = couch_util:get_value(<<"_id">>, NewDoc),
                NewPassShaDoc = couch_util:get_value(<<"password_sha">>, NewDoc, false),
                NewSaltDoc = couch_util:get_value(<<"salt">>, NewDoc, false),
                NewPassSchemeDoc = couch_util:get_value(<<"password_scheme">>, NewDoc),
                NewIterationsDoc = couch_util:get_value(<<"iterations">>, NewDoc),
                NewDerivedKeyDoc = couch_util:get_value(<<"derived_key">>, NewDoc),
                NewDocType = couch_util:get_value(<<"type">>, NewDoc),
                if
                    % we only allow user docs for now
                    NewDocType =/= <<"user">> ->
                        throw({forbidden, <<"doc.type must be user.">>});
                    NewDocName =:= undefined ->
                        throw({forbidden, <<"doc.name is required.">>});
                    NewDocRoles =:= undefined ->
                        throw({forbidden, <<"doc.roles must exist.">>});
                    is_list(NewDocRoles) =:= false ->
                        throw({forbidden, <<"doc.roles must be an array.">>});
                    RolesIsBinary =:= false ->
                        throw({forbidden, <<"doc.roles can only contain strings.">>});
                    NewDocId =/= <<"org.couchdb.user:", NewDocName/binary>> ->
                        throw({
                            forbidden,
                            <<"Doc ID must be of the form org.couchdb.user:name.">>
                        });
                    OldDoc =/= null, OldDocName =/= NewDocName ->
                        throw({forbidden, <<"Usernames can not be changed.">>});
                    NewPassShaDoc, not NewSaltDoc ->
                        throw({
                            forbidden,
                            <<"Users with password_sha must have a salt. ",
                            "See /_utils/script/couch.js for example code.">>
                        });
                    NewPassSchemeDoc =:= <<"pbkdf2">>, not is_number(NewIterationsDoc) ->
                        throw({forbidden, <<"iterations must be a number.">>});
                    NewPassSchemeDoc =:= <<"pbkdf2">>, not is_binary(NewDerivedKeyDoc) ->
                        throw({forbidden, <<"derived_key must be a string.">>});
                    true -> ok
                end,

                % see if the user a database admin specified by name
                Admins = couch_util:get_value(<<"admins">>, SecObj, []),
                DbAdmin = lists:member(NewUserCxtName, Admins),
                DbRoles = couch_util:get_value(<<"roles">>, Admins, []),
                % see if the user a database admin specified by role
                IsAdminRole = if
                    length(Admins) > 0, length(DbRoles) > 0 ->
                        lists:foldl(fun
                            (UserRole, true) -> true;
                            (UserRole, _) -> lists:member(UserRole, DbRoles)
                        end, false, couch_util:get_value(<<"roles">>, UserCtx, []));
                    true -> false
                end,
                IsServerOrDatabaseAdmin = if
                    IsAdmin -> true;
                    SecObj =/= [], DbAdmin -> true;
                    SecObj =/= [], IsAdminRole -> true;
                    true -> false % default to no admin
                end,

                NewRolesSort = lists:sort(NewDocRoles),
                UserCtxName = couch_util:get_value(<<"name">>, UserCtx),
                if
                    OldDoc =/= null, not IsServerOrDatabaseAdmin, NewDocName =/= UserCtxName ->
                        throw({forbidden, <<"You may only update your own user document.">>});
                    OldDoc =/= null, not IsServerOrDatabaseAdmin, length(NewRolesSort) > 0 ->
                        throw({forbidden, <<"Only _admin may set roles.">>});
                    OldDoc =/= null ->
                        % validate role updates
                        {D} = OldDoc,
                        OldRoles = lists:sort(couch_util:get_value(<<"roles">>, D)),
                        if
                            OldRoles =/= NewRolesSort ->
                                throw({forbidden, <<"Only _admin may edit roles.">>});
                            true -> ok
                        end;
                    length(NewRolesSort) > 0, not IsServerOrDatabaseAdmin ->
                        throw({forbidden, <<"Only _admin may edit roles.">>});
                    true -> ok
                end,
                lists:foreach(fun
                    (<<"_", _/bits>>) ->
                        throw({forbidden, <<"No system roles (starting with underscore) in users db.">>});
                    (_) -> ok
                end, NewDocRoles),
                case NewDocName of
                    <<"_", _/bits>> ->
                        throw({forbidden, <<"Username may not start with underscore.">>});
                    _ -> ok
                end,
                BadUserNameChars = [<<":">>],
                lists:foreach(fun(Char) ->
                    CharInName = binary:split(NewDocName, Char),
                    if
                        length(CharInName) > 1 ->
                                throw({forbidden, <<"Character `",
                                    Char/bits, "` is not allowed in usernames.">>});
                        true -> ok
                    end
                end, BadUserNameChars),
                1
        end
    end
).
