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

-define(AUTH_DB_DOC_VALIDATE_FUNCTION, <<"
    function(newDoc, oldDoc, userCtx, secObj) {
        if (newDoc._deleted === true) {
            // allow deletes by admins and matching users
            // without checking the other fields
            if ((userCtx.roles.indexOf('_admin') !== -1) ||
                (userCtx.name == oldDoc.name)) {
                return;
            } else {
                throw({forbidden: 'Only admins may delete other user docs.'});
            }
        }

        if ((oldDoc && oldDoc.type !== 'user') || newDoc.type !== 'user') {
            throw({forbidden : 'doc.type must be user'});
        } // we only allow user docs for now

        if (!newDoc.name) {
            throw({forbidden: 'doc.name is required'});
        }

        if (!newDoc.roles) {
            throw({forbidden: 'doc.roles must exist'});
        }

        if (!isArray(newDoc.roles)) {
            throw({forbidden: 'doc.roles must be an array'});
        }

        for (var idx = 0; idx < newDoc.roles.length; idx++) {
            if (typeof newDoc.roles[idx] !== 'string') {
                throw({forbidden: 'doc.roles can only contain strings'});
            }
        }

        if (newDoc._id !== ('org.couchdb.user:' + newDoc.name)) {
            throw({
                forbidden: 'Doc ID must be of the form org.couchdb.user:name'
            });
        }

        if (oldDoc) { // validate all updates
            if (oldDoc.name !== newDoc.name) {
                throw({forbidden: 'Usernames can not be changed.'});
            }
        }

        if (newDoc.password_sha && !newDoc.salt) {
            throw({
                forbidden: 'Users with password_sha must have a salt.' +
                    'See /_utils/script/couch.js for example code.'
            });
        }

        if (newDoc.password_scheme === \"pbkdf2\") {
            if (typeof(newDoc.iterations) !== \"number\") {
               throw({forbidden: \"iterations must be a number.\"});
            }
            if (typeof(newDoc.derived_key) !== \"string\") {
               throw({forbidden: \"derived_key must be a string.\"});
            }
        }

        var is_server_or_database_admin = function(userCtx, secObj) {
            // see if the user is a server admin
            if(userCtx.roles.indexOf('_admin') !== -1) {
                return true; // a server admin
            }

            // see if the user a database admin specified by name
            if(secObj && secObj.admins && secObj.admins.names) {
                if(secObj.admins.names.indexOf(userCtx.name) !== -1) {
                    return true; // database admin
                }
            }

            // see if the user a database admin specified by role
            if(secObj && secObj.admins && secObj.admins.roles) {
                var db_roles = secObj.admins.roles;
                for(var idx = 0; idx < userCtx.roles.length; idx++) {
                    var user_role = userCtx.roles[idx];
                    if(db_roles.indexOf(user_role) !== -1) {
                        return true; // role matches!
                    }
                }
            }

            return false; // default to no admin
        }

        if (!is_server_or_database_admin(userCtx, secObj)) {
            if (oldDoc) { // validate non-admin updates
                if (userCtx.name !== newDoc.name) {
                    throw({
                        forbidden: 'You may only update your own user document.'
                    });
                }
                // validate role updates
                var oldRoles = oldDoc.roles.sort();
                var newRoles = newDoc.roles.sort();

                if (oldRoles.length !== newRoles.length) {
                    throw({forbidden: 'Only _admin may edit roles'});
                }

                for (var i = 0; i < oldRoles.length; i++) {
                    if (oldRoles[i] !== newRoles[i]) {
                        throw({forbidden: 'Only _admin may edit roles'});
                    }
                }
            } else if (newDoc.roles.length > 0) {
                throw({forbidden: 'Only _admin may set roles'});
            }
        }

        // no system roles in users db
        for (var i = 0; i < newDoc.roles.length; i++) {
            if (newDoc.roles[i][0] === '_') {
                throw({
                    forbidden:
                    'No system roles (starting with underscore) in users db.'
                });
            }
        }

        // no system names as names
        if (newDoc.name[0] === '_') {
            throw({forbidden: 'Username may not start with underscore.'});
        }

        var badUserNameChars = [':'];

        for (var i = 0; i < badUserNameChars.length; i++) {
            if (newDoc.name.indexOf(badUserNameChars[i]) >= 0) {
                throw({forbidden: 'Character `' + badUserNameChars[i] +
                        '` is not allowed in usernames.'});
            }
        }
    }
">>).


-define(OAUTH_MAP_FUN, <<"
    function(doc) {
        if (doc.type === 'user' && doc.oauth && doc.oauth.consumer_keys) {
            for (var consumer_key in doc.oauth.consumer_keys) {
                for (var token in doc.oauth.tokens) {
                    var obj = {
                        'consumer_secret': doc.oauth.consumer_keys[consumer_key],
                        'token_secret': doc.oauth.tokens[token],
                        'username': doc.name
                    };
                    emit([consumer_key, token], obj);
                }
            }
        }
    }
">>).
