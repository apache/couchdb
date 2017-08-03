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

-define(REP_DB_DOC_VALIDATE_FUN, <<"
    function(newDoc, oldDoc, userCtx) {
        function reportError(error_msg) {
            log('Error writing document `' + newDoc._id +
                '\\' to the replicator database: ' + error_msg);
            throw({forbidden: error_msg});
        }

        function validateEndpoint(endpoint, fieldName) {
            if ((typeof endpoint !== 'string') &&
                ((typeof endpoint !== 'object') || (endpoint === null))) {

                reportError('The `' + fieldName + '\\' property must exist' +
                    ' and be either a string or an object.');
            }

            if (typeof endpoint === 'object') {
                if ((typeof endpoint.url !== 'string') || !endpoint.url) {
                    reportError('The url property must exist in the `' +
                        fieldName + '\\' field and must be a non-empty string.');
                }

                if ((typeof endpoint.auth !== 'undefined') &&
                    ((typeof endpoint.auth !== 'object') ||
                        endpoint.auth === null)) {

                    reportError('`' + fieldName +
                        '.auth\\' must be a non-null object.');
                }

                if ((typeof endpoint.headers !== 'undefined') &&
                    ((typeof endpoint.headers !== 'object') ||
                        endpoint.headers === null)) {

                    reportError('`' + fieldName +
                        '.headers\\' must be a non-null object.');
                }
            }
        }

        var isReplicator = (userCtx.roles.indexOf('_replicator') >= 0);
        var isAdmin = (userCtx.roles.indexOf('_admin') >= 0);

        if (isReplicator) {
            // Always let replicator update the replication document
            return;
        }

        if (newDoc._replication_state === 'failed') {
            // Skip validation in case when we update the document with the
            // failed state. In this case it might be malformed. However,
            // replicator will not pay attention to failed documents so this
            // is safe.
            return;
        }

        if (!newDoc._deleted) {
            validateEndpoint(newDoc.source, 'source');
            validateEndpoint(newDoc.target, 'target');

            if ((typeof newDoc.create_target !== 'undefined') &&
                (typeof newDoc.create_target !== 'boolean')) {

                reportError('The `create_target\\' field must be a boolean.');
            }

            if ((typeof newDoc.continuous !== 'undefined') &&
                (typeof newDoc.continuous !== 'boolean')) {

                reportError('The `continuous\\' field must be a boolean.');
            }

            if ((typeof newDoc.doc_ids !== 'undefined') &&
                !isArray(newDoc.doc_ids)) {

                reportError('The `doc_ids\\' field must be an array of strings.');
            }

            if ((typeof newDoc.selector !== 'undefined') &&
                (typeof newDoc.selector !== 'object')) {

                reportError('The `selector\\' field must be an object.');
            }

            if ((typeof newDoc.filter !== 'undefined') &&
                ((typeof newDoc.filter !== 'string') || !newDoc.filter)) {

                reportError('The `filter\\' field must be a non-empty string.');
            }

            if ((typeof newDoc.doc_ids !== 'undefined') &&
                (typeof newDoc.selector !== 'undefined')) {

                reportError('`doc_ids\\' field is incompatible with `selector\\'.');
            }

            if ( ((typeof newDoc.doc_ids !== 'undefined') ||
                  (typeof newDoc.selector !== 'undefined')) &&
                 (typeof newDoc.filter !== 'undefined') ) {

                reportError('`filter\\' field is incompatible with `selector\\' and `doc_ids\\'.');
            }

            if ((typeof newDoc.query_params !== 'undefined') &&
                ((typeof newDoc.query_params !== 'object') ||
                    newDoc.query_params === null)) {

                reportError('The `query_params\\' field must be an object.');
            }

            if (newDoc.user_ctx) {
                var user_ctx = newDoc.user_ctx;

                if ((typeof user_ctx !== 'object') || (user_ctx === null)) {
                    reportError('The `user_ctx\\' property must be a ' +
                        'non-null object.');
                }

                if (!(user_ctx.name === null ||
                    (typeof user_ctx.name === 'undefined') ||
                    ((typeof user_ctx.name === 'string') &&
                        user_ctx.name.length > 0))) {

                    reportError('The `user_ctx.name\\' property must be a ' +
                        'non-empty string or null.');
                }

                if (!isAdmin && (user_ctx.name !== userCtx.name)) {
                    reportError('The given `user_ctx.name\\' is not valid');
                }

                if (user_ctx.roles && !isArray(user_ctx.roles)) {
                    reportError('The `user_ctx.roles\\' property must be ' +
                        'an array of strings.');
                }

                if (!isAdmin && user_ctx.roles) {
                    for (var i = 0; i < user_ctx.roles.length; i++) {
                        var role = user_ctx.roles[i];

                        if (typeof role !== 'string' || role.length === 0) {
                            reportError('Roles must be non-empty strings.');
                        }
                        if (userCtx.roles.indexOf(role) === -1) {
                            reportError('Invalid role (`' + role +
                                '\\') in the `user_ctx\\'');
                        }
                    }
                }
            } else {
                if (!isAdmin) {
                    reportError('The `user_ctx\\' property is missing (it is ' +
                       'optional for admins only).');
                }
            }
        } else {
            if (!isAdmin) {
                if (!oldDoc.user_ctx || (oldDoc.user_ctx.name !== userCtx.name)) {
                    reportError('Replication documents can only be deleted by ' +
                        'admins or by the users who created them.');
                }
            }
        }
    }
">>).
