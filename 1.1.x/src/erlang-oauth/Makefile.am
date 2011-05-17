## Licensed under the Apache License, Version 2.0 (the "License"); you may not
## use this file except in compliance with the License.  You may obtain a copy
## of the License at
##
##   http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
## WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
## License for the specific language governing permissions and limitations under
## the License.

oauthebindir = $(localerlanglibdir)/erlang-oauth/ebin

oauth_file_collection = \
	oauth.app.in \
    oauth.erl \
    oauth_hmac_sha1.erl \
    oauth_http.erl \
    oauth_plaintext.erl \
    oauth_rsa_sha1.erl \
    oauth_unix.erl \
    oauth_uri.erl

# Removed oauth_rsa_sha1.beam until we require R12B5 or
# we add a ./configure option to enable it.

oauthebin_make_generated_file_list = \
	oauth.app \
    oauth.beam \
    oauth_hmac_sha1.beam \
    oauth_http.beam \
    oauth_plaintext.beam \
    oauth_unix.beam \
    oauth_uri.beam

oauthebin_DATA = \
	$(oauthebin_make_generated_file_list)

EXTRA_DIST = \
	$(oauth_file_collection)

CLEANFILES = \
    $(oauthebin_make_generated_file_list)

%.app: %.app.in
	cp $< $@

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $<
