/**
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

#include <string.h>
#include <jsapi.h>
#include "erl_nif.h"

ERL_NIF_TERM
get_spidermonkey_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    const char *JAVASCRIPT = "JavaScript-C";
    int js_len = strlen(JAVASCRIPT);

    // JS_GetImplementationVersion()
    // returns "JavaScript-CMAJOR.MINOR.PATCH"
    const char *FULLVERSION = JS_GetImplementationVersion();
    int fv_len = strlen(FULLVERSION);

    const char* foundJSString = strstr(FULLVERSION,JAVASCRIPT);
    if (foundJSString != NULL) {
        //trim off "JavaScript-C",
        char *buf = (char*) malloc((fv_len - js_len + 1) * sizeof(char));
        strncpy(buf, &FULLVERSION[js_len], fv_len - js_len);
        buf[fv_len - js_len] = '\0';
        return enif_make_string(env, buf, ERL_NIF_LATIN1);
    } else {
        //something changed in JS_GetImplementationVersion(), return original
        return enif_make_string(env, FULLVERSION, ERL_NIF_LATIN1);
    }
}

static ErlNifFunc nif_functions[] = {
    {"get_spidermonkey_version", 0, get_spidermonkey_version}
};

#ifdef __cplusplus
extern "C" {
#endif

ERL_NIF_INIT(couch_spidermonkey, nif_functions, NULL, NULL, NULL, NULL);

#ifdef __cplusplus
}
#endif
