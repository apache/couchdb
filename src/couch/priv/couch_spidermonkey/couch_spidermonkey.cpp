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

#include <string>
#include <jsapi.h>
#include "erl_nif.h"

using namespace std;

ERL_NIF_TERM
get_spidermonkey_version(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    const string JAVASCRIPT = "JavaScript-C";
    int js_len = JAVASCRIPT.length();

    // JS_GetImplementationVersion()
    // returns "JavaScript-CMAJOR.MINOR.PATCH"
    const string FULLVERSION = JS_GetImplementationVersion();

    string result;
    size_t foundJSString = FULLVERSION.find(JAVASCRIPT);
    if (foundJSString != string::npos) {
        //trim off "JavaScript-C",
        result = FULLVERSION.substr(js_len);
    } else {
        //something changed in JS_GetImplementationVersion(), return original
        result = FULLVERSION;
    }

    return enif_make_string(env, result.c_str(), ERL_NIF_LATIN1);
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