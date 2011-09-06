#ifndef COUCHJS_SM_H
#define COUCHJS_SM_H

#include "config.h"
#ifdef HAVE_JS_JSAPI_H
#include <js/jsapi.h>
#elif HAVE_MOZJS_JSAPI_H
#include <mozjs/jsapi.h>
#else
#include <jsapi.h>
#endif

#endif // included sm.h
