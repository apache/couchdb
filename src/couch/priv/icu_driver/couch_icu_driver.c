/*

Licensed under the Apache License, Version 2.0 (the "License"); you may not use
this file except in compliance with the License. You may obtain a copy of the
License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed
under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied. See the License for the
specific language governing permissions and limitations under the License.

*/

/* This file is the C port driver for Erlang. It provides a low overhead
 * means of calling into C code, however coding errors in this module can
 * crash the entire Erlang server.
 */

#ifdef DARWIN
#define U_HIDE_DRAFT_API 1
#define U_DISABLE_RENAMING 1
#endif

#include "erl_driver.h"
#include "unicode/ucol.h"
#include "unicode/ucasemap.h"
#ifndef WIN32
#include <string.h> /* for memcpy */
#endif


typedef struct {
    ErlDrvPort port;
    UCollator* collNoCase;
    UCollator* coll;
} couch_drv_data;

static void couch_drv_stop(ErlDrvData data)
{
    couch_drv_data* pData = (couch_drv_data*)data;
    if (pData->coll) {
        ucol_close(pData->coll);
    }
    if (pData->collNoCase) {
        ucol_close(pData->collNoCase);
    }
    driver_free((void*)pData);
}

static ErlDrvData couch_drv_start(ErlDrvPort port, char *buff)
{
    UErrorCode status = U_ZERO_ERROR;
    couch_drv_data* pData = (couch_drv_data*)driver_alloc(sizeof(couch_drv_data));

    if (pData == NULL)
        return ERL_DRV_ERROR_GENERAL;

    pData->port = port;

    pData->coll = ucol_open("", &status);
    if (U_FAILURE(status)) {
        couch_drv_stop((ErlDrvData)pData);
        return ERL_DRV_ERROR_GENERAL;
    }

    pData->collNoCase = ucol_open("", &status);
    if (U_FAILURE(status)) {
        couch_drv_stop((ErlDrvData)pData);
        return ERL_DRV_ERROR_GENERAL;
    }

    ucol_setAttribute(pData->collNoCase, UCOL_STRENGTH, UCOL_PRIMARY, &status);
    if (U_FAILURE(status)) {
        couch_drv_stop((ErlDrvData)pData);
        return ERL_DRV_ERROR_GENERAL;
    }

    return (ErlDrvData)pData;
}

ErlDrvSSizeT
return_control_result(void* pLocalResult, int localLen,
            char **ppRetBuf, ErlDrvSizeT returnLen)
{
    if (*ppRetBuf == NULL || localLen > returnLen) {
        *ppRetBuf = (char*)driver_alloc_binary(localLen);
        if(*ppRetBuf == NULL) {
            return -1;
        }
    }
    memcpy(*ppRetBuf, pLocalResult, localLen);
    return localLen;
}

static ErlDrvSSizeT
couch_drv_control(ErlDrvData drv_data, unsigned int command,
        char *pBuf, ErlDrvSizeT bufLen,
        char **rbuf, ErlDrvSizeT rlen)
{

    couch_drv_data* pData = (couch_drv_data*)drv_data;
    switch(command) {
    case 0: /* COLLATE */
    case 1: /* COLLATE_NO_CASE: */
        {
        UErrorCode status = U_ZERO_ERROR;
        int collResult;
        char response;
        UCharIterator iterA;
        UCharIterator iterB;
        int32_t length;

        /* 2 strings are in the buffer, consecutively
         * The strings begin first with a 32 bit integer byte length, then the actual
         * string bytes follow.
         */

        /* first 32bits are the length */
        memcpy(&length, pBuf, sizeof(length));
        pBuf += sizeof(length);

        /* point the iterator at it. */
        uiter_setUTF8(&iterA, pBuf, length);

        pBuf += length; /* now on to string b */

        /* first 32bits are the length */
        memcpy(&length, pBuf, sizeof(length));
        pBuf += sizeof(length);

        /* point the iterator at it. */
        uiter_setUTF8(&iterB, pBuf, length);

        if (command == 0) /* COLLATE */
          collResult = ucol_strcollIter(pData->coll, &iterA, &iterB, &status);
        else              /* COLLATE_NO_CASE */
          collResult = ucol_strcollIter(pData->collNoCase, &iterA, &iterB, &status);

        if (collResult < 0)
          response = 0; /*lt*/
        else if (collResult > 0)
          response = 2; /*gt*/
        else
          response = 1; /*eq*/

        return return_control_result(&response, sizeof(response), rbuf, rlen);
        }

    default:
        return -1;
    }
}

ErlDrvEntry couch_driver_entry = {
        NULL,               /* F_PTR init, N/A */
        couch_drv_start,    /* L_PTR start, called when port is opened */
        couch_drv_stop,     /* F_PTR stop, called when port is closed */
        NULL,               /* F_PTR output, called when erlang has sent */
        NULL,               /* F_PTR ready_input, called when input descriptor ready */
        NULL,               /* F_PTR ready_output, called when output descriptor ready */
        "couch_icu_driver", /* char *driver_name, the argument to open_port */
        NULL,               /* F_PTR finish, called when unloaded */
        NULL,               /* Not used */
        couch_drv_control,  /* F_PTR control, port_command callback */
        NULL,               /* F_PTR timeout, reserved */
        NULL,               /* F_PTR outputv, reserved */
        NULL,               /* F_PTR ready_async */
        NULL,               /* F_PTR flush */
        NULL,               /* F_PTR call */
        NULL,               /* F_PTR event */
        ERL_DRV_EXTENDED_MARKER,
        ERL_DRV_EXTENDED_MAJOR_VERSION,
        ERL_DRV_EXTENDED_MINOR_VERSION,
        ERL_DRV_FLAG_USE_PORT_LOCKING,
        NULL,               /* Reserved -- Used by emulator internally */
        NULL,               /* F_PTR process_exit */
};

DRIVER_INIT(couch_icu_driver) /* must match name in driver_entry */
{
        return &couch_driver_entry;
}
