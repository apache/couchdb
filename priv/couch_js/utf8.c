// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

#include <jsapi.h>
#include "config.h"

static int
enc_char(uint8 *utf8Buffer, uint32 ucs4Char)
{
    int utf8Length = 1;

    if (ucs4Char < 0x80)
    {
        *utf8Buffer = (uint8)ucs4Char;
    }
    else
    {
        int i;
        uint32 a = ucs4Char >> 11;
        utf8Length = 2;
        while(a)
        {
            a >>= 5;
            utf8Length++;
        }
        i = utf8Length;
        while(--i)
        {
            utf8Buffer[i] = (uint8)((ucs4Char & 0x3F) | 0x80);
            ucs4Char >>= 6;
        }
        *utf8Buffer = (uint8)(0x100 - (1 << (8-utf8Length)) + ucs4Char);
    }

    return utf8Length;
}

static JSBool
enc_charbuf(const jschar* src, size_t srclen, char* dst, size_t* dstlenp)
{
    size_t i;
    size_t utf8Len;
    size_t dstlen = *dstlenp;
    size_t origDstlen = dstlen;
    jschar c;
    jschar c2;
    uint32 v;
    uint8 utf8buf[6];

    if(!dst)
    {
        dstlen = origDstlen = (size_t) -1;
    }

    while(srclen)
    {
        c = *src++;
        srclen--;

        if(c <= 0xD7FF || c >= 0xE000)
        {
            v = (uint32) c;
        }
        else if(c >= 0xD800 && c <= 0xDBFF)
        {
            if(srclen < 1) goto buffer_too_small;
            c2 = *src++;
            srclen--;
            if(c2 >= 0xDC00 && c2 <= 0xDFFF)
            {
                v = (uint32) (((c - 0xD800) << 10) + (c2 - 0xDC00) + 0x10000);
            }
            else
            {
                // Invalid second half of surrogate pair
                v = (uint32) 0xFFFD;
            }
            // Undo our character advancement
            src--;
            srclen++;
        }
        else
        {
            // Invalid first half surrogate pair
            v = (uint32) 0xFFFD;
        }

        if(v < 0x0080)
        {
            /* no encoding necessary - performance hack */
            if(!dstlen) goto buffer_too_small;
            if(dst) *dst++ = (char) v;
            utf8Len = 1;
        }
        else
        {
            utf8Len = enc_char(utf8buf, v);
            if(utf8Len > dstlen) goto buffer_too_small;
            if(dst)
            {
                for (i = 0; i < utf8Len; i++)
                {
                    *dst++ = (char) utf8buf[i];
                }
            }
        }
        dstlen -= utf8Len;
    }
    
    *dstlenp = (origDstlen - dstlen);
    return JS_TRUE;

buffer_too_small:
    *dstlenp = (origDstlen - dstlen);
    return JS_FALSE;
}

char*
enc_string(JSContext* cx, jsval arg, size_t* buflen)
{
    JSString* str = NULL;
    const jschar* src = NULL;
    char* bytes = NULL;
    size_t srclen = 0;
    size_t byteslen = 0;
    
    str = JS_ValueToString(cx, arg);
    if(!str) goto error;

#ifdef HAVE_JS_GET_STRING_CHARS_AND_LENGTH
    src = JS_GetStringCharsAndLength(cx, str, &srclen);
#else
    src = JS_GetStringChars(str);
    srclen = JS_GetStringLength(str);
#endif

    if(!enc_charbuf(src, srclen, NULL, &byteslen)) goto error;
    
    bytes = JS_malloc(cx, (byteslen) + 1);
    bytes[byteslen] = 0;
    
    if(!enc_charbuf(src, srclen, bytes, &byteslen)) goto error;

    if(buflen) *buflen = byteslen;
    goto success;

error:
    if(bytes != NULL) JS_free(cx, bytes);
    bytes = NULL;

success:
    return bytes;
}

static uint32
dec_char(const uint8 *utf8Buffer, int utf8Length)
{
    uint32 ucs4Char;
    uint32 minucs4Char;

    /* from Unicode 3.1, non-shortest form is illegal */
    static const uint32 minucs4Table[] = {
        0x00000080, 0x00000800, 0x0001000, 0x0020000, 0x0400000
    };

    if (utf8Length == 1)
    {
        ucs4Char = *utf8Buffer;
    }
    else
    {
        ucs4Char = *utf8Buffer++ & ((1<<(7-utf8Length))-1);
        minucs4Char = minucs4Table[utf8Length-2];
        while(--utf8Length)
        {
            ucs4Char = ucs4Char<<6 | (*utf8Buffer++ & 0x3F);
        }
        if(ucs4Char < minucs4Char || ucs4Char == 0xFFFE || ucs4Char == 0xFFFF)
        {
            ucs4Char = 0xFFFD;
        }
    }

    return ucs4Char;
}

static JSBool
dec_charbuf(const char *src, size_t srclen, jschar *dst, size_t *dstlenp)
{
    uint32 v;
    size_t offset = 0;
    size_t j;
    size_t n;
    size_t dstlen = *dstlenp;
    size_t origDstlen = dstlen;

    if(!dst) dstlen = origDstlen = (size_t) -1;

    while(srclen)
    {
        v = (uint8) *src;
        n = 1;
        
        if(v & 0x80)
        {
            while(v & (0x80 >> n))
            {
                n++;
            }
            
            if(n > srclen) goto buffer_too_small;
            if(n == 1 || n > 6) goto bad_character;
            
            for(j = 1; j < n; j++)
            {
                if((src[j] & 0xC0) != 0x80) goto bad_character;
            }

            v = dec_char((const uint8 *) src, n);
            if(v >= 0x10000)
            {
                v -= 0x10000;
                
                if(v > 0xFFFFF || dstlen < 2)
                {
                    *dstlenp = (origDstlen - dstlen);
                    return JS_FALSE;
                }
                
                if(dstlen < 2) goto buffer_too_small;

                if(dst)
                {
                    *dst++ = (jschar)((v >> 10) + 0xD800);
                    v = (jschar)((v & 0x3FF) + 0xDC00);
                }
                dstlen--;
            }
        }

        if(!dstlen) goto buffer_too_small;
        if(dst) *dst++ = (jschar) v;

        dstlen--;
        offset += n;
        src += n;
        srclen -= n;
    }

    *dstlenp = (origDstlen - dstlen);
    return JS_TRUE;

bad_character:
    *dstlenp = (origDstlen - dstlen);
    return JS_FALSE;

buffer_too_small:
    *dstlenp = (origDstlen - dstlen);
    return JS_FALSE;
}

JSString*
dec_string(JSContext* cx, const char* bytes, size_t byteslen)
{
    JSString* str = NULL;
    jschar* chars = NULL;
    size_t charslen;
    
    if(!dec_charbuf(bytes, byteslen, NULL, &charslen)) goto error;

    chars = JS_malloc(cx, (charslen + 1) * sizeof(jschar));
    if(!chars) return NULL;
    chars[charslen] = 0;

    if(!dec_charbuf(bytes, byteslen, chars, &charslen)) goto error;

    str = JS_NewUCString(cx, chars, charslen - 1);
    if(!str) goto error;

    goto success;

error:
    if(chars != NULL) JS_free(cx, chars);
    str = NULL;

success:
    return str;
}
