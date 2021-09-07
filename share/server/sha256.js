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

// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.
/*
 * Adapted to deno from:
 *
 * [js-sha256]{@link https://github.com/emn178/js-sha256}
 *
 * @version 0.9.0
 * @author Chen, Yi-Cyuan [emn178@gmail.com]
 * @copyright Chen, Yi-Cyuan 2014-2017
 * @license MIT
 */
 // generated from sha256.ts with these commands:
 // $ npm install -g typescript
 // $ tsc --target ES6 sha256.ts
 // manually remove the two uses of `export class` and just have it be `class`
var __classPrivateFieldSet = (this && this.__classPrivateFieldSet) || function (receiver, state, value, kind, f) {
    if (kind === "m") throw new TypeError("Private method is not writable");
    if (kind === "a" && !f) throw new TypeError("Private accessor was defined without a setter");
    if (typeof state === "function" ? receiver !== state || !f : !state.has(receiver)) throw new TypeError("Cannot write private member to an object whose class did not declare it");
    return (kind === "a" ? f.call(receiver, value) : f ? f.value = value : state.set(receiver, value)), value;
};
var __classPrivateFieldGet = (this && this.__classPrivateFieldGet) || function (receiver, state, kind, f) {
    if (kind === "a" && !f) throw new TypeError("Private accessor was defined without a getter");
    if (typeof state === "function" ? receiver !== state || !f : !state.has(receiver)) throw new TypeError("Cannot read private member from an object whose class did not declare it");
    return kind === "m" ? f : kind === "a" ? f.call(receiver) : f ? f.value : state.get(receiver);
};
var _Sha256_block, _Sha256_blocks, _Sha256_bytes, _Sha256_finalized, _Sha256_first, _Sha256_h0, _Sha256_h1, _Sha256_h2, _Sha256_h3, _Sha256_h4, _Sha256_h5, _Sha256_h6, _Sha256_h7, _Sha256_hashed, _Sha256_hBytes, _Sha256_is224, _Sha256_lastByteIndex, _Sha256_start, _HmacSha256_inner, _HmacSha256_is224, _HmacSha256_oKeyPad, _HmacSha256_sharedMemory;
const HEX_CHARS = "0123456789abcdef".split("");
const EXTRA = [-2147483648, 8388608, 32768, 128];
const SHIFT = [24, 16, 8, 0];
// deno-fmt-ignore
const K = [
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1,
    0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
    0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147,
    0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
    0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
    0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
];
const blocks = [];
class Sha256 {
    constructor(is224 = false, sharedMemory = false) {
        _Sha256_block.set(this, void 0);
        _Sha256_blocks.set(this, void 0);
        _Sha256_bytes.set(this, void 0);
        _Sha256_finalized.set(this, void 0);
        _Sha256_first.set(this, void 0);
        _Sha256_h0.set(this, void 0);
        _Sha256_h1.set(this, void 0);
        _Sha256_h2.set(this, void 0);
        _Sha256_h3.set(this, void 0);
        _Sha256_h4.set(this, void 0);
        _Sha256_h5.set(this, void 0);
        _Sha256_h6.set(this, void 0);
        _Sha256_h7.set(this, void 0);
        _Sha256_hashed.set(this, void 0);
        _Sha256_hBytes.set(this, void 0);
        _Sha256_is224.set(this, void 0);
        _Sha256_lastByteIndex.set(this, 0);
        _Sha256_start.set(this, void 0);
        this.init(is224, sharedMemory);
    }
    init(is224, sharedMemory) {
        if (sharedMemory) {
            // deno-fmt-ignore
            blocks[0] = blocks[16] = blocks[1] = blocks[2] = blocks[3] = blocks[4] = blocks[5] = blocks[6] = blocks[7] = blocks[8] = blocks[9] = blocks[10] = blocks[11] = blocks[12] = blocks[13] = blocks[14] = blocks[15] = 0;
            __classPrivateFieldSet(this, _Sha256_blocks, blocks, "f");
        }
        else {
            __classPrivateFieldSet(this, _Sha256_blocks, [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], "f");
        }
        if (is224) {
            __classPrivateFieldSet(this, _Sha256_h0, 0xc1059ed8, "f");
            __classPrivateFieldSet(this, _Sha256_h1, 0x367cd507, "f");
            __classPrivateFieldSet(this, _Sha256_h2, 0x3070dd17, "f");
            __classPrivateFieldSet(this, _Sha256_h3, 0xf70e5939, "f");
            __classPrivateFieldSet(this, _Sha256_h4, 0xffc00b31, "f");
            __classPrivateFieldSet(this, _Sha256_h5, 0x68581511, "f");
            __classPrivateFieldSet(this, _Sha256_h6, 0x64f98fa7, "f");
            __classPrivateFieldSet(this, _Sha256_h7, 0xbefa4fa4, "f");
        }
        else {
            // 256
            __classPrivateFieldSet(this, _Sha256_h0, 0x6a09e667, "f");
            __classPrivateFieldSet(this, _Sha256_h1, 0xbb67ae85, "f");
            __classPrivateFieldSet(this, _Sha256_h2, 0x3c6ef372, "f");
            __classPrivateFieldSet(this, _Sha256_h3, 0xa54ff53a, "f");
            __classPrivateFieldSet(this, _Sha256_h4, 0x510e527f, "f");
            __classPrivateFieldSet(this, _Sha256_h5, 0x9b05688c, "f");
            __classPrivateFieldSet(this, _Sha256_h6, 0x1f83d9ab, "f");
            __classPrivateFieldSet(this, _Sha256_h7, 0x5be0cd19, "f");
        }
        __classPrivateFieldSet(this, _Sha256_block, __classPrivateFieldSet(this, _Sha256_start, __classPrivateFieldSet(this, _Sha256_bytes, __classPrivateFieldSet(this, _Sha256_hBytes, 0, "f"), "f"), "f"), "f");
        __classPrivateFieldSet(this, _Sha256_finalized, __classPrivateFieldSet(this, _Sha256_hashed, false, "f"), "f");
        __classPrivateFieldSet(this, _Sha256_first, true, "f");
        __classPrivateFieldSet(this, _Sha256_is224, is224, "f");
    }
    /** Update hash
     *
     * @param message The message you want to hash.
     */
    update(message) {
        if (__classPrivateFieldGet(this, _Sha256_finalized, "f")) {
            return this;
        }
        let msg;
        if (message instanceof ArrayBuffer) {
            msg = new Uint8Array(message);
        }
        else {
            msg = message;
        }
        let index = 0;
        const length = msg.length;
        const blocks = __classPrivateFieldGet(this, _Sha256_blocks, "f");
        while (index < length) {
            let i;
            if (__classPrivateFieldGet(this, _Sha256_hashed, "f")) {
                __classPrivateFieldSet(this, _Sha256_hashed, false, "f");
                blocks[0] = __classPrivateFieldGet(this, _Sha256_block, "f");
                // deno-fmt-ignore
                blocks[16] = blocks[1] = blocks[2] = blocks[3] = blocks[4] = blocks[5] = blocks[6] = blocks[7] = blocks[8] = blocks[9] = blocks[10] = blocks[11] = blocks[12] = blocks[13] = blocks[14] = blocks[15] = 0;
            }
            if (typeof msg !== "string") {
                for (i = __classPrivateFieldGet(this, _Sha256_start, "f"); index < length && i < 64; ++index) {
                    blocks[i >> 2] |= msg[index] << SHIFT[i++ & 3];
                }
            }
            else {
                for (i = __classPrivateFieldGet(this, _Sha256_start, "f"); index < length && i < 64; ++index) {
                    let code = msg.charCodeAt(index);
                    if (code < 0x80) {
                        blocks[i >> 2] |= code << SHIFT[i++ & 3];
                    }
                    else if (code < 0x800) {
                        blocks[i >> 2] |= (0xc0 | (code >> 6)) << SHIFT[i++ & 3];
                        blocks[i >> 2] |= (0x80 | (code & 0x3f)) << SHIFT[i++ & 3];
                    }
                    else if (code < 0xd800 || code >= 0xe000) {
                        blocks[i >> 2] |= (0xe0 | (code >> 12)) << SHIFT[i++ & 3];
                        blocks[i >> 2] |= (0x80 | ((code >> 6) & 0x3f)) << SHIFT[i++ & 3];
                        blocks[i >> 2] |= (0x80 | (code & 0x3f)) << SHIFT[i++ & 3];
                    }
                    else {
                        code = 0x10000 +
                            (((code & 0x3ff) << 10) | (msg.charCodeAt(++index) & 0x3ff));
                        blocks[i >> 2] |= (0xf0 | (code >> 18)) << SHIFT[i++ & 3];
                        blocks[i >> 2] |= (0x80 | ((code >> 12) & 0x3f)) << SHIFT[i++ & 3];
                        blocks[i >> 2] |= (0x80 | ((code >> 6) & 0x3f)) << SHIFT[i++ & 3];
                        blocks[i >> 2] |= (0x80 | (code & 0x3f)) << SHIFT[i++ & 3];
                    }
                }
            }
            __classPrivateFieldSet(this, _Sha256_lastByteIndex, i, "f");
            __classPrivateFieldSet(this, _Sha256_bytes, __classPrivateFieldGet(this, _Sha256_bytes, "f") + (i - __classPrivateFieldGet(this, _Sha256_start, "f")), "f");
            if (i >= 64) {
                __classPrivateFieldSet(this, _Sha256_block, blocks[16], "f");
                __classPrivateFieldSet(this, _Sha256_start, i - 64, "f");
                this.hash();
                __classPrivateFieldSet(this, _Sha256_hashed, true, "f");
            }
            else {
                __classPrivateFieldSet(this, _Sha256_start, i, "f");
            }
        }
        if (__classPrivateFieldGet(this, _Sha256_bytes, "f") > 4294967295) {
            __classPrivateFieldSet(this, _Sha256_hBytes, __classPrivateFieldGet(this, _Sha256_hBytes, "f") + ((__classPrivateFieldGet(this, _Sha256_bytes, "f") / 4294967296) << 0), "f");
            __classPrivateFieldSet(this, _Sha256_bytes, __classPrivateFieldGet(this, _Sha256_bytes, "f") % 4294967296, "f");
        }
        return this;
    }
    finalize() {
        if (__classPrivateFieldGet(this, _Sha256_finalized, "f")) {
            return;
        }
        __classPrivateFieldSet(this, _Sha256_finalized, true, "f");
        const blocks = __classPrivateFieldGet(this, _Sha256_blocks, "f");
        const i = __classPrivateFieldGet(this, _Sha256_lastByteIndex, "f");
        blocks[16] = __classPrivateFieldGet(this, _Sha256_block, "f");
        blocks[i >> 2] |= EXTRA[i & 3];
        __classPrivateFieldSet(this, _Sha256_block, blocks[16], "f");
        if (i >= 56) {
            if (!__classPrivateFieldGet(this, _Sha256_hashed, "f")) {
                this.hash();
            }
            blocks[0] = __classPrivateFieldGet(this, _Sha256_block, "f");
            // deno-fmt-ignore
            blocks[16] = blocks[1] = blocks[2] = blocks[3] = blocks[4] = blocks[5] = blocks[6] = blocks[7] = blocks[8] = blocks[9] = blocks[10] = blocks[11] = blocks[12] = blocks[13] = blocks[14] = blocks[15] = 0;
        }
        blocks[14] = (__classPrivateFieldGet(this, _Sha256_hBytes, "f") << 3) | (__classPrivateFieldGet(this, _Sha256_bytes, "f") >>> 29);
        blocks[15] = __classPrivateFieldGet(this, _Sha256_bytes, "f") << 3;
        this.hash();
    }
    hash() {
        let a = __classPrivateFieldGet(this, _Sha256_h0, "f");
        let b = __classPrivateFieldGet(this, _Sha256_h1, "f");
        let c = __classPrivateFieldGet(this, _Sha256_h2, "f");
        let d = __classPrivateFieldGet(this, _Sha256_h3, "f");
        let e = __classPrivateFieldGet(this, _Sha256_h4, "f");
        let f = __classPrivateFieldGet(this, _Sha256_h5, "f");
        let g = __classPrivateFieldGet(this, _Sha256_h6, "f");
        let h = __classPrivateFieldGet(this, _Sha256_h7, "f");
        const blocks = __classPrivateFieldGet(this, _Sha256_blocks, "f");
        let s0;
        let s1;
        let maj;
        let t1;
        let t2;
        let ch;
        let ab;
        let da;
        let cd;
        let bc;
        for (let j = 16; j < 64; ++j) {
            // rightrotate
            t1 = blocks[j - 15];
            s0 = ((t1 >>> 7) | (t1 << 25)) ^ ((t1 >>> 18) | (t1 << 14)) ^ (t1 >>> 3);
            t1 = blocks[j - 2];
            s1 = ((t1 >>> 17) | (t1 << 15)) ^ ((t1 >>> 19) | (t1 << 13)) ^
                (t1 >>> 10);
            blocks[j] = (blocks[j - 16] + s0 + blocks[j - 7] + s1) << 0;
        }
        bc = b & c;
        for (let j = 0; j < 64; j += 4) {
            if (__classPrivateFieldGet(this, _Sha256_first, "f")) {
                if (__classPrivateFieldGet(this, _Sha256_is224, "f")) {
                    ab = 300032;
                    t1 = blocks[0] - 1413257819;
                    h = (t1 - 150054599) << 0;
                    d = (t1 + 24177077) << 0;
                }
                else {
                    ab = 704751109;
                    t1 = blocks[0] - 210244248;
                    h = (t1 - 1521486534) << 0;
                    d = (t1 + 143694565) << 0;
                }
                __classPrivateFieldSet(this, _Sha256_first, false, "f");
            }
            else {
                s0 = ((a >>> 2) | (a << 30)) ^
                    ((a >>> 13) | (a << 19)) ^
                    ((a >>> 22) | (a << 10));
                s1 = ((e >>> 6) | (e << 26)) ^
                    ((e >>> 11) | (e << 21)) ^
                    ((e >>> 25) | (e << 7));
                ab = a & b;
                maj = ab ^ (a & c) ^ bc;
                ch = (e & f) ^ (~e & g);
                t1 = h + s1 + ch + K[j] + blocks[j];
                t2 = s0 + maj;
                h = (d + t1) << 0;
                d = (t1 + t2) << 0;
            }
            s0 = ((d >>> 2) | (d << 30)) ^
                ((d >>> 13) | (d << 19)) ^
                ((d >>> 22) | (d << 10));
            s1 = ((h >>> 6) | (h << 26)) ^
                ((h >>> 11) | (h << 21)) ^
                ((h >>> 25) | (h << 7));
            da = d & a;
            maj = da ^ (d & b) ^ ab;
            ch = (h & e) ^ (~h & f);
            t1 = g + s1 + ch + K[j + 1] + blocks[j + 1];
            t2 = s0 + maj;
            g = (c + t1) << 0;
            c = (t1 + t2) << 0;
            s0 = ((c >>> 2) | (c << 30)) ^
                ((c >>> 13) | (c << 19)) ^
                ((c >>> 22) | (c << 10));
            s1 = ((g >>> 6) | (g << 26)) ^
                ((g >>> 11) | (g << 21)) ^
                ((g >>> 25) | (g << 7));
            cd = c & d;
            maj = cd ^ (c & a) ^ da;
            ch = (g & h) ^ (~g & e);
            t1 = f + s1 + ch + K[j + 2] + blocks[j + 2];
            t2 = s0 + maj;
            f = (b + t1) << 0;
            b = (t1 + t2) << 0;
            s0 = ((b >>> 2) | (b << 30)) ^
                ((b >>> 13) | (b << 19)) ^
                ((b >>> 22) | (b << 10));
            s1 = ((f >>> 6) | (f << 26)) ^
                ((f >>> 11) | (f << 21)) ^
                ((f >>> 25) | (f << 7));
            bc = b & c;
            maj = bc ^ (b & d) ^ cd;
            ch = (f & g) ^ (~f & h);
            t1 = e + s1 + ch + K[j + 3] + blocks[j + 3];
            t2 = s0 + maj;
            e = (a + t1) << 0;
            a = (t1 + t2) << 0;
        }
        __classPrivateFieldSet(this, _Sha256_h0, (__classPrivateFieldGet(this, _Sha256_h0, "f") + a) << 0, "f");
        __classPrivateFieldSet(this, _Sha256_h1, (__classPrivateFieldGet(this, _Sha256_h1, "f") + b) << 0, "f");
        __classPrivateFieldSet(this, _Sha256_h2, (__classPrivateFieldGet(this, _Sha256_h2, "f") + c) << 0, "f");
        __classPrivateFieldSet(this, _Sha256_h3, (__classPrivateFieldGet(this, _Sha256_h3, "f") + d) << 0, "f");
        __classPrivateFieldSet(this, _Sha256_h4, (__classPrivateFieldGet(this, _Sha256_h4, "f") + e) << 0, "f");
        __classPrivateFieldSet(this, _Sha256_h5, (__classPrivateFieldGet(this, _Sha256_h5, "f") + f) << 0, "f");
        __classPrivateFieldSet(this, _Sha256_h6, (__classPrivateFieldGet(this, _Sha256_h6, "f") + g) << 0, "f");
        __classPrivateFieldSet(this, _Sha256_h7, (__classPrivateFieldGet(this, _Sha256_h7, "f") + h) << 0, "f");
    }
    /** Return hash in hex string. */
    hex() {
        this.finalize();
        const h0 = __classPrivateFieldGet(this, _Sha256_h0, "f");
        const h1 = __classPrivateFieldGet(this, _Sha256_h1, "f");
        const h2 = __classPrivateFieldGet(this, _Sha256_h2, "f");
        const h3 = __classPrivateFieldGet(this, _Sha256_h3, "f");
        const h4 = __classPrivateFieldGet(this, _Sha256_h4, "f");
        const h5 = __classPrivateFieldGet(this, _Sha256_h5, "f");
        const h6 = __classPrivateFieldGet(this, _Sha256_h6, "f");
        const h7 = __classPrivateFieldGet(this, _Sha256_h7, "f");
        let hex = HEX_CHARS[(h0 >> 28) & 0x0f] +
            HEX_CHARS[(h0 >> 24) & 0x0f] +
            HEX_CHARS[(h0 >> 20) & 0x0f] +
            HEX_CHARS[(h0 >> 16) & 0x0f] +
            HEX_CHARS[(h0 >> 12) & 0x0f] +
            HEX_CHARS[(h0 >> 8) & 0x0f] +
            HEX_CHARS[(h0 >> 4) & 0x0f] +
            HEX_CHARS[h0 & 0x0f] +
            HEX_CHARS[(h1 >> 28) & 0x0f] +
            HEX_CHARS[(h1 >> 24) & 0x0f] +
            HEX_CHARS[(h1 >> 20) & 0x0f] +
            HEX_CHARS[(h1 >> 16) & 0x0f] +
            HEX_CHARS[(h1 >> 12) & 0x0f] +
            HEX_CHARS[(h1 >> 8) & 0x0f] +
            HEX_CHARS[(h1 >> 4) & 0x0f] +
            HEX_CHARS[h1 & 0x0f] +
            HEX_CHARS[(h2 >> 28) & 0x0f] +
            HEX_CHARS[(h2 >> 24) & 0x0f] +
            HEX_CHARS[(h2 >> 20) & 0x0f] +
            HEX_CHARS[(h2 >> 16) & 0x0f] +
            HEX_CHARS[(h2 >> 12) & 0x0f] +
            HEX_CHARS[(h2 >> 8) & 0x0f] +
            HEX_CHARS[(h2 >> 4) & 0x0f] +
            HEX_CHARS[h2 & 0x0f] +
            HEX_CHARS[(h3 >> 28) & 0x0f] +
            HEX_CHARS[(h3 >> 24) & 0x0f] +
            HEX_CHARS[(h3 >> 20) & 0x0f] +
            HEX_CHARS[(h3 >> 16) & 0x0f] +
            HEX_CHARS[(h3 >> 12) & 0x0f] +
            HEX_CHARS[(h3 >> 8) & 0x0f] +
            HEX_CHARS[(h3 >> 4) & 0x0f] +
            HEX_CHARS[h3 & 0x0f] +
            HEX_CHARS[(h4 >> 28) & 0x0f] +
            HEX_CHARS[(h4 >> 24) & 0x0f] +
            HEX_CHARS[(h4 >> 20) & 0x0f] +
            HEX_CHARS[(h4 >> 16) & 0x0f] +
            HEX_CHARS[(h4 >> 12) & 0x0f] +
            HEX_CHARS[(h4 >> 8) & 0x0f] +
            HEX_CHARS[(h4 >> 4) & 0x0f] +
            HEX_CHARS[h4 & 0x0f] +
            HEX_CHARS[(h5 >> 28) & 0x0f] +
            HEX_CHARS[(h5 >> 24) & 0x0f] +
            HEX_CHARS[(h5 >> 20) & 0x0f] +
            HEX_CHARS[(h5 >> 16) & 0x0f] +
            HEX_CHARS[(h5 >> 12) & 0x0f] +
            HEX_CHARS[(h5 >> 8) & 0x0f] +
            HEX_CHARS[(h5 >> 4) & 0x0f] +
            HEX_CHARS[h5 & 0x0f] +
            HEX_CHARS[(h6 >> 28) & 0x0f] +
            HEX_CHARS[(h6 >> 24) & 0x0f] +
            HEX_CHARS[(h6 >> 20) & 0x0f] +
            HEX_CHARS[(h6 >> 16) & 0x0f] +
            HEX_CHARS[(h6 >> 12) & 0x0f] +
            HEX_CHARS[(h6 >> 8) & 0x0f] +
            HEX_CHARS[(h6 >> 4) & 0x0f] +
            HEX_CHARS[h6 & 0x0f];
        if (!__classPrivateFieldGet(this, _Sha256_is224, "f")) {
            hex += HEX_CHARS[(h7 >> 28) & 0x0f] +
                HEX_CHARS[(h7 >> 24) & 0x0f] +
                HEX_CHARS[(h7 >> 20) & 0x0f] +
                HEX_CHARS[(h7 >> 16) & 0x0f] +
                HEX_CHARS[(h7 >> 12) & 0x0f] +
                HEX_CHARS[(h7 >> 8) & 0x0f] +
                HEX_CHARS[(h7 >> 4) & 0x0f] +
                HEX_CHARS[h7 & 0x0f];
        }
        return hex;
    }
    /** Return hash in hex string. */
    toString() {
        return this.hex();
    }
    /** Return hash in integer array. */
    digest() {
        this.finalize();
        const h0 = __classPrivateFieldGet(this, _Sha256_h0, "f");
        const h1 = __classPrivateFieldGet(this, _Sha256_h1, "f");
        const h2 = __classPrivateFieldGet(this, _Sha256_h2, "f");
        const h3 = __classPrivateFieldGet(this, _Sha256_h3, "f");
        const h4 = __classPrivateFieldGet(this, _Sha256_h4, "f");
        const h5 = __classPrivateFieldGet(this, _Sha256_h5, "f");
        const h6 = __classPrivateFieldGet(this, _Sha256_h6, "f");
        const h7 = __classPrivateFieldGet(this, _Sha256_h7, "f");
        const arr = [
            (h0 >> 24) & 0xff,
            (h0 >> 16) & 0xff,
            (h0 >> 8) & 0xff,
            h0 & 0xff,
            (h1 >> 24) & 0xff,
            (h1 >> 16) & 0xff,
            (h1 >> 8) & 0xff,
            h1 & 0xff,
            (h2 >> 24) & 0xff,
            (h2 >> 16) & 0xff,
            (h2 >> 8) & 0xff,
            h2 & 0xff,
            (h3 >> 24) & 0xff,
            (h3 >> 16) & 0xff,
            (h3 >> 8) & 0xff,
            h3 & 0xff,
            (h4 >> 24) & 0xff,
            (h4 >> 16) & 0xff,
            (h4 >> 8) & 0xff,
            h4 & 0xff,
            (h5 >> 24) & 0xff,
            (h5 >> 16) & 0xff,
            (h5 >> 8) & 0xff,
            h5 & 0xff,
            (h6 >> 24) & 0xff,
            (h6 >> 16) & 0xff,
            (h6 >> 8) & 0xff,
            h6 & 0xff,
        ];
        if (!__classPrivateFieldGet(this, _Sha256_is224, "f")) {
            arr.push((h7 >> 24) & 0xff, (h7 >> 16) & 0xff, (h7 >> 8) & 0xff, h7 & 0xff);
        }
        return arr;
    }
    /** Return hash in integer array. */
    array() {
        return this.digest();
    }
    /** Return hash in ArrayBuffer. */
    arrayBuffer() {
        this.finalize();
        const buffer = new ArrayBuffer(__classPrivateFieldGet(this, _Sha256_is224, "f") ? 28 : 32);
        const dataView = new DataView(buffer);
        dataView.setUint32(0, __classPrivateFieldGet(this, _Sha256_h0, "f"));
        dataView.setUint32(4, __classPrivateFieldGet(this, _Sha256_h1, "f"));
        dataView.setUint32(8, __classPrivateFieldGet(this, _Sha256_h2, "f"));
        dataView.setUint32(12, __classPrivateFieldGet(this, _Sha256_h3, "f"));
        dataView.setUint32(16, __classPrivateFieldGet(this, _Sha256_h4, "f"));
        dataView.setUint32(20, __classPrivateFieldGet(this, _Sha256_h5, "f"));
        dataView.setUint32(24, __classPrivateFieldGet(this, _Sha256_h6, "f"));
        if (!__classPrivateFieldGet(this, _Sha256_is224, "f")) {
            dataView.setUint32(28, __classPrivateFieldGet(this, _Sha256_h7, "f"));
        }
        return buffer;
    }
}
_Sha256_block = new WeakMap(), _Sha256_blocks = new WeakMap(), _Sha256_bytes = new WeakMap(), _Sha256_finalized = new WeakMap(), _Sha256_first = new WeakMap(), _Sha256_h0 = new WeakMap(), _Sha256_h1 = new WeakMap(), _Sha256_h2 = new WeakMap(), _Sha256_h3 = new WeakMap(), _Sha256_h4 = new WeakMap(), _Sha256_h5 = new WeakMap(), _Sha256_h6 = new WeakMap(), _Sha256_h7 = new WeakMap(), _Sha256_hashed = new WeakMap(), _Sha256_hBytes = new WeakMap(), _Sha256_is224 = new WeakMap(), _Sha256_lastByteIndex = new WeakMap(), _Sha256_start = new WeakMap();
class HmacSha256 extends Sha256 {
    constructor(secretKey, is224 = false, sharedMemory = false) {
        super(is224, sharedMemory);
        _HmacSha256_inner.set(this, void 0);
        _HmacSha256_is224.set(this, void 0);
        _HmacSha256_oKeyPad.set(this, void 0);
        _HmacSha256_sharedMemory.set(this, void 0);
        let key;
        if (typeof secretKey === "string") {
            const bytes = [];
            const length = secretKey.length;
            let index = 0;
            for (let i = 0; i < length; ++i) {
                let code = secretKey.charCodeAt(i);
                if (code < 0x80) {
                    bytes[index++] = code;
                }
                else if (code < 0x800) {
                    bytes[index++] = 0xc0 | (code >> 6);
                    bytes[index++] = 0x80 | (code & 0x3f);
                }
                else if (code < 0xd800 || code >= 0xe000) {
                    bytes[index++] = 0xe0 | (code >> 12);
                    bytes[index++] = 0x80 | ((code >> 6) & 0x3f);
                    bytes[index++] = 0x80 | (code & 0x3f);
                }
                else {
                    code = 0x10000 +
                        (((code & 0x3ff) << 10) | (secretKey.charCodeAt(++i) & 0x3ff));
                    bytes[index++] = 0xf0 | (code >> 18);
                    bytes[index++] = 0x80 | ((code >> 12) & 0x3f);
                    bytes[index++] = 0x80 | ((code >> 6) & 0x3f);
                    bytes[index++] = 0x80 | (code & 0x3f);
                }
            }
            key = bytes;
        }
        else {
            if (secretKey instanceof ArrayBuffer) {
                key = new Uint8Array(secretKey);
            }
            else {
                key = secretKey;
            }
        }
        if (key.length > 64) {
            key = new Sha256(is224, true).update(key).array();
        }
        const oKeyPad = [];
        const iKeyPad = [];
        for (let i = 0; i < 64; ++i) {
            const b = key[i] || 0;
            oKeyPad[i] = 0x5c ^ b;
            iKeyPad[i] = 0x36 ^ b;
        }
        this.update(iKeyPad);
        __classPrivateFieldSet(this, _HmacSha256_oKeyPad, oKeyPad, "f");
        __classPrivateFieldSet(this, _HmacSha256_inner, true, "f");
        __classPrivateFieldSet(this, _HmacSha256_is224, is224, "f");
        __classPrivateFieldSet(this, _HmacSha256_sharedMemory, sharedMemory, "f");
    }
    finalize() {
        super.finalize();
        if (__classPrivateFieldGet(this, _HmacSha256_inner, "f")) {
            __classPrivateFieldSet(this, _HmacSha256_inner, false, "f");
            const innerHash = this.array();
            super.init(__classPrivateFieldGet(this, _HmacSha256_is224, "f"), __classPrivateFieldGet(this, _HmacSha256_sharedMemory, "f"));
            this.update(__classPrivateFieldGet(this, _HmacSha256_oKeyPad, "f"));
            this.update(innerHash);
            super.finalize();
        }
    }
}
_HmacSha256_inner = new WeakMap(), _HmacSha256_is224 = new WeakMap(), _HmacSha256_oKeyPad = new WeakMap(), _HmacSha256_sharedMemory = new WeakMap();
