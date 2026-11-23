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

// Replaces the config.h snappy's CMake build generates from
// cmake/config.h.in. Everything is decided from compiler macros so rebar does
// not need any per-platform checks. SNAPPY_HAVE_BMI2, SNAPPY_HAVE_X86_CRC32
// and SNAPPY_HAVE_NEON_CRC32 are left undefined on purpose: snappy.cc derives
// them from __BMI2__, __SSE4_2__ and __ARM_FEATURE_CRC32.

#ifndef THIRD_PARTY_SNAPPY_OPENSOURCE_CMAKE_CONFIG_H_
#define THIRD_PARTY_SNAPPY_OPENSOURCE_CMAKE_CONFIG_H_

#if defined(__GNUC__) || defined(__clang__)
#define HAVE_ATTRIBUTE_ALWAYS_INLINE 1
#define HAVE_BUILTIN_CTZ 1
#define HAVE_BUILTIN_EXPECT 1
#define HAVE_BUILTIN_PREFETCH 1
#else
#define HAVE_ATTRIBUTE_ALWAYS_INLINE 0
#define HAVE_BUILTIN_CTZ 0
#define HAVE_BUILTIN_EXPECT 0
#define HAVE_BUILTIN_PREFETCH 0
#endif

#if defined(__SSSE3__) || (defined(_MSC_VER) && defined(__AVX__))
#define SNAPPY_HAVE_SSSE3 1
#else
#define SNAPPY_HAVE_SSSE3 0
#endif

#if defined(__ARM_NEON) || defined(__aarch64__)
#define SNAPPY_HAVE_NEON 1
#else
#define SNAPPY_HAVE_NEON 0
#endif

#define SNAPPY_RVV_1 0
#define SNAPPY_RVV_0_7 0

// Wrong endianness does not fail to build, it silently produces frames other
// snappy implementations cannot read, so be explicit about it.
#if defined(__BYTE_ORDER__) && defined(__ORDER_BIG_ENDIAN__) && \
    __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#define SNAPPY_IS_BIG_ENDIAN 1
#elif defined(__BYTE_ORDER__) && defined(__ORDER_LITTLE_ENDIAN__) && \
    __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define SNAPPY_IS_BIG_ENDIAN 0
#elif defined(_WIN32)
#define SNAPPY_IS_BIG_ENDIAN 0
#else
#error "snappy: cannot tell the byte order, define SNAPPY_IS_BIG_ENDIAN"
#endif

#endif  // THIRD_PARTY_SNAPPY_OPENSOURCE_CMAKE_CONFIG_H_
