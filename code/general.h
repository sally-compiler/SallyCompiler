#ifndef GENERAL_H
#define GENERAL_H

#include <stdint.h>
#include <stdarg.h>
#include <stdio.h> // vprintf printf
#include <stdlib.h> // exit
#include <assert.h>
#include <utility> // std::pair
#include <map>
#include <set>
#include <queue>

#include <cstring> // strcpy, strlen

#include "array.h"

// @TODO: printf for debugging
// can be turned on/off by a flag

typedef uint8_t  uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;
typedef uint64_t uint64;

typedef int8_t  int8;
typedef int16_t int16;
typedef int32_t int32;
typedef int64_t int64;

typedef int8   bool8;
typedef int32  bool32;

typedef float real32;
typedef double real64;

template <typename A, typename B>
using Pair = std::pair<A, B>;

template <typename A, typename B>
using Map = std::map<A, B>;

template <typename A, typename Compare = std::less<A>>
using Set = std::set<A, Compare>;

template <typename A>
using Queue = std::queue<A>;

struct String_Builder {
    Array<char> buffer;

    size_t size() { return buffer.len; }
    char *c_str() { return buffer.a; } // @NOTE Don't append after acquiring this!!!
    void add_terminator() { buffer.push('\0'); }
    void append(const char *s, ...);
};


void report_error(const char *format, ...);

bool fits_into(int64 v, uint8 bits);

#endif
