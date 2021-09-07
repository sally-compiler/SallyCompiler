#include "general.h"

void report_error(const char *format, ...) {
    va_list args;
    va_start(args, format);

    vprintf(format, args);
    printf("\n");

    va_end(args);
    assert(false);
}

// @TODO: ditch sprintf because performance is too bad
// but for debugging purposes (e.g. printing the AST)
// this should be fine
void String_Builder::append(const char *s, ...) {
    char buf[8192]; // @Overflow
    va_list args;

    va_start(args, s);
    size_t len = vsprintf(buf, s, args);
    va_end(args);

    // @note: +1 for \0
    buffer.maygrow(len+1);
    //strcpy_s(buffer.a+buffer.len, len, buf);
    strcpy(buffer.a+buffer.len, buf);
    buffer.len += len;
}

bool fits_into(int64 v, uint8 bits) {
    uint8 clear_bits = (sizeof(v) * 8) - bits;
    int64 b = (v << clear_bits) >> clear_bits;
    return v == b;
}
