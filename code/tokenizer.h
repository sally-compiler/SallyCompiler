#ifndef TOKENIZER_H
#define TOKENIZER_H

#include "array.h"
#include "general.h"

#include <cinttypes>
#include <ctype.h>
#include <string.h> // strcmp, strncmp

enum Token_Type {

    // imagine here is an ASCII table
    TOKEN_END = 0,
    // end of ASCII table

    TOKEN_IDENTIFIER = 128,
    TOKEN_INTEGER,

    TOKEN_NOT_EQUAL_TO,
    TOKEN_EQUAL_TO,
    TOKEN_LESS_THAN_OR_EQUAL_TO,
    TOKEN_GREATER_THAN_OR_EQUAL_TO,
    TOKEN_LESS_THAN,
    TOKEN_GREATER_THAN,

    TOKEN_KEYWORD_IF,
    TOKEN_KEYWORD_ELSE,

    TOKEN_KEYWORD_WHILE,

    TOKEN_KEYWORD_INT,
    TOKEN_KEYWORD_RETURN,
    TOKEN_KEYWORD_CONTINUE,
    TOKEN_KEYWORD_BREAK,
    TOKEN_KEYWORD_VOID,
    TOKEN_KEYWORD_CONST,

    TOKEN_LOGICAL_AND,
    TOKEN_LOGICAL_OR
};

struct Token {
    Token_Type type;
    int l, c; // line and column
    bool32 is_op;
    bool32 is_keyword;
    union {
        int64 int_val; // INT_MIN....  -2147483648
        char *identifier;
    } data;
};

const char *token_type_to_desc(Token_Type token_type);
const char *token_to_desc(Token *token);

struct Tokenizer {
    char *expr;
    size_t cur;
    Array<Token> tokens_buffer;
    const char *src_filename;
    int token_index;

    int l = 1, c = 0;

    void load_source_file(const char *filename);
    void scan();
    void eat_multiline_comments();
    bool32 scan_next_token();
    void rewind_to_first_token();
    Token *token();
    Token *peek(int lookahead = 1);
    void next_char();
    void expect(Token_Type token_type);
    void expect(char token_type);
    void eat();
    void expect_and_eat(char token_type);
    void expect_and_eat(Token_Type token_type);
    Token_Type read_op();
    char *read_string();
};

void report_error(Token *token, const char *format, ...);

#endif
