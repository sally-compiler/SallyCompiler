#include "tokenizer.h"

void Tokenizer::load_source_file(const char *filename) {
    src_filename = filename;
    FILE *source = fopen(filename, "rb");

    if (!source) {
        report_error("error opening file %s", filename);
    }

    fseek(source, 0, SEEK_END);
    size_t length = ftell(source);
    rewind(source);

    char *buffer = new char[length + 1]; // one more for terminator \0
    if (buffer) {
        if (fread(buffer, 1, length, source) == 0) {
            report_error("error reading the source file '%s'.", filename);
        }
        buffer[length] = '\0';
    } else {
        report_error("cannot create buffer when opening %s", filename);
    }

    fclose(source);

    expr = buffer;
}

void Tokenizer::scan() {
    cur = 0;
    tokens_buffer.set_cap(64);

    // scan all the tokens, and store them into tokens_buffer
    while (scan_next_token())
        ;

    token_index = 0;
}

void Tokenizer::eat_multiline_comments() {

    if (expr[cur] == '/' && expr[cur + 1] == '*') {
        next_char();
        while (1) {
            next_char();
            if (expr[cur] == '*' && expr[cur + 1] == '/') {
                next_char();
                next_char();
                break;
            }
        }
    }
}

bool32 Tokenizer::scan_next_token() {

    Token t;

    // ignore spaces and comments
    int last_cur;
    do {
        last_cur = cur;
        // ignore spaces
        while (isspace(expr[cur]) && expr[cur] != '\0')
            next_char();

        // inline comments
        if (expr[cur] == '/' && expr[cur + 1] == '/') {
            while (expr[cur] != '\n')
                next_char();
        }

        // multi-line comments
        eat_multiline_comments();

    } while (last_cur != cur); // advance cur until cur doesn't advance

    // after skipping comments and spaces
    // we arrive at the true thing we care
    t.l = l;
    t.c = c;

    // @TODO: after we have enough flags we can pack it into an integer.
    t.is_op = false;
    t.is_keyword = false;

    char c = expr[cur];
    if (isdigit(c)) {

        t.type = TOKEN_INTEGER;
        sscanf(expr + cur, "%" PRIi64, &t.data.int_val);
        // eats 0x before hex number
        if (expr[cur + 1] == 'x' || expr[cur + 1] == 'X') {
            next_char();
            next_char();
        }
        while (isdigit(expr[cur]) || (expr[cur] >= 'a' && expr[cur] <= 'f') ||
               (expr[cur] >= 'A' && expr[cur] <= 'F'))
            next_char();

    } else if (c == '\0') {
        t.type = TOKEN_END; // don't advance to next char, stays at TOKEN_END
        tokens_buffer.push(t);
        return false; // end the scanning loop
    } else if (isalpha(c) || c == '_') {
        // might be a keyword or an identifier
        char *string = read_string();

        if (strcmp(string, "if") == 0) {
            t.type = TOKEN_KEYWORD_IF;
        } else if (strcmp(string, "else") == 0) {
            t.type = TOKEN_KEYWORD_ELSE;
        } else if (strcmp(string, "while") == 0) {
            t.type = TOKEN_KEYWORD_WHILE;
        } else if (strcmp(string, "int") == 0) {
            t.type = TOKEN_KEYWORD_INT;
        } else if (strcmp(string, "return") == 0) {
            t.type = TOKEN_KEYWORD_RETURN;
        } else if (strcmp(string, "continue") == 0) {
            t.type = TOKEN_KEYWORD_CONTINUE;
        } else if (strcmp(string, "break") == 0) {
            t.type = TOKEN_KEYWORD_BREAK;
        } else if (strcmp(string, "void") == 0) {
            t.type = TOKEN_KEYWORD_VOID;
        } else if (strcmp(string, "const") == 0) {
            t.type = TOKEN_KEYWORD_CONST;
        } else { // it's a identifier
            t.type = TOKEN_IDENTIFIER;
            t.data.identifier = string;
        }

        if (t.type != TOKEN_IDENTIFIER) {
            t.is_keyword = true;
        }

    } else {
        // deals with operators and other single characters like semicolon and
        // comma
        Token_Type op_type = read_op();

        if (op_type) { // operator
            t.is_op = true;
            t.type = op_type;
        } else { // single character
            t.type = (Token_Type)c;
            next_char();
        }
    }

    // copy the token struct into the array
    tokens_buffer.push(t);
    return true;
}

void Tokenizer::rewind_to_first_token() { token_index = 0; }

Token *Tokenizer::token() { return &tokens_buffer[token_index]; }

Token *Tokenizer::peek(int lookahead) {
    int index = token_index + lookahead;
    if (index > tokens_buffer.len - 1) {
        return &tokens_buffer[tokens_buffer.len - 1]; // TOKEN_END!
    } else {
        return &tokens_buffer[index];
    }
}

void Tokenizer::next_char() {
    c++;
    if (expr[cur] == '\n') {
        l++;
        c = 0;
    }
    cur++;
}

void Tokenizer::expect(Token_Type token_type) {
    if (token()->type != token_type) {
        report_error("Line %d - expected %s, got %s", token()->l,
                     token_type_to_desc(token_type), token_to_desc(token()));
    }
}

void Tokenizer::expect(char token_type) { expect((Token_Type)token_type); }

void Tokenizer::eat() { token_index++; }

void Tokenizer::expect_and_eat(char token_type) {
    expect(token_type);
    eat();
}

void Tokenizer::expect_and_eat(Token_Type token_type) {
    expect(token_type);
    eat();
}

// take the text stream to see if there are any operators
// returns 0 if no operator is found.
Token_Type Tokenizer::read_op() {

    char *cur_expr = expr + cur;
    if (strncmp("<=", cur_expr, 2) == 0) {
        next_char();
        next_char();
        return TOKEN_LESS_THAN_OR_EQUAL_TO;
    } else if (strncmp(">=", cur_expr, 2) == 0) {
        next_char();
        next_char();
        return TOKEN_GREATER_THAN_OR_EQUAL_TO;
    } else if (strncmp("!=", cur_expr, 2) == 0) {
        next_char();
        next_char();
        return TOKEN_NOT_EQUAL_TO;
    } else if (strncmp("==", cur_expr, 2) == 0) {
        next_char();
        next_char();
        return TOKEN_EQUAL_TO;
    } else if (strncmp("&&", cur_expr, 2) == 0) {
        next_char();
        next_char();
        return TOKEN_LOGICAL_AND;
    } else if (strncmp("||", cur_expr, 2) == 0) {
        next_char();
        next_char();
        return TOKEN_LOGICAL_OR;
    } else if (expr[cur] == '!') {
        next_char();
        return (Token_Type)'!';
    } else if (expr[cur] == '<') {
        next_char();
        return TOKEN_LESS_THAN;
    } else if (expr[cur] == '>') {
        next_char();
        return TOKEN_GREATER_THAN;
    } else if (expr[cur] == '+') {
        next_char();
        return (Token_Type)'+';
    } else if (expr[cur] == '-') {
        next_char();
        return (Token_Type)'-';
    } else if (expr[cur] == '*') {
        next_char();
        return (Token_Type)'*';
    } else if (expr[cur] == '/') {
        next_char();
        return (Token_Type)'/';
    } else if (expr[cur] == '%') {
        next_char();
        return (Token_Type)'%';
    } else {
        return (Token_Type)0;
    }
}

char *Tokenizer::read_string() {
    int cap = 32;
    char *string = new char[cap];
    int string_index = 0;

    // Identifiers and keywords can only starts with alphabets
    // and follows only alphabets and digits.
    while (isalpha(expr[cur]) || isdigit(expr[cur]) || expr[cur] == '_') {
        string[string_index] = expr[cur];
        string_index++;
        if (string_index == cap - 1) {
            cap *= 2;
            char *new_string = new char[cap];
            strncpy(new_string, string, string_index);
            string = new_string;
        }
        next_char();
    }
    string[string_index] = '\0';
    return string;
}

// returns a textual description of token, to help with error messages
// e.g. for TOKEN_LESS_THAN, it returns <
// for TOKEN_IDENTIFIER, it returns "identifier 'name'"
const char *token_type_to_desc(Token_Type token_type) {

    char *desc = new char[1024];

    // for ASCII characters, just return the character it correspond to
    if (1 <= token_type && token_type <= 127) {
        desc[0] = token_type;
        desc[1] = '\0';
        return desc;
    }

    switch (token_type) {

    case TOKEN_INTEGER: {
        return "intger litreal";
    } break;
    case TOKEN_IDENTIFIER: {
        return "identifier";
    } break;
    case TOKEN_END: {
        return "EOF";
    } break;
    case TOKEN_NOT_EQUAL_TO: {
        return "!=";
    } break;
    case TOKEN_EQUAL_TO: {
        return "==";
    } break;
    case TOKEN_LESS_THAN_OR_EQUAL_TO: {
        return "<=";
    } break;
    case TOKEN_GREATER_THAN_OR_EQUAL_TO: {
        return ">=";
    } break;
    case TOKEN_LESS_THAN: {
        return "<=";
    } break;
    case TOKEN_GREATER_THAN: {
        return ">=";
    } break;
    case TOKEN_LOGICAL_AND: {
        return "&&";
    } break;
    case TOKEN_LOGICAL_OR: {
        return "||";
    } break;
    case TOKEN_KEYWORD_IF: {
        return "if";
    } break;
    case TOKEN_KEYWORD_ELSE: {
        return "else";
    } break;
    case TOKEN_KEYWORD_INT: {
        return "int";
    } break;
    case TOKEN_KEYWORD_RETURN: {
        return "return";
    } break;
    case TOKEN_KEYWORD_CONTINUE: {
        return "continue";
    } break;
    case TOKEN_KEYWORD_BREAK: {
        return "break";
    } break;
    case TOKEN_KEYWORD_VOID: {
        return "void";
    } break;

    default: {
        return "(no text desc)";
    } break;
    }
}

const char *token_to_desc(Token *token) {
    const char *desc = token_type_to_desc(token->type);
    if (!desc) {
        char *other_desc = new char[1024];
        switch (token->type) {
        case TOKEN_IDENTIFIER: {
            sprintf(other_desc, "identifier '%s'", token->data.identifier);
        } break;
        case TOKEN_INTEGER: {
            sprintf(other_desc, "integer %" PRId64, token->data.int_val);
        } break;
        default: {
            other_desc = (char *)"(unknown)";
        } break;
        }
        return other_desc;
    }
    return desc;
};

void report_error(Token *token, const char *format, ...) {
    va_list args;
    va_start(args, format);

    printf("Line %d: ", token->l);
    vprintf(format, args);
    printf("\n");

    va_end(args);
    assert(false);
    exit(0);
}
