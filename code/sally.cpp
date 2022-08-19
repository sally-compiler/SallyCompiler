#include "arm.h"
#include "asm/asm_passes.h"
#include "general.h"
#include "ir.h"
#include "opt/opt.h"
#include "parser.h"
#include "tokenizer.h"

#include "cfg_viewer.h"

#include <stdio.h>
#include <string.h> // strcmp for matching command line arguments

void print_help(char *program) {
    printf("Usage: %s", program);
    printf(" [ -h | --help ] [ -O2 ] [ -o <output-asm = build\\test.s> ] "
           "<input-file = code\\first.sy>\n");
}

std::string input_command;

bool match(char *a, const char *b) { return strcmp(a, b) == 0; }

int main(int argc, char **argv) {

    const char *source_file = "code/first.sy";
    const char *output_file = "build/test.s";
    bool enable_optimization = false;

    for (int i = 1; i < argc; ++i) {
        if (match(argv[i], "-h") || match(argv[i], "--help")) {
            print_help(argv[0]);
            return 0;
        } else if (match(argv[i], "-o")) {
            if (i + 1 < argc) {
                output_file = argv[i + 1];
                i += 1;
            } else {
                print_help(argv[0]);
                return 0;
            }
        } else if (match(argv[i], "-O2") || match(argv[i], "-O1") ||
                   match(argv[i], "-O")) {
            enable_optimization = true;
        } else if (match(argv[i], "-S")) {
        } else {
            source_file = argv[i];
        }
    }

    printf(">>> Parsing...");
    Parser parser;
    Program_AST *program_ast = parser.parse(source_file);
    printf(" OK.\n");

    printf(">>> Converting to IR...\n\n");
    auto program_IR = emit_IR(program_ast);

    printf(">>> Un-optimized IR:\n\n");
    std::string ir = print_procedure_IR(program_IR->procedures[0]);
    printf("%s", ir.c_str());

    if (enable_optimization) {
        printf(">>> Optimizing IR...\n\n");
        perform_magic(program_IR);
        printf("\n");

        printf(">>> Optimized IR:\n\n");
        ir = print_procedure_IR(program_IR->procedures[0]);
        printf("%s", ir.c_str());
    }

    // print_use_list(program_IR->procedures[0]);

#ifdef ENABLE_CFG_VIEWER
    printf(">>> Drawing IR CFG... ");
    make_cfg(program_IR->procedures[0]->blocks);
    printf("saved to build/ir.pdf.\n\n");
#endif

    printf(">>> Emitting primitive assembly...\n\n");
    Program_Asm *program_asm = emit_asm(program_IR, enable_optimization);

    String_Builder s_p;
    build_program_asm(&s_p, program_asm, program_ast->globals);
    s_p.add_terminator();
    printf("%s\n", s_p.c_str());

    printf(">>> Optimizing assembly...\n\n");
    bless(program_asm, enable_optimization);
    printf("\n");

    String_Builder s;
    build_program_asm(&s, program_asm, program_ast->globals);
    s.add_terminator();
    printf("%s", s.c_str());

    FILE *assembly_file = fopen(output_file, "w");
    if (assembly_file == NULL) {
        assert(false && "error opening assembly output file");
    }

    fprintf(assembly_file, "%s", s.c_str());
    fclose(assembly_file);

    return 0;
}
