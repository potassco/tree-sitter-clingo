#include "tree_sitter/parser.h"

enum TokenType { _EMPTY_TERMS };

bool tree_sitter_clingo_external_scanner_scan(void *payload, TSLexer *lexer,
                                             const bool *valid_symbols) {
  if (valid_symbols[_EMPTY_TERMS]) {
	lexer->mark_end(lexer);
        if (lexer->lookahead == ';' || lexer->lookahead == ')' ||
	    lexer->lookahead == ']' || lexer->lookahead == '}') {
	    lexer->result_symbol = _EMPTY_TERMS;
	    return true;
	} else {
	    return false;
	};
    }
    return false;
}

// If we need to allocate/deallocate state, we do it in these functions.
void *tree_sitter_clingo_external_scanner_create() { return NULL; }

void tree_sitter_clingo_external_scanner_destroy(void *payload) {}

// If we have state, we should load and save it in these functions.
unsigned tree_sitter_clingo_external_scanner_serialize(void *payload,
                                                      char *buffer) {
    return 0;
}

void tree_sitter_clingo_external_scanner_deserialize(void *payload, char *buffer,
                                                    unsigned length) {}

