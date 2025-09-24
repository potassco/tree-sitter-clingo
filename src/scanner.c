#include "tree_sitter/parser.h"

enum TokenType { EMPTY_POOL_ITEM_FIRST, EMPTY_POOL_ITEM, COLON };

bool tree_sitter_clingo_external_scanner_scan(void *payload, TSLexer *lexer,
                                              const bool *valid_symbols) {
  // skip whitespace (tree-sitter quirk)
  while (lexer->lookahead == ' ' ||
         ('\t' <= lexer->lookahead && lexer->lookahead <= '\r')) {
    lexer->advance(lexer, true);
  }

  if (valid_symbols[COLON] && lexer->lookahead == ':') {
    lexer->advance(lexer, false);
    if (lexer->lookahead != '-') {
      lexer->result_symbol = COLON;
      return true;
    }
    return false;
  }

  if (valid_symbols[EMPTY_POOL_ITEM_FIRST]) {
    if (lexer->lookahead == ';') {
      lexer->result_symbol = EMPTY_POOL_ITEM_FIRST;
      return true;
    }
    return false;
  }

  if (valid_symbols[EMPTY_POOL_ITEM]) {
    if (lexer->lookahead == ';' || lexer->lookahead == ')') {
      lexer->result_symbol = EMPTY_POOL_ITEM;
      return true;
    }
    return false;
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

void tree_sitter_clingo_external_scanner_deserialize(void *payload,
                                                     char *buffer,
                                                     unsigned length) {}
