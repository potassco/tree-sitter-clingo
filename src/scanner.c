#include "tree_sitter/parser.h"

enum TokenType {
  EMPTY_POOL_ITEM_FIRST,
  EMPTY_POOL_ITEM,
  COLON,
  BLOCK_COMMENT,
  DOC_STRING_FRAGMENT,
  DOC_ARGS,
  DOC_LPAREN,
  DOC_MINUS,
};

static void skip_whitespace(TSLexer *lexer, bool skip) {
  while (lexer->lookahead == ' ' ||
         ('\t' <= lexer->lookahead && lexer->lookahead <= '\r')) {
    lexer->advance(lexer, skip);
  }
}

static bool match_string(TSLexer *lexer, const char *literal) {
  const char *p = literal;
  for (; *p; ++p) {
    if (lexer->lookahead != *p) {
      return false;
    }
    lexer->advance(lexer, false);
  }
  return true;
}

/**
 * Handles parsing of documentation comments and their fragments.
 *
 * This function attempts to match specific documentation-related tokens:
 * - DOC_LPAREN: Matches '(' if active.
 * - DOC_MINUS: Matches '-' if active.
 * - DOC_ARGS: Matches "Args:" at the start of a fragment if active.
 * - DOC_STRING_FRAGMENT: Consumes a fragment of the docstring, stopping at
 *   '-', markdown characters (`*`, '`', `_`), or "Args:".
 *
 * Returns true if a valid documentation token is found and sets
 * lexer->result_symbol accordingly.
 */
static bool doc_comment(TSLexer *lexer, const bool *valid_symbols) {
  if (lexer->lookahead == 0) {
    return false;
  }
  // prefer `(` if active
  if (valid_symbols[DOC_LPAREN]) {
    if (lexer->lookahead == '(') {
      lexer->advance(lexer, false);
      lexer->result_symbol = DOC_LPAREN;
      return true;
    }
  }
  // prefer `-` if active
  if (valid_symbols[DOC_MINUS] && lexer->lookahead == '-') {
    lexer->advance(lexer, false);
    lexer->result_symbol = DOC_MINUS;
    return true;
  }
  bool empty = true;
  if (valid_symbols[DOC_STRING_FRAGMENT]) {
    while (true) {
      if (lexer->lookahead == 0) {
        return false;
      }
      // try to match `Args:` if active; continue if it did not match
      if (valid_symbols[DOC_ARGS] && lexer->lookahead == 'A') {
        lexer->mark_end(lexer);
        if (match_string(lexer, "Args:")) {
          if (empty) {
            // we just matched `Args:` at the start of the token
            lexer->mark_end(lexer);
            lexer->result_symbol = DOC_ARGS;
            return true;
          } else {
            // we matched `Args:` but not at the start, so stop here
            break;
          }
        } else {
          empty = false;
          continue;
        }
      }
      // stop at `-` (for arguments) if active
      if (valid_symbols[DOC_MINUS] && lexer->lookahead == '-') {
        lexer->mark_end(lexer);
        break;
      }
      // stop at nested markdown
      if (lexer->lookahead == '`' || lexer->lookahead == '*' ||
          lexer->lookahead == '_') {
        lexer->mark_end(lexer);
        break;
      }
      empty = false;
      lexer->advance(lexer, false);
    }
    lexer->result_symbol = DOC_STRING_FRAGMENT;
  }
  return !empty;
}

static bool consume_blockcomment(TSLexer *lexer) {
  // Consume all characters while counting `%*` and `*%` occurrences.
  // Stop when the count hits zero.
  int open = 1;
  while (open > 0) {
    if (lexer->lookahead == 0) {
      return false;
    }
    if (lexer->lookahead == '%') {
      lexer->advance(lexer, false);
      if (lexer->lookahead == 0) {
        return false;
      }
      if (lexer->lookahead == '*') {
        lexer->advance(lexer, false);
        ++open;
      }
      continue;
    }
    if (lexer->lookahead == '*') {
      lexer->advance(lexer, false);
      if (lexer->lookahead == 0) {
        return false;
      }
      if (lexer->lookahead == '%') {
        lexer->advance(lexer, false);
        --open;
      }
      continue;
    }
    lexer->advance(lexer, false);
  }
  lexer->result_symbol = BLOCK_COMMENT;
  lexer->mark_end(lexer);
  return true;
}

bool tree_sitter_clingo_external_scanner_scan(void *payload, TSLexer *lexer,
                                              const bool *valid_symbols) {
  if (valid_symbols[DOC_ARGS] || valid_symbols[DOC_LPAREN] ||
      valid_symbols[DOC_MINUS] || valid_symbols[DOC_STRING_FRAGMENT]) {
    return doc_comment(lexer, valid_symbols);
  }

  skip_whitespace(lexer, true);

  if (valid_symbols[BLOCK_COMMENT] && lexer->lookahead == '%') {
    lexer->advance(lexer, false);
    if (lexer->lookahead == '*') {
      lexer->advance(lexer, false);
      if (lexer->lookahead != '!') {
        return consume_blockcomment(lexer);
      }
    }
    return false;
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
