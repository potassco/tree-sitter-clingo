#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "tree_sitter/parser.h"

enum TokenType {
  EMPTY_POOL_ITEM_FIRST,
  EMPTY_POOL_ITEM,
  COLON,
  BLOCK_COMMENT,
  DOC_DESC,
  DOC_ARGS,
  DOC_LPAREN,
  DOC_MINUS,
};

typedef struct clingo_lexer_state {
  bool enable_doc_minus;
} clingo_lexer_state_t;

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
 * - DOC_DESC: Matches a description stopping at '-' or "Args:".
 *
 * Returns true if a valid documentation token is found and sets
 * lexer->result_symbol accordingly.
 */
static bool doc_comment(clingo_lexer_state_t *state, TSLexer *lexer,
                        const bool *valid_symbols) {
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
  if (valid_symbols[DOC_MINUS] && state->enable_doc_minus &&
      lexer->lookahead == '-') {
    lexer->advance(lexer, false);
    lexer->result_symbol = DOC_MINUS;
    return true;
  }
  bool empty = true;
  if (valid_symbols[DOC_DESC]) {
    while (true) {
      if (lexer->lookahead == 0) {
        return false;
      }
      // permit '-' at the start of a line
      if (lexer->lookahead == '\n') {
        state->enable_doc_minus = true;
      } else if (lexer->lookahead != ' ' && lexer->lookahead != '\t' &&
                 lexer->lookahead != '\r' && lexer->lookahead != '-') {
        state->enable_doc_minus = false;
      }

      // try to match `Args:` if active; continue if it did not match
      if (valid_symbols[DOC_ARGS] && lexer->lookahead == 'A') {
        lexer->mark_end(lexer);
        if (match_string(lexer, "Args:")) {
          if (empty) {
            // we just matched `Args:` at the start of the token
            lexer->mark_end(lexer);
            lexer->result_symbol = DOC_ARGS;
            // permit '-' after 'Args:'
            state->enable_doc_minus = true;
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
      // we can gobble the minus if we have not seen a newline
      if (valid_symbols[DOC_MINUS] && state->enable_doc_minus &&
          lexer->lookahead == '-') {
        lexer->mark_end(lexer);
        break;
      }
      // stop at a closing block comment
      if (lexer->lookahead == '*') {
        lexer->mark_end(lexer);
        lexer->advance(lexer, false);
        // stop at closing block comment
        if (lexer->lookahead == '%') {
          break;
        }
        empty = false;
        continue;
      }
      empty = false;
      lexer->advance(lexer, false);
    }
    lexer->result_symbol = DOC_DESC;
  } else if (valid_symbols[DOC_ARGS] && match_string(lexer, "Args:")) {
    lexer->mark_end(lexer);
    lexer->result_symbol = DOC_ARGS;
    state->enable_doc_minus = true;
    return true;
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
      valid_symbols[DOC_MINUS] || valid_symbols[DOC_DESC]) {
    return doc_comment(payload, lexer, valid_symbols);
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

void *tree_sitter_clingo_external_scanner_create() {
  clingo_lexer_state_t *s = malloc(sizeof(clingo_lexer_state_t));
  memset(s, 0, sizeof(*s));
  return s;
}

void tree_sitter_clingo_external_scanner_destroy(void *payload) {
  free(payload);
}

unsigned tree_sitter_clingo_external_scanner_serialize(void *payload,
                                                       char *buffer) {
  assert(payload != NULL);
  size_t n = sizeof(clingo_lexer_state_t);
  assert(n <= TREE_SITTER_SERIALIZATION_BUFFER_SIZE);
  memcpy(buffer, payload, n);
  return n;
}

void tree_sitter_clingo_external_scanner_deserialize(void *payload,
                                                     char *buffer,
                                                     unsigned length) {
  assert(payload != NULL);
  size_t n = sizeof(clingo_lexer_state_t);
  if (n == length) {
    assert(buffer != NULL);
    memcpy(payload, buffer, n);
  } else {
    memset(payload, 0, n);
  }
}
