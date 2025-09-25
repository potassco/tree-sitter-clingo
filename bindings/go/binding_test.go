package tree_sitter_clingo_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_clingo "github.com/potassco/tree-sitter-clingo/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_clingo.Language())
	if language == nil {
		t.Errorf("Error loading Clingo grammar")
	}
}
