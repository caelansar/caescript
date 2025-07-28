package tree_sitter_caescript_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_caescript "github.com/tree-sitter/tree-sitter-caescript/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_caescript.Language())
	if language == nil {
		t.Errorf("Error loading Caescript grammar")
	}
}
