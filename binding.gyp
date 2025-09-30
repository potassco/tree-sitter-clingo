{
  "targets": [
    {
      "target_name": "generate_parser",
      "type": "none",
      "actions": [
        {
          "action_name": "tree_sitter_generate",
          "inputs": [ "grammar.js" ],
          "outputs": [ "src/parser.c", "src/tree_sitter/parser.h", "src/tree_sitter/alloc.h", "src/tree_sitter/array.h" ],
          "action": [ "tree-sitter", "generate" ]
        }
      ]
    },
    {
      "target_name": "tree_sitter_clingo_binding",
      "dependencies": [
        "generate_parser",
        "<!(node -p \"require('node-addon-api').targets\"):node_addon_api_except",
      ],
      "include_dirs": [
        "src",
      ],
      "sources": [
        "bindings/node/binding.cc",
        "src/parser.c",
        "src/scanner.c"
      ],
      "conditions": [
        ["OS!='win'", {
          "cflags_c": [
            "-std=c11",
          ],
        }, { # OS == "win"
          "cflags_c": [
            "/std:c11",
            "/utf-8",
          ],
        }],
      ],
    }
  ]
}
