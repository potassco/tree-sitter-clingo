; comments
(line_comment) @comment
(block_comment) @comment

; identifiers
(identifier) @identifier

; constants
(number) @number
(string) @string
(supremum) @constant
(infimum) @constant

; terms
(variable) @parameter
(anonymous) @parameter

; meta directives
["#include"] @include
["#const"] @define
["#defined" ] @preproc
["#theory" ] @preproc
(show "#show" @preproc)
(show_signature "#show" @preproc)
(project_signature "#project" @preproc)
["#script" "#end"] @preproc

; delimiters
[":-" ":~" ";" "," "." "@" ":"] @delimiter

; punctuation
["(" ")" "{" "}" "[" "}"] @punctuation.bracket

; operators
(relation) @operator
["+" "/" "-" "*" "\\" "**" ".." "?" "~" "&" "not" "=" (operator)] @operator

; atoms and some terms
(symbolic_atom (identifier) @function)
(symbolic_atom (negative_identifier) @function)
(function (identifier) @constant) 
(external_function ["@"] @function.builtin (identifier) @function.builtin)

; keywords
(aggregate_function) @keyword
(theory_atom_definition
    ["&"]@keyword
    name: (_)@keyword)
(theory_atom
    ["&"]@keyword
    (theory_atom_name)@keyword)
["#true" "#false" "#external" "#minimize" "#minimise" "#maximize" "#maximise" "#edge" "#heuristic"] @keyword
(show_term "#show" @keyword)
(project_atom "#project" @keyword)

