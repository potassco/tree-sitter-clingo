;; comments
(line_comment) @comment
(block_comment) @comment

;; constants
(number) @number
(string) @string
(const
 name: (identifier) @constant)
(function
 name: (identifier) @constant
 !arguments)
(program
 identifiers: (identifiers (identifier) @constant))

(supremum) @constant.builtin
(infimum) @constant.builtin

(function
 name: (identifier) @function
 arguments: (_))
(external_function "@" @function (identifier) @function)
(theory_function (identifier) @function)

(variable) @variable
(anonymous) @variable.builtin

;; delimiters
[":-" ":~" ";" "," "." "@" ":"] @delimiter
(disjunction
 "|" @delimiter)
(signature
 "/" @delimiter)

;; punctuation
["(" ")" "{" "}" "[" "]"] @punctuation.bracket

;; operators
[
 ".."
 "^"
 "?"
 "&"
 "+"
 "-"
 "*"
 "/"
 "\\"
 "**"
 "|"
 "~"
 (theory_operator)
 (relation)
 (default_negation)
 (double_default_negation)
 (classical_negation)
 ] @operator

;; atoms
(symbolic_atom (identifier) @constructor)
(signature
 name: (identifier) @constructor)

;; keywords
(theory_atom_definition
    "&" @keyword
    name: (identifier) @keyword)
(theory_atom
    "&" @keyword
    (identifier) @keyword)
(script
 language: (identifier) @keyword)
[
 "#external"
 "#minimize"
 "#minimise"
 "#maximize"
 "#maximise"
 "#edge"
 "#heuristic"
 "#script"
 "#end"
 "#show"
 "#project"
 "#theory"
 "#defined"
 "#const"
 "#include"
 (aggregate_function)
 "#true"
 "#false"
 ] @keyword

;; type
(theory_atom_definition
 theory_term_name: (identifier) @type
 guard: (identifier) @type)

(theory_term_definition
 name: (identifier) @type)

(heuristic
 type: (_) @type)

(external
 type: (_) @type)

[
 (theory_atom_type)
 (const_type)
 (theory_operator_arity)
 (theory_operator_associativity)
] @type.builtin


;; module
(include
 (identifier) @module)
(program
 name: (identifier) @module)

;; embedded
(script
 code: (code) @embedded)

