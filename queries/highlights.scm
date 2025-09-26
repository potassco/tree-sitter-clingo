;; we conform to the neovim highlighting captures
;; https://neovim.io/doc/user/treesitter.html#_treesitter-syntax-highlighting
;; which is a superset/extension of the standard tree-sitter highlighting captures
;; https://github.com/tree-sitter/tree-sitter/blob/34ef1157a65987d53a18a32dbdd04046c263f7e5/crates/highlight/src/highlight.rs#L29

;;
;; keywords
;;
[
 "#minimize"
 "#minimise"
 "#maximize"
 "#maximise"
 "#edge"
 "#heuristic"
 "#project"
 "#show"
 "#defined"
 "#external"
] @keyword.directive

[
 "#include"
] @keyword.import

[
 (default_negation)
 (double_default_negation)
 (aggregate_function)
] @keyword.operator

[
 "#const"
] @keyword.directive.define

[
 "#script"
 "#end"
 "#theory"
 "#program"
 ] @keyword

(theory 
  name: (identifier) @keyword)

(theory_atom_definition "&" @keyword)
(theory_atom_definition
  name: (identifier) @keyword)

(theory_atom "&" @keyword)
(theory_atom 
  name: (identifier) @keyword)

(script language: (identifier) @keyword)

[
 (theory_atom_type)
 (const_type)
 (theory_operator_arity)
 (theory_operator_associativity)
] @keyword

;;
;; comments
;;
(line_comment) @comment
(block_comment) @comment

;;
;; punctuation
;;
[
 ";"
 ","
 "."
 ":-"
 ":~"
 ":"
 (lone_comma)
] @punctuation.delimiter
(disjunction "|" @punctuation.delimiter)
(weight "@" @punctuation.delimiter)
(signature "/" @punctuation.delimiter)
(theory_atom_definition "/" @punctuation.delimiter)

["(" ")" "{" "}" "[" "]"] @punctuation.bracket

;;
;; operators
;;
[
 ".."
 "^"
 "?"
 "+"
 "-"
 "*"
 "\\"
 "**"
 "~"
 (theory_operator)
 (relation)
] @operator
(abs "|" @operator)
(binary_operation "/" @operator)
(binary_operation "&" @operator)
(unary_operation "-" @operator)

;;
;; other terms
;;
(number) @number
(escape_sequence) @string.escape
(string) @string
(fstring) @string

(supremum) @constant.builtin
(infimum) @constant.builtin
;; We highlight all constants (non-external function symbols with 0
;; arity) as preprocessor-defined constants, since similarly to a
;; macro, they are replaced by the given value when there is a #const
;; statement or -c flag or if they are a parameter of a #program
;; statement
(const
  name: (identifier) @constant.macro
  "=" @operator)
(program
  parameters: (parameters (identifier) @constant.macro))
(function
  name: (identifier) @constant.macro
  !arguments)
(theory_function 
  name: (identifier) @constant.macro
  !arguments)
;; External functions are highlighted as preprocessor macros, since
;; similarly to macros, they are replaced during grounding with the
;; value computed by an external script. function.macro would be a
;; better fit, but functions are reserved for highlighting atoms.
(external_function
  "@" @constant.macro
  name: (identifier) @constant.macro)
;; we highlight non-zero arity, non external function symbols as data
;; constructors (as in Haskell). The @function capture is instead
;; used for atoms
(function
  name: (identifier) @constructor
  arguments: (_))
(theory_function 
  name: (identifier) @constructor
  arguments: (_))

;;
;; variables
;;
(variable) @variable
(anonymous) @variable.builtin

;;
;; symbolic atoms
;;
;; We highlight symbolic atoms as functions, since @function is
;; usually used to highlight the fundamental building blocks of a
;; language in tree-sitter, and as this allows us to distinguish
;; between definitions (atoms in the head) and references (atoms
;; occurring elsewhere), highlighting them as @function and
;; @function.call, respectively.
;;
;; atom definitions (@function)
(rule
  head: (literal
    !sign
    atom: (symbolic_atom
	   ((classical_negation)? (identifier)) @function)))

(disjunction
  (literal
    !sign
    atom: (symbolic_atom
	   ((classical_negation)? (identifier)) @function)))


(disjunction
  (conditional_literal
    literal: (literal
      !sign
      atom: (symbolic_atom
	   ((classical_negation)? (identifier)) @function))))


(head_aggregate_element
  literal: (literal
    !sign
    atom: (symbolic_atom
	   ((classical_negation)? (identifier)) @function)))


(rule
  head: (set_aggregate
    elements: (set_aggregate_elements
      (set_aggregate_element
        literal: (literal
          !sign
	  atom: (symbolic_atom
	   ((classical_negation)? (identifier)) @function))))))

(external
  atom: (symbolic_atom
	   ((classical_negation)? (identifier)) @function))

;; atom references (@function.call)
(rule
  head: (literal
    sign: (_)
    atom: (symbolic_atom
	   ((classical_negation)? (identifier)) @function.call)))

(disjunction
  (literal
    sign: (_)
    atom: (symbolic_atom
	   ((classical_negation)? (identifier)) @function.call)))

(disjunction
  (conditional_literal
    literal: (literal
      sign: (_)
      atom: (symbolic_atom
	   ((classical_negation)? (identifier)) @function.call))))

(head_aggregate_element
  literal: (literal
    sign: (_)
    atom: (symbolic_atom
	   ((classical_negation)? (identifier)) @function.call)))

(rule
  head: (set_aggregate
    elements: (set_aggregate_elements
      (set_aggregate_element
        literal: (literal
          sign: (_)
	  atom: (symbolic_atom
	   ((classical_negation)? (identifier)) @function.call))))))

(body_literal
  atom: (symbolic_atom
	   ((classical_negation)? (identifier)) @function.call))
(body
  (conditional_literal
    literal: (literal
      atom: (symbolic_atom
	   ((classical_negation)? (identifier)) @function.call))))
(body_literal
  atom: (set_aggregate
    elements: (set_aggregate_elements
      (set_aggregate_element
        literal: (literal
	  atom: (symbolic_atom
	   ((classical_negation)? (identifier)) @function.call))))))
(condition
  (literal
    atom: (symbolic_atom
	   ((classical_negation)? (identifier)) @function.call)))
(project_atom
  atom: (symbolic_atom
	   ((classical_negation)? (identifier)) @function.call))
(heuristic
  atom: (symbolic_atom
	   ((classical_negation)? (identifier)) @function.call))

(signature
  sign: (classical_negation)? @function.call
  name: (identifier) @function.call) 

;; boolean constant
(boolean_constant) @boolean

;;
;; types
;;
(theory_atom_definition
  theory_term_name: (identifier) @type
  guard: (identifier)? @type)

(theory_term_definition
  name: (identifier) @type)

;;
;; modules
;;
(include
  (identifier) @module)
(program
  name: (identifier) @module)
