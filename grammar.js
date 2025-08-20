const binary_expression = function (pre, lhs, op, rhs) {
    if (pre < 0) {
        return prec.right(-pre, seq(field('left', lhs), field('operator', op), field('right', rhs)))
    }
    return prec.left(pre, seq(field('left', lhs), field('operator', op), field('right', rhs)))
}

const unary_expression = function (pre, op, rhs) {
    if (pre < 0) {
        return prec.right(-pre, seq(field('operator', op), field('right', rhs)))
    }
    return prec.left(pre, seq(field('operator', op), field('right', rhs)))
}

module.exports = grammar({
    name: 'clingo',
    
    extras: $ => [$.line_comment, $.block_comment, /\s/],

    // Note that the ambiguity between signature and term in show statements
    // does not necessarily have to be resolved in the grammar. It could also
    // be left to the user of the parser. Then, we could simply delete the
    // "show signature" part of the statement production.
    conflicts: $ => [[$.signature, $._function]],

    supertypes: $ => [$._statement, $._term, $._theory_root_term,
		      $._simple_atom, $._head, $._literal_sign, $._theory_term],
    
    rules: {
        source_file: $ => repeat($._statement),

        // comments

        line_comment: _ => token(choice(
            /%[^*].*/,
            '%'
        )),
	
        // TODO: clingo counts nested %* *% blocks
        block_comment: _ => token(
            seq(
                '%*',
                /[^*]*\*+([^%*][^*]*\*+)*/,
                '%'
            ),
        ),

        // terms

        identifier: _ => /[_']*[a-z][A-Za-z0-9_']*/,

        // TODO: clingo does something simpler!
        string: $ => choice(
            seq(
                '"',
                repeat(choice(
                    alias($.unescaped_double_string_fragment, $.string_fragment),
                    $.escape_sequence
                )),
                '"'
            )
        ),

        // Workaround to https://github.com/tree-sitter/tree-sitter/issues/1156
        // We give names to the token() constructs containing a regexp
        // so as to obtain a node in the CST.
        unescaped_double_string_fragment: _ =>
            token.immediate(prec(1, /[^"\\]+/)),

        escape_sequence: _ => token.immediate(seq(
            '\\',
            choice(
                /[^xu0-7]/,
                /[0-7]{1,3}/,
                /x[0-9a-fA-F]{2}/,
                /u[0-9a-fA-F]{4}/,
                /u\{[0-9a-fA-F]+\}/
            )
        )),

        supremum: _ => token(choice(
            '#sup',
            '#supremum'
        )),
	
        infimum: _ => token(choice(
            '#inf',
            '#infimum'
        )),
	
        number: _ => token(choice(
            choice('0', /([1-9][0-9]*)/),
            token(seq('0x', /([0-9A-Fa-f]+)/)),
            token(seq('0o', /([1-7]+)/)),
            token(seq('0b', /([0-1]+)/)),
        )),
	
        anonymous: _ => '_',
	
        variable: _ => /[_']*[A-Z][A-Za-z0-9_']*/,

        _const_term: $ => choice(
            $.infimum,
            $.supremum,
            $.number,
            $.string,
            alias($._const_binary_operation, $.binary_operation),
            alias($._const_unary_operation, $.unary_operation),
            alias($._const_abs, $.abs),
            alias($._const_function, $.function),
            alias($._const_tuple, $.tuple),
        ),

        _const_binary_operation: $ => choice(
            binary_expression(1, $._const_term, "^", $._const_term),
            binary_expression(1, $._const_term, "?", $._const_term),
            binary_expression(1, $._const_term, "&", $._const_term),
            binary_expression(2, $._const_term, "+", $._const_term),
            binary_expression(2, $._const_term, "-", $._const_term),
            binary_expression(3, $._const_term, "*", $._const_term),
            binary_expression(3, $._const_term, "/", $._const_term),
            binary_expression(3, $._const_term, "\\", $._const_term),
            binary_expression(-5, $._const_term, "**", $._const_term),
        ),

        _const_unary_operation: $ => choice(
            unary_expression(4, "-", $._const_term),
            unary_expression(4, "~", $._const_term),
        ),

        _const_abs: $ => seq(
            "|",
            $._const_term,
            "|",
        ),

        _const_terms: $ => seq(
            $._const_term,
            repeat(seq(",", $._const_term))
	),

	_const_arguments_par: $ => seq("(", optional($._const_terms)),
	
        _const_arguments: $ => seq(
	    alias($._const_arguments_par, $.terms),
	    ")"
	),

        _const_function: $ => seq(
            field("name", $.identifier),
            field('arguments', optional($._const_arguments))
	),

        _const_terms_trail: $ => choice(
	    seq(
		$._const_term,
		repeat(seq(",", $._const_term)),
		optional(",")
            ),
	    ","
	),

	_const_terms_trail_par: $ => seq("(", optional($._const_terms_trail)),

	_const_tuple: $ => seq(
	    alias($._const_terms_trail_par, $.terms),
	    ")"
	),

	// based off of clingox.ast operator prec and assoc. values
        binary_operation: $ => choice(
	    binary_expression(0, $._term, "..", $._term),
            binary_expression(1, $._term, "^", $._term),
            binary_expression(1, $._term, "?", $._term),
            binary_expression(1, $._term, "&", $._term),
            binary_expression(2, $._term, "+", $._term),
            binary_expression(2, $._term, "-", $._term),
            binary_expression(3, $._term, "*", $._term),
            binary_expression(3, $._term, "/", $._term),
            binary_expression(3, $._term, "\\", $._term),
            binary_expression(-5, $._term, "**", $._term),
        ),
	// changes in clingo 6: now ** has higher precedence than the unary operators
	// i.e. 2**-1**0 parses as (2 ** -(1 ** 0)), whereas in clingo 5 it parsed as
	// (2 ** ((-1) ** 0))
	// is this an intentional change? Maybe ask Roland.
        unary_operation: $ => choice(
            unary_expression(4, "-", $._term),
            unary_expression(4, "~", $._term),
        ),

        abs: $ => seq(
            "|",
            $._term,
            repeat(seq(";", $._term)),
            "|"
        ),
	
        _terms: $ => seq(
            $._term,
            repeat(seq(",", $._term))
        ),

	terms: $ => $._terms,

	_arguments_par: $ => seq("(", optional($._terms)),
	_arguments_sem: $ => seq(";", optional($._terms)),

	_arguments: $ => seq(
	    alias($._arguments_par, $.terms),
	    repeat(alias($._arguments_sem, $.terms)),
	    ")"
	),

        _function: $ => seq(
            field("name", $.identifier),
            field('arguments', optional($._arguments)),
        ),

	function: $ => $._function,
	
        external_function: $ => seq(
            "@",
            field("name", $.identifier),
            field('arguments', optional($._arguments)),
        ),

        _terms_trail: $ => choice(
	    seq(
		$._term,
		repeat(seq(",", $._term)),
		optional(",")
            ),
	    ","
	),

        _terms_trail_par: $ => seq("(", optional($._terms_trail)),
        _terms_trail_sem: $ => seq(";", optional($._terms_trail)),

        tuple: $ => seq(
            alias($._terms_trail_par, $.terms),
            repeat(alias($._terms_trail_sem, $.terms)),
            ")"
        ),

        _term: $ => choice(
            $.infimum,
            $.supremum,
            $.number,
            $.string,
            $.anonymous,
            $.variable,
            $.binary_operation,
            $.unary_operation,
            $.abs,
            $.function,
            $.external_function,
            $.tuple,
        ),

        // theory terms

        theory_operator: $ => choice(
            /[/!<=>+\-*\\?&@|:;~\^\.]+/,
            "not"
        ),
        theory_operators: $ => repeat1($.theory_operator),

	// we put in some extra and somewhat unnecessary work to make
	// the arguments of theory function/tuple be parsed in way
	// consistent wit regular symbolic atoms/function/tuple
	// arguments, though in the case of theory function/tuple we
	// have no pools.
	
        _theory_terms: $ => seq($._theory_term, repeat(seq(",", $._theory_term))),

	theory_terms: $ => $._theory_terms,

	theory_arguments_par: $ => seq("(", optional($._theory_terms)),

	_theory_arguments: $ => seq(
	    alias($.theory_arguments_par, $.theory_terms),
	    ")"
	),

        theory_function: $ => seq(
	    field("name", $.identifier),
	    field("arguments", optional($._theory_arguments))
	),

        _theory_terms_trail: $ => choice(

	    seq(
		$._theory_term,
		repeat(seq(",", $._theory_term)),
		optional(",")
	    ),
	    ",",
	),

	theory_terms_trail_par: $ => seq("(", optional($._theory_terms_trail)),

        theory_tuple: $ => seq(
	    alias($.theory_terms_trail_par, $.theory_terms),
	    ")"
	),
	
        theory_list: $ => seq("[", optional($.theory_terms), "]"),
	
        theory_set: $ => seq("{", optional($.theory_terms), "}"),

        theory_unparsed_term: $ => choice(
            repeat1(seq($.theory_operators, $._theory_root_term)),
            seq($._theory_root_term,
		repeat1(seq(
		    $.theory_operators,
		    $._theory_root_term
		))
	       ),
        ),

        _theory_term: $ => choice(
            $.theory_unparsed_term,
            $._theory_root_term,
        ),

        _theory_root_term: $ => choice(
            $.theory_function,
            $.theory_tuple,
            $.theory_list,
            $.theory_set,
            $.number,
            $.string,
            $.infimum,
            $.supremum,
            $.variable,
            $.anonymous,
        ),

        // Literals

        boolean_constant: $ => token(choice("#true", "#false")),
	
	_classical_negation_operation: $ => prec.left(
	    1,
	    seq(field("sign", alias("-", $.classical_negation)),
		$._function)
	),

        symbolic_atom: $ => choice(
	    $._function,
	    $._classical_negation_operation,
	),

        relation: $ => token(choice(">", "<", ">=", "<=", "=", "!=")),

        comparison: $ => seq(
	    $._term,
	    $.relation,
	    $._term,
	    repeat(seq($.relation, $._term))),
	
        _simple_atom: $ => choice(
            $.symbolic_atom,
            $.comparison,
            $.boolean_constant,
        ),

        default_negation: $ => "not",
	double_default_negation: $ => "not not",

	_literal_sign: $ => choice(
	    $.default_negation,
	    $.double_default_negation,
	),

        literal: $ => seq(
	    field("sign", optional($._literal_sign)),
	    field("atom",$._simple_atom)),

        // aggregates

        literals: $ => seq($.literal, repeat(seq(",", $.literal))),

        _condition: $ => seq(":", optional($.literals)),

        aggregate_function: $ => token(choice(
            "#sum",
            "#sum+",
            "#min",
            "#max",
            "#count"
        )),

        upper: $ => seq(optional($.relation), $._term),
        lower: $ => seq($._term, optional($.relation)),

        set_aggregate_element: $ => seq(
            field("literal", $.literal),
            field("condition", optional($._condition))),
	
        set_aggregate_elements: $ => seq(
	    $.set_aggregate_element, repeat(seq(";", $.set_aggregate_element))),

        set_aggregate: $ => seq(
	    field("left", optional($.lower)),
	    field("elements",seq("{", optional($.set_aggregate_elements), "}")),
	    field("right", optional($.upper))),

        // theory atoms

        theory_element: $ => choice(
            seq(
		field("theory_terms", $.theory_terms),
		optional(field("condition", $._condition))),
            $._condition,
        ),

        theory_elements: $ => seq($.theory_element, repeat(seq(";", $.theory_element))),

	theory_atom_upper: $ => seq($.theory_operator, $._theory_term),

        theory_atom: $ => seq(
	    "&",
	    field("name", $.identifier),
	    field("arguments", optional($._arguments)),
	    field("elements", optional(seq("{", optional($.theory_elements), "}"))),
	    field("right",optional($.theory_atom_upper))),

        // body literals

        body_aggregate_element: $ => choice(
            field("condition", $._condition),
            seq(field("terms", $.terms), field("condition", optional($._condition))),
        ),
	
        body_aggregate_elements: $ => seq(
	    $.body_aggregate_element,
	    repeat(seq(";", $.body_aggregate_element))
	),
	
        body_aggregate: $ => seq(
	    field("left", optional($.lower)),
	    field("function", $.aggregate_function),
	    field("elements", seq("{", optional($.body_aggregate_elements), "}")),
	    field("right",optional($.upper))
	),

        body_literal: $ => seq(
	    field("sign", optional($._literal_sign)),
	    field("atom", choice(
		$.set_aggregate,
		$.body_aggregate,
		$.theory_atom,
		$._simple_atom
	    ))
	),

        _body_literal_sep: $ => choice(
            seq($.body_literal, choice(";", ",")),
            seq($.conditional_literal, ";"),
        ),

        body: $ => seq(
	    repeat($._body_literal_sep), 
	    choice($.body_literal, $.conditional_literal)
	),
	
	_colon_body: $ => seq(
	    optional(seq(":", field("body", optional($.body)))),
	    "."
	),

        // head literals

        head_aggregate_element: $ => seq(
            field("terms", optional($.terms)),
            ":",
            field("literal", $.literal),
            field("condition", optional($._condition))
	),

        head_aggregate_elements: $ => seq(
	    $.head_aggregate_element, 
	    repeat(seq(";", $.head_aggregate_element))
	),

        head_aggregate: $ => seq(
	    field("left",optional($.lower)), 
	    field("function", $.aggregate_function),
	    field("elements", seq("{", optional($.head_aggregate_elements), "}")),
	    field("right", optional($.upper))
	),

        conditional_literal: $ => seq($.literal, $._condition),

        _conditional_literal_n: $ => seq($.literal, ":", $.literals),

        _conditional_literal_0: $ => seq($.literal, ":"),

        _disjunction_element_sep: $ => choice(
            seq($.literal, choice(",", ";", "|")),
            seq(alias($._conditional_literal_n, $.conditional_literal), 
		choice(";", "|")),
            seq(alias($._conditional_literal_0, $.conditional_literal), ";"),
        ),

        disjunction: $ => choice(
            seq(repeat1($._disjunction_element_sep), 
		choice($.literal, $.conditional_literal)),
            $.conditional_literal,
        ),

        _head: $ => choice(
            $.literal,
            $.disjunction,
            $.set_aggregate,
            $.head_aggregate,
            $.theory_atom,
        ),

        // statements

        rule: $ => seq(
	    field("head", $._head),
	    optional(seq(
		":-",
		field("body", $.body)
	    )),
	    "."
	),

        integrity_constraint: $ => seq(
	    ":-",
	    field("body", optional($.body)),
	    "."
	),

        _optimize_tuple: $ => seq(",", $.terms),
	
        weight: $ => seq(
	    field("term", $._term),
	    field("priority", optional(seq("@", $._term))),
	),

        optimize_element: $ => seq(
            field("weight", $.weight),
            field("terms", optional($._optimize_tuple)),
            field("condition", optional($._condition))
        ),
        optimize_elements: $ => seq(
	    $.optimize_element, 
	    repeat(seq(";", $.optimize_element))),

        weak_constraint: $ => seq(
	    ":~",
	    field("body", optional($.body)),
	    ".",
	    "[", 
	    field("weight", $.weight),
	    field("terms",optional($._optimize_tuple)),
	    "]"
	),

        maximize: $ => seq(
	    choice("#maximize", "#maximise"), 
	    field("elements", seq("{", optional($.optimize_elements), "}")), 
	    "."
	),

        minimize: $ => seq(
	    choice("#minimize", "#minimise"), 
	    field("elements", seq("{", optional($.optimize_elements), "}")),
	    "."
	),

	_classical_negated_identifier: $ => prec.left(
	    1,
	    seq(field("sign", alias("-", $.classical_negation)),
		field("name", $.identifier))
	),

        signature: $ => prec.dynamic(1, seq(
	    choice(
		$._classical_negated_identifier,
		field("name", $.identifier),
	    ),
	    "/",
	    field("arity",$.number)
	)),

        show: $ => seq("#show", "."),

        show_term: $ => seq(
	    "#show",
	    field("term", $._term),
	    $._colon_body
	),

        show_signature: $ => seq(
	    "#show", 
	    field("signature", $.signature),
	    "."
	),

        defined: $ => seq("#defined", field("signature", $.signature), "."),

        project_signature: $ => seq("#project", field("signature", $.signature), "."),

        project_atom: $ => seq(
	    "#project",
	    field("atom", $.symbolic_atom),
	    $._colon_body,
	),

        identifiers: $ => seq($.identifier, repeat(seq(",", $.identifier))),

        program: $ => seq(
	    "#program", 
	    field("name", $.identifier),
	    field("identifiers", optional(seq("(", optional($.identifiers), ")"))),
	    "."
	),

        code: $ => token(
	    repeat(choice(/[^#]/, /#[^e][^#]/, /#e[^n][^#]/, /#en[^d][^#]/,))
	),

        script: $ => seq(
	    "#script",
	    "(",
	    field("language", $.identifier),
	    ")", 
	    field("code", $.code), 
	    "#end", 
	    "."
	),

        const_type: $ => token(choice('default', 'override')),

        const: $ => seq(
	    "#const",
	    field("name", $.identifier),
	    "=",
	    field("value", $._const_term),
	    ".",
	    field("type", optional(seq("[", $.const_type, "]")))
	),

	edge_pair: $ => seq($._term, ",", $._term),

        edge: $ => seq(
	    "#edge", 
	    "(",
	    field("edge_pair", seq($.edge_pair, repeat(seq(";", $.edge_pair)))),
	    ")",
	    $._colon_body,
	),

        heuristic: $ => seq(
            "#heuristic",
            field("atom", $.symbolic_atom),
	    $._colon_body,
            "[",
            field("weight", $.weight),
            ",",
            field("type", $._term),
            "]"
        ),

        include: $ => seq(
	    "#include",
	    choice($.string, seq("<", $.identifier, ">")),
	    "."
	),

        external: $ => seq(
	    "#external",
	    field("atom", $.symbolic_atom),
	    $._colon_body,
	    field("type", optional(seq("[", $._term, "]")))
	),

        theory_operator_arity: _ => token("unary"),
        _theory_operator_arity_binary: _ => token("binary"),

        theory_operator_associativity: _ => token(choice("left", "right")),

        theory_operator_definition: $ => choice(
            seq(
		field("operator", $.theory_operator),
		":",
		field("priority", $.number),
		",",
		field("arity", $.theory_operator_arity)
	    ),
            seq(
		field("operator", $.theory_operator),
		":",
		field("priority", $.number),
		",",
		field("arity", alias($._theory_operator_arity_binary, 
				     $.theory_operator_arity)),
		",",
		field("associativity", $.theory_operator_associativity)
	    ),
        ),

        theory_operator_definitions: $ => seq(
	    $.theory_operator_definition,
	    repeat(seq(";", $.theory_operator_definition))
	),

        theory_term_definition: $ => seq(
	    field("name",$.identifier),
	    field("operators", seq("{", optional($.theory_operator_definitions), "}"))
	),

        theory_atom_type: _ => token(choice("head", "body", "any", "directive")),

        _theory_operators_sep: $ => seq(
	    $.theory_operator,
	    repeat(seq(",", $.theory_operator))
	),

        theory_atom_definition: $ => seq(
            "&",
            field("name", $.identifier),
            "/",
            field("arity", $.number),
            ":",
            field("theory_term_name", $.identifier),
            ",",
            optional(seq(
		field("operators", seq(
                    "{",
                    optional(alias($._theory_operators_sep, $.theory_operators)),
                    "}"
		)),
                ",",
                field("guard", $.identifier),
                ","
	    )),
            field("type", $.theory_atom_type)
        ),

        _theory_definitions: $ => choice(
            seq(
		$.theory_atom_definition,
		repeat(seq(";", $.theory_atom_definition))
	    ),
            seq(
		$.theory_term_definition,
		repeat(seq(";", $.theory_term_definition)),
		repeat(seq(";", $.theory_atom_definition))
	    ),
        ),

        theory: $ => seq(
	    "#theory",
	    field("name", $.identifier),
	    "{",
	    optional($._theory_definitions),
	    "}",
	    "."
	),

        _statement: $ => choice(
            $.rule,
            $.integrity_constraint,
            $.weak_constraint,
            $.minimize,
            $.maximize,
            $.show,
            $.show_term,
            $.show_signature,
            $.defined,
            $.edge,
            $.heuristic,
            $.project_signature,
            $.project_atom,
            $.const,
            $.script,
            $.include,
            $.program,
            $.external,
            $.theory
        ),
    }
});
