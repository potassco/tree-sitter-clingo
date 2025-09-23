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

    externals: $ => [$.empty_pool_item_first, $.empty_pool_item],

    // Note that the conflict below between signature and function in show statements
    // does not necessarily have to be resolved in the grammar. It could also
    // be left to the user of the parser. Then, we could simply delete the
    // "show signature" part of the statement production, along with this conflict.
    conflicts: $ => [
	[$.signature, $.function]
    ],

    inline: $ => [
	$.atom_identifier,
	$._tuple_pool_item,
	$._const_tuple_item,
	$._simple_atom,
	$._literal_sign,
	$._condition,
	$._body_literal_sep,
	$._colon_body,
	$._disjunction_element_sep,
	$._optimize_tuple,
	$._theory_definitions,
	// these rules  should be inlined as well, but
	// cannot be due to bug:
	// https://github.com/tree-sitter/tree-sitter/issues/2299
	//
	// $._const_term_comma
	// $._const_terms_trail,
	// $._term_comma
	// $._terms_trail,
	// $._theory_terms_trail,
	// $._theory_operators_sep,

    ],

    supertypes: $ => [
	$._statement,
	$._term,
	$._tuple_pool_item,
	$._theory_root_term,
	$._simple_atom,
	$._head,
	$._literal_sign,
	$._theory_term
    ],
    
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
            alias($.const_binary_operation, $.binary_operation),
            alias($.const_unary_operation, $.unary_operation),
            alias($.const_abs, $.abs),
            alias($.const_function, $.function),
            alias($.const_tuple, $.tuple),
        ),

        const_binary_operation: $ => choice(
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

        const_unary_operation: $ => choice(
            unary_expression(4, "-", $._const_term),
            unary_expression(4, "~", $._const_term),
        ),

        const_abs: $ => seq(
            "|",
            $._const_term,
            "|",
        ),

        const_terms: $ => seq(
            $._const_term,
            repeat(seq(",", $._const_term))
	),
	
        _const_arg_pool: $ => seq(
	    "(",
	    field("arguments", optional(alias($.const_terms, $.terms))),
	    ")"
	),

        const_function: $ => seq(
            field("name", $.identifier),
	    optional($._const_arg_pool)
	),

	_const_term_comma: $ => seq($._const_term, ","),

	_const_terms_trail: $ => seq(
            $._const_term,
            repeat1(seq(",", $._const_term)),
            optional(",")
        ),

	_const_tuple_item: $ => choice(
	    alias(",", $.lone_comma),
	    $._const_term,
	    alias($._const_term_comma, $.terms),
	    alias($._const_terms_trail, $.terms),
	),

	const_tuple: $ => seq("(", optional($._const_tuple_item), ")"),

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
	
        terms: $ => seq(
            $._term,
            repeat(seq(",", $._term))
        ),

	_arg_pool_n: $ => seq(
	    field("arguments", choice($.terms,
				      alias($.empty_pool_item_first,
					    $.empty_pool_item))),
	    repeat1(seq(";", field("arguments", choice($.terms, $.empty_pool_item))))
	),

	_arg_pool: $ => seq(
	    "(",
	    choice(field("arguments", optional($.terms)), $._arg_pool_n),
	    ")"
	),

        function: $ => seq(
            field("name", $.identifier),
            optional($._arg_pool),
        ),
	
        external_function: $ => seq(
            "@",
            field("name", $.identifier),
            optional(field("arguments", $._arg_pool)),
        ),

	_term_comma: $ => seq($._term, ","),
	
	_terms_trail: $ => seq(
            $._term,
            repeat1(seq(",", $._term)),
            optional(",")
        ),

	_tuple_pool_item: $ => choice(
	    alias(",", $.lone_comma),
	    $._term,
	    alias($._term_comma, $.terms),
	    alias($._terms_trail, $.terms),
	),

	_tuple_pool_n: $ => seq(
	    choice($._tuple_pool_item,
		   alias($.empty_pool_item_first,
			 $.empty_pool_item)),
	    repeat1(seq(";", choice($._tuple_pool_item, $.empty_pool_item)))
	),

	tuple: $ => seq(
	    "(",
	    choice(optional($._tuple_pool_item), $._tuple_pool_n),
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
        theory_operator: $ => token(choice(
	    "not",
	    // the general pattern would be [/!<=>+\-*\\?&@|:;~\^\.]+,
	    // but we have to exclude the four operators . : ; :-
	    /[/!<=>+\-*\\?&@|~\^]/,
	    /[/!<=>+\-*\\?&@|;~\^\.][/!<=>+\-*\\?&@|:;~\^\.]+/,
	    /:[/!<=>+*\\?&@|:;~\^\.]/,
	    /:[/!<=>+\-*\\?&@|:;~\^\.]{2,}/,
	)),

        theory_operators: $ => repeat1($.theory_operator),
	
        theory_terms: $ => seq($._theory_term, repeat(seq(",", $._theory_term))),

	_theory_arguments : $ => seq(
	    "(", 
	    optional(field("arguments", $.theory_terms)),
	    ")"
	),

        theory_function: $ => seq(
	    field("name", $.identifier),
	    optional($._theory_arguments),
	),

        _theory_terms_trail: $ => choice(
	    seq(
		$._theory_term,
		repeat(seq(",", $._theory_term)),
		optional(",")
	    ),
	    alias(",", $.lone_comma),
	),
	
        theory_tuple: $ => seq(
	    "(",
	    optional(alias($._theory_terms_trail, $.theory_terms)),
	    ")"
	),
	
        theory_list: $ => seq("[", optional($.theory_terms), "]"),
	
        theory_set: $ => seq("{", optional($.theory_terms), "}"),

	theory_unparsed_term: $ => seq(
            optional($._theory_root_term),
	    repeat1(seq(
		$.theory_operators,
		$._theory_root_term
	    ))
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


	atom_identifier: $ => seq(
	    optional(field("sign", alias("-", $.classical_negation))),
	    field("name", $.identifier),	    
	),

	// Ideally we'd like to to have a named child node
	// atom_identifier, as it would make querying the names of atoms
	// somewhat easier. However, atom_identifier *must* be inlined, as
	// otherwise we run into conflicts that I could not resolve
	// with associativity/precedence. In theory we could just inline and
	// write alias($.atom_identifier, $.atom_identifier), however
	// this does no work expected due to a bug:
	// https://github.com/tree-sitter/tree-sitter/issues/2299 
	// so we leave things as is for now.
        symbolic_atom: $ => seq(
	    $.atom_identifier,
            optional(field("arguments", $._arg_pool)),
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
	    optional(field("sign", $._literal_sign)),
	    field("atom",$._simple_atom)),

        // aggregates

        condition: $ => seq($.literal, repeat(seq(",", $.literal))),

        _condition: $ => seq(":", optional(field("condition", $.condition))),

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
            optional(field("condition", $._condition))),
	
        set_aggregate_elements: $ => seq(
	    $.set_aggregate_element, repeat(seq(";", $.set_aggregate_element))),

        set_aggregate: $ => seq(
	    optional(field("left", $.lower)),
	    "{",
	    optional(field("elements", $.set_aggregate_elements)),
	    "}",
	    optional(field("right", $.upper))),

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
	    optional(field("arguments", $._arg_pool)),
	    optional(seq(
		"{",
		optional(field("elements", $.theory_elements)), 
		"}"
	    )),
	    optional(field("right", $.theory_atom_upper))
	),

        // body literals

        body_aggregate_element: $ => choice(
            field("condition", $._condition),
            seq(field("terms", $.terms), 
		optional(field("condition", $._condition))
	       ),
        ),
	
        body_aggregate_elements: $ => seq(
	    $.body_aggregate_element,
	    repeat(seq(";", $.body_aggregate_element))
	),
	
        body_aggregate: $ => seq(
	    optional(field("left", $.lower)),
	    field("function", $.aggregate_function),
	    "{",
	    optional(field("elements", $.body_aggregate_elements)),
	    "}",
	    optional(field("right", $.upper))
	),

        body_literal: $ => seq(
	    optional(field("sign", $._literal_sign)),
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
	    optional(seq(":", optional(field("body", $.body)))),
	    "."
	),

        // head literals

        head_aggregate_element: $ => seq(
            optional(field("terms", $.terms)),
            ":",
            field("literal", $.literal),
            optional(field("condition", $._condition))
	),

        head_aggregate_elements: $ => seq(
	    $.head_aggregate_element, 
	    repeat(seq(";", $.head_aggregate_element))
	),

        head_aggregate: $ => seq(
	    optional(field("left", $.lower)), 
	    field("function", $.aggregate_function),
	    "{",
	    optional(field("elements", $.head_aggregate_elements)),
	    "}",
	    optional(field("right", $.upper))
	),

        _conditional_literal_n: $ => seq(
	    field("literal", $.literal),
	    ":",
	    field("condition", $.condition)
	),

        _conditional_literal_0: $ => seq(
	    field("literal", $.literal),
	    ":"
	),

	conditional_literal: $ => choice(
	    $._conditional_literal_n,
	    $._conditional_literal_0,
	),

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
	    optional(field("body", $.body)),
	    "."
	),

        _optimize_tuple: $ => seq(",", $.terms),
	
        weight: $ => seq(
	    field("term", $._term),
	    optional(seq(
		"@",
		field("priority",$._term)
	    ))
	),

        optimize_element: $ => seq(
            field("weight", $.weight),
            optional(field("terms", $._optimize_tuple)),
            optional(field("condition", $._condition))
        ),
        optimize_elements: $ => seq(
	    $.optimize_element, 
	    repeat(seq(";", $.optimize_element))),

        weak_constraint: $ => seq(
	    ":~",
	    optional(field("body", $.body)),
	    ".",
	    "[", 
	    field("weight", $.weight),
	    optional(field("terms", $._optimize_tuple)),
	    "]"
	),

        maximize: $ => seq(
	    choice("#maximize", "#maximise"), 
	    "{",
	    optional(field("elements", $.optimize_elements)), 
	    "}",
	    "."
	),

        minimize: $ => seq(
	    choice("#minimize", "#minimise"),
	    "{",
	    optional(field("elements", $.optimize_elements)),
	    "}",
	    "."
	),

        signature: $ => seq(
	    $.atom_identifier,
	    "/",
	    field("arity",$.number)
	),

        show: $ => seq(
	    "#show",
	    "."
	),

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

        defined: $ => seq(
	    "#defined",
	    field("signature", $.signature),
	    "."
	),

        project_signature: $ => seq(
	    "#project", 
	    field("signature", $.signature),
	    "."
	),

        project_atom: $ => seq(
	    "#project",
	    field("atom", $.symbolic_atom),
	    $._colon_body,
	),

        parameters: $ => seq($.identifier, repeat(seq(",", $.identifier))),

        program: $ => seq(
	    "#program", 
	    field("name", $.identifier),
	    optional(seq(
		"(",
		optional(field("parameters", $.parameters)),
		")"
	    )),
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
	    optional(seq(
		"[",
		field("type", $.const_type),
		"]"
	    ))
	),

	edge_pair: $ => seq($._term, ",", $._term),

        edge: $ => seq(
	    "#edge", 
	    "(",
	    field("edge_pair", $.edge_pair),
	    repeat(seq(";", field("edge_pair", $.edge_pair))),
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
	    optional(seq(
		"[",
		field("type", $._term),
		"]"
	    ))
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
	    "{",
	    optional(field("operators", $.theory_operator_definitions)),
	    "}"
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
		"{",
		optional(field("operators", alias($._theory_operators_sep, $.theory_operators))),
                "}",
                ",",
                field("guard", $.identifier),
                ","
	    )),
            field("atom_type", $.theory_atom_type)
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
