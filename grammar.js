const binary_expression = function (pre, lhs, op, rhs) {
    if (pre < 0) {
        return prec.right(-pre, seq(field('lhs', lhs), field('op', op), field('rhs', rhs)))
    }
    return prec.left(pre, seq(field('lhs', lhs), field('op', op), field('rhs', rhs)))
}

const unary_expression = function (pre, op, rhs) {
    if (pre < 0) {
        return prec.right(-pre, seq(field('op', op), field('rhs', rhs)))
    }
    return prec.left(pre, seq(field('op', op), field('rhs', rhs)))
}

module.exports = grammar({
    name: 'clingo',
    extras: $ => [$.line_comment, $.block_comment, /\s/],
    // Note that the ambiguity between signature and term in show statements
    // does not necessarily have to be resolved in the grammar. It could also
    // be left to the user of the parser. Then, we could simply delete the
    // "show signature" part of the statement production.
    conflicts: $ => [[$.signature, $.function]],
    rules: {
        source_file: $ => repeat($.statement),

        line_comment: _$ => token(choice(
            /%[^*].*/,
            '%'
        )),
        // TODO: clingo counts nested %* *% blocks
        block_comment: _$ => token(
            seq(
                '%*',
                /[^*]*\*+([^%*][^*]*\*+)*/,
                '%'
            ),
        ),

        // TODO: looking at other grammars it does not seem like tokens are
        // represented explicitely
        EQ: _$ => '=',
        AT: _$ => '@',
        CONST: _$ => '#const',
        COUNT: _$ => '#count',
        EXTERNAL: _$ => '#external',
        DEFINED: _$ => '#defined',
        GEQ: _$ => '>=',
        GT: _$ => '>',
        IF: _$ => ':-',
        INCLUDE: _$ => '#include',
        LBRACE: _$ => '{',
        LBRACK: _$ => '[',
        LEQ: _$ => '<=',
        LT: _$ => '<',
        MAX: _$ => '#max',
        MAXIMIZE: _$ => choice(
            '#maximize',
            '#maximise'
        ),
        MIN: _$ => '#min',
        MINIMIZE: _$ => choice(
            '#minimize',
            '#minimise'
        ),
        NEQ: _$ => '!=',
        QUESTION: _$ => '?',
        RBRACE: _$ => '}',
        RBRACK: _$ => ']',
        SHOW: _$ => '#show',
        EDGE: _$ => '#edge',
        PROJECT: _$ => '#project',
        HEURISTIC: _$ => '#heuristic',
        SLASH: _$ => '/',
        SUB: _$ => '-',
        SUM: _$ => '#sum',
        SUMP: _$ => '#sum+',
        BLOCK: _$ => '#program',
        WIF: _$ => ':~',
        ANY: _$ => 'any',
        UNARY: _$ => 'unary',
        BINARY: _$ => 'binary',
        LEFT: _$ => 'left',
        RIGHT: _$ => 'right',
        HEAD: _$ => 'head',
        BODY: _$ => 'body',
        DIRECTIVE: _$ => 'directive',
        THEORY: _$ => '#theory',

        anonymous: _$ => '_',

        // the stunt here is meant to exclude not
        identifier: _$ => token(seq(
            choice(
                /[_']+[a-z]/,
                /[a-m,o-z]/,
                /[a-z][a-n,p-z]/,
                /[a-z][a-z][a-s,u-z]/),
            /[A-Za-z0-9_']*/)),

        SCRIPT: _$ => '#script',
        CODE: _$ => token(choice(
            seq(/[^#]*/, /(#+[^e][^#]*)*/, /(#+e*[^n][^#]*)*/, /(#+e*n*[^d][^#]*)*/, '#end'),
        )),
        DEFAULT: _$ => 'default',
        OVERRIDE: _$ => 'override',

        supremum: _$ => choice(
            '#sup',
            '#supremum'
        ),
        infimum: _$ => choice(
            '#inf',
            '#infimum'
        ),
        number: _$ => choice(
            choice('0', /([1-9][0-9]*)/),
            token(seq('0x', /([0-9A-Fa-f]+)/)),
            token(seq('0o', /([1-7]+)/)),
            token(seq('0b', /([0-1]+)/)),
        ),
        variable: _$ => token(seq(repeat('_'), /[A-Z]/, repeat(/[A-Za-z0-9_']/))),

        const_binary: $ => choice(
            binary_expression(7, $._const_term, "^", $._const_term),
            binary_expression(6, $._const_term, "?", $._const_term),
            binary_expression(5, $._const_term, "&", $._const_term),
            binary_expression(4, $._const_term, "+", $._const_term),
            binary_expression(4, $._const_term, "-", $._const_term),
            binary_expression(3, $._const_term, "*", $._const_term),
            binary_expression(3, $._const_term, "/", $._const_term),
            binary_expression(3, $._const_term, "\\", $._const_term),
            binary_expression(-2, $._const_term, "**", $._const_term),
        ),

        const_unary: $ => choice(
            unary_expression(1, "-", $._const_term),
            unary_expression(1, "~", $._const_term),
        ),

        external: _$ => "@",

        const_function: $ => seq(
            field("name", $.identifier),
            field('arguments', optional(seq("(", optional(alias($.const_terms, $.terms)), ")"))),
        ),

        const_tuple: $ => seq(
            "(",
            optional(alias($.const_terms_trail, $.terms)),
            ")"
        ),

        const_abs: $ => seq(
            "|",
            $._const_term,
            "|",
        ),

        const_term: $ => choice(
            alias($.const_binary, $.binary),
            alias($.const_unary, $.unary),
            alias($.const_function, $.function),
            alias($.const_tuple, $.tuple),
            alias($.const_abs, $.abs),
            $.number,
            $.string,
            $.infimum,
            $.supremum,
        ),
        _const_term: $ => alias($.const_term, $.term),

        const_terms: $ => seq(
            $._const_term,
            repeat(seq(",", $._const_term))
        ),
        const_terms_trail: $ => seq(
            $._const_term,
            repeat(seq(",", $._const_term)),
            optional(",")
        ),

        binary: $ => choice(
            binary_expression(8, $.term, "..", $.term),
            binary_expression(7, $.term, "^", $.term),
            binary_expression(6, $.term, "?", $.term),
            binary_expression(5, $.term, "&", $.term),
            binary_expression(4, $.term, "+", $.term),
            binary_expression(4, $.term, "-", $.term),
            binary_expression(3, $.term, "*", $.term),
            binary_expression(3, $.term, "/", $.term),
            binary_expression(3, $.term, "\\", $.term),
            binary_expression(-2, $.term, "**", $.term),
        ),

        unary: $ => choice(
            unary_expression(1, "-", $.term),
            unary_expression(1, "~", $.term),
        ),

        tuple: $ => seq(
            "(",
            optional(alias($.terms_trail, $.terms)),
            repeat(seq(';', alias($.terms_trail, $.terms))),
            ")"
        ),

        external_function: $ => seq(
            "@",
            field("name", $.identifier),
            field('arguments', optional($.pool)),
        ),

        function: $ => seq(
            field("name", $.identifier),
            field('arguments', optional($.pool)),
        ),

        abs: $ => seq(
            "|",
            $.term,
            repeat(seq(";", $.term)),
            "|"
        ),

        term: $ => choice(
            $.binary,
            $.unary,
            $.tuple,
            $.function,
            $.external_function,
            $.abs,
            $.number,
            $.string,
            $.infimum,
            $.supremum,
            $.variable,
            $.anonymous,
        ),

        terms: $ => $._terms,
        _terms: $ => seq(
            $.term,
            repeat(seq(",", $.term))
        ),
        terms_trail: $ => seq(
            $.term,
            repeat(seq(",", $.term)),
            optional(",")
        ),

        separator: _$ => ';',

        // Note: "non-empty" and aliasable to terms
        terms_par: $ => seq("(", optional($._terms)),
        terms_sem: $ => seq(";", optional($._terms)),

        pool: $ => seq(
            alias($.terms_par, $.terms),
            repeat(alias($.terms_sem, $.terms)),
            ")"
        ),
        pool_binary: $ => seq(
            $.term, ",", $.term,
            repeat(seq(";", $.term, ",", $.term))),

        relation: $ => choice(
            $.GT,
            $.LT,
            $.GEQ,
            $.LEQ,
            $.EQ,
            $.NEQ
        ),

        classical_negation: _$ => "-",

        symbolic_atom: $ => seq(
            field("sign", optional($.classical_negation)),
            field("name", $.identifier),
            field("pool", optional($.pool)),
        ),

        comparison: $ => seq($.term, $.relation, $.term, repeat(seq($.relation, $.term))),

        boolean_constant: _$ => choice("#true", "#false"),

        _simple_atom: $ => choice(
            $.symbolic_atom,
            $.comparison,
            $.boolean_constant,
        ),

        default_negation: $ => "not",

        sign: $ => seq($.default_negation, optional($.default_negation)),

        literal: $ => seq(optional($.sign), $._simple_atom),

        literal_tuple: $ => seq($.literal, repeat(seq(",", $.literal))),

        _condition: $ => seq(":", optional($.literal_tuple)),

        aggregate_function: $ => choice(
            $.SUM,
            $.SUMP,
            $.MIN,
            $.MAX,
            $.COUNT
        ),

        upper: $ => seq(optional($.relation), $.term),
        lower: $ => seq($.term, optional($.relation)),

        set_aggregate_element: $ => seq(
            field("literal", $.literal),
            field("condition", optional($._condition))),
        set_aggregate_elements: $ => seq($.set_aggregate_element, repeat(seq(";", $.set_aggregate_element))),

        _set_aggregate: $ => seq($.LBRACE, optional($.set_aggregate_elements), $.RBRACE),
        set_aggregate: $ => seq(optional($.lower), $._set_aggregate, optional($.upper)),

        body_aggregate_element: $ => choice(
            field("condition", $._condition),
            seq(field("tuple", $.terms), field("condition", optional($._condition))),
        ),
        body_aggregate_elements: $ => seq($.body_aggregate_element, repeat(seq(";", $.body_aggregate_element))),

        _body_aggregate: $ => seq($.aggregate_function, $.LBRACE, optional($.body_aggregate_elements), $.RBRACE),
        body_aggregate: $ => seq(optional($.lower), $._body_aggregate, optional($.upper)),

        head_aggregate_element: $ => seq(
            field("tuple", optional($.terms)),
            ":",
            field("literal", $.literal),
            field("condition", optional($._condition))),
        head_aggregate_elements: $ => seq($.head_aggregate_element, repeat(seq(";", $.head_aggregate_element))),

        _head_aggregate: $ => seq($.aggregate_function, $.LBRACE, optional($.head_aggregate_elements), $.RBRACE),
        head_aggregate: $ => seq(optional($.lower), $._head_aggregate, optional($.upper)),

        conditional_literal: $ => seq($.literal, $._condition),
        conditional_literal_n: $ => seq($.literal, ":", $.literal_tuple),
        conditional_literal_0: $ => seq($.literal, ":"),

        _disjunction_element_sep: $ => choice(
            seq($.literal, choice(",", ";", "|")),
            seq(alias($.conditional_literal_n, $.conditional_literal), choice(";", "|")),
            seq(alias($.conditional_literal_0, $.conditional_literal), ";"),
        ),

        disjunction: $ => choice(
            seq(repeat1($._disjunction_element_sep), choice($.literal, $.conditional_literal)),
            $.conditional_literal,
        ),

        head: $ => choice(
            $.literal,
            $.disjunction,
            $.set_aggregate,
            $.head_aggregate,
            $.theory_atom,
        ),

        body_literal: $ => seq(optional($.sign), choice(
            $.set_aggregate,
            $.body_aggregate,
            $.theory_atom,
            $._simple_atom
        )),

        _body_literal_sep: $ => choice(
            seq($.body_literal, choice(";", ",")),
            seq(alias($.conditional_literal, $.body_literal), ";"),
        ),

        _body_literal: $ => choice($.body_literal, alias($.conditional_literal, $.body_literal)),

        body: $ => seq(optional(seq(repeat($._body_literal_sep), $._body_literal)), "."),
        body_0: _$ => ".",

        _colon_body: $ => choice(
            alias($.body_0, $.body),
            seq(":", $.body),
        ),

        signature: $ => prec.dynamic(1, seq(optional($.classical_negation), $.identifier, $.SLASH, $.number)),

        _optimize_tuple: $ => seq(",", $.terms),
        _optimize_weight: $ => seq($.term, optional(seq($.AT, $.term))),

        optimize_element: $ => seq(
            field("weight", $._optimize_weight),
            field("tuple", optional($._optimize_tuple)),
            field("condition", optional($._condition))
        ),
        optimize_elements: $ => seq($.optimize_element, repeat(seq(";", $.optimize_element))),

        identifiers: $ => seq($.identifier, repeat(seq(",", $.identifier))),

        statement: $ => choice(
            seq($.head, choice(".", seq($.IF, $.body))),
            seq($.IF, $.body),
            seq($.WIF, $.body, $.LBRACK, $._optimize_weight, optional($._optimize_tuple), $.RBRACK),
            seq(choice($.MAXIMIZE, $.MINIMIZE), $.LBRACE, optional($.optimize_elements), $.RBRACE, "."),
            seq($.SHOW, "."),
            seq($.SHOW, $.term, $._colon_body),
            seq($.SHOW, $.signature, "."),
            seq($.DEFINED, $.signature, "."),
            seq($.EDGE, "(", $.pool_binary, ")", $._colon_body),
            seq($.HEURISTIC, $.symbolic_atom, $._colon_body, $.LBRACK, $.term, optional(seq($.AT, $.term)), ",", $.term, $.RBRACK),
            seq($.PROJECT, $.signature, "."),
            seq($.PROJECT, $.symbolic_atom, $._colon_body),
            seq($.CONST, $.identifier, $.EQ, $._const_term, ".", optional(seq($.LBRACK, $.DEFAULT, $.RBRACK))),
            seq($.SCRIPT, "(", $.identifier, ")", $.CODE, "."),
            seq($.INCLUDE, choice($.string, seq($.LT, $.identifier, $.GT)), "."),
            seq($.BLOCK, $.identifier, optional(seq("(", optional($.identifiers), ")")), "."),
            seq($.EXTERNAL, $.symbolic_atom, $._colon_body, optional(seq($.LBRACK, $.term, $.RBRACK))),
            seq($.THEORY, $.identifier, $.LBRACE, optional($._theory_definitions), $.RBRACE, ".")
        ),

        // TODO: align with https://github.com/potassco/guide/issues/25
        theory_op: _$ => choice(
            /[/!<=>+\-*\\?&@|:;~\^\.]+/,
            "not"
        ),

        theory_op_list: $ => choice(
            seq($.theory_op_list, $.theory_op),
            $.theory_op
        ),

        theory_term: $ => choice(
            seq($.LBRACE, optional($.theory_opterm_nlist), $.RBRACE),
            seq($.LBRACK, optional($.theory_opterm_nlist), $.RBRACK),
            seq("(", optional(seq($.theory_opterm, optional(seq(",", optional($.theory_opterm_nlist))))), ")"),
            seq($.identifier, "(", optional($.theory_opterm_nlist), ")"),
            $.identifier,
            $.number,
            $.string,
            $.infimum,
            $.supremum,
            $.variable,
        ),

        // TODO: rename
        theory_opterm: $ => choice(
            seq($.theory_opterm, $.theory_op_list, $.theory_term),
            seq($.theory_op_list, $.theory_term),
            $.theory_term
        ),

        // TODO: rename
        theory_opterm_nlist: $ => choice(
            seq($.theory_opterm_nlist, ",", $.theory_opterm),
            $.theory_opterm
        ),

        theory_atom_element: $ => choice(
            seq($.theory_opterm_nlist, optional($._condition)),
            seq($._condition),
        ),

        theory_elements: $ => seq($.theory_atom_element, repeat(seq(";", $.theory_atom_element))),

        theory_atom_name: $ => seq($.identifier, optional($.pool)),

        theory_atom: $ => seq("&", $.theory_atom_name, optional(seq($.LBRACE, optional($.theory_elements), $.RBRACE, optional(seq($.theory_op, $.theory_opterm))))),

        theory_operators: $ => seq($.theory_op, repeat(seq(",", $.theory_op))),

        theory_operator_definition: $ => choice(
            seq($.theory_op, ":", $.number, ",", $.UNARY),
            seq($.theory_op, ":", $.number, ",", $.BINARY, ",", choice($.LEFT, $.RIGHT)),
        ),

        theory_operator_definitions: $ => seq($.theory_operator_definition, repeat(seq(";", $.theory_operator_definition))),

        theory_term_definition: $ => seq($.identifier, $.LBRACE, optional($.theory_operator_definitions), $.RBRACE),
        theory_term_definitions: $ => seq($.theory_term_definition, repeat(seq(";", $.theory_term_definition))),

        theory_atom_type: $ => choice($.HEAD, $.BODY, $.ANY, $.DIRECTIVE),

        theory_atom_definition: $ => seq("&", $.identifier, $.SLASH, $.number, ":", $.identifier, ",", optional(seq($.LBRACE, optional($.theory_operators), $.RBRACE, ",", $.identifier, ",")), $.theory_atom_type),
        theory_atom_definitions: $ => seq($.theory_atom_definition, repeat(seq(";", $.theory_atom_definition))),


        _theory_definitions: $ => choice(
            seq($.theory_atom_definitions),
            seq($.theory_term_definition, optional($.theory_atom_definitions)),
        ),

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
        unescaped_double_string_fragment: _$ =>
            token.immediate(prec(1, /[^"\\]+/)),

        escape_sequence: _$ => token.immediate(seq(
            '\\',
            choice(
                /[^xu0-7]/,
                /[0-7]{1,3}/,
                /x[0-9a-fA-F]{2}/,
                /u[0-9a-fA-F]{4}/,
                /u\{[0-9a-fA-F]+\}/
            )
        )),
    }
});
