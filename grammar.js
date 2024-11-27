module.exports = grammar({
    name: 'clingo',
    extras: $ => [$.line_comment, $.block_comment, /\s/],

    // Note that the ambiguity between signature and term in show statements
    // does not necessarily have to be resolved in the grammar. It could also
    // be left to the user of the parser. Then, we could simply delete the
    // "show signature" part of the statement production.
    conflicts: $ => [[$.signature, $.term]],

    rules: {

        source_file: $ => repeat($.statement),

        // http://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment/36328890#36328890
        // Taken from tree-sitter-prolog
        line_comment: _$ => token(choice(
            seq('%', /[^*]/, /.*/),
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

        // tokens
        ADD: _$ => '+',
        AND: _$ => '&',
        EQ: _$ => '=',
        AT: _$ => '@',
        BNOT: _$ => '~',
        COLON: _$ => ':',
        COMMA: _$ => ',',
        CONST: _$ => '#const',
        COUNT: _$ => '#count',
        DOT: _$ => '.',
        DOTS: _$ => '..',
        EXTERNAL: _$ => '#external',
        DEFINED: _$ => '#defined',
        FALSE: _$ => '#false',
        GEQ: _$ => '>=',
        GT: _$ => '>',
        IF: _$ => ':-',
        INCLUDE: _$ => '#include',
        INFIMUM: _$ => choice(
            '#inf',
            '#infimum'
        ),
        LBRACE: _$ => '{',
        LBRACK: _$ => '[',
        LEQ: _$ => '<=',
        LPAREN: _$ => '(',
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
        MOD: _$ => '\\',
        MUL: _$ => '*',
        NEQ: _$ => '!=',
        POW: _$ => '**',
        QUESTION: _$ => '?',
        RBRACE: _$ => '}',
        RBRACK: _$ => ']',
        RPAREN: _$ => ')',
        SEM: _$ => ';',
        SHOW: _$ => '#show',
        EDGE: _$ => '#edge',
        PROJECT: _$ => '#project',
        HEURISTIC: _$ => '#heuristic',
        SLASH: _$ => '/',
        SUB: _$ => '-',
        SUM: _$ => '#sum',
        SUMP: _$ => '#sum+',
        SUPREMUM: _$ => choice(
            '#sup',
            '#supremum'
        ),
        TRUE: _$ => '#true',
        BLOCK: _$ => '#program',
        VBAR: _$ => '|',
        WIF: _$ => ':~',
        XOR: _$ => '^',
        ANY: _$ => 'any',
        UNARY: _$ => 'unary',
        BINARY: _$ => 'binary',
        LEFT: _$ => 'left',
        RIGHT: _$ => 'right',
        HEAD: _$ => 'head',
        BODY: _$ => 'body',
        DIRECTIVE: _$ => 'directive',
        THEORY: _$ => '#theory',
        NUMBER: $ => choice(
            $.dec,
            $.hex,
            $.oct,
            $.bin,
        ),
        dec: _$ => choice('0', /([1-9][0-9]*)/),
        hex: _$ => token(seq('0x', /([0-9A-Fa-f]+)/)),
        oct: _$ => token(seq('0o', /([1-7]+)/)),
        bin: _$ => token(seq('0b', /([0-1]+)/)),

        ANONYMOUS: _$ => '_',

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
        VARIABLE: _$ => token(seq(repeat('_'), /[A-Z]/, repeat(/[A-Za-z0-9_']/))),
        THEORY_OP: _$ => /[/!<=>+\-*\\?&@|:;~\^\.]+/,
        NOT: _$ => 'not',
        DEFAULT: _$ => 'default',
        OVERRIDE: _$ => 'override',

        constterm: $ => choice(
            prec.left(7, seq($.constterm, $.XOR, $.constterm)),
            prec.left(6, seq($.constterm, $.QUESTION, $.constterm)),
            prec.left(5, seq($.constterm, $.AND, $.constterm)),
            prec.left(4, seq($.constterm, $.ADD, $.constterm)),
            prec.left(4, seq($.constterm, $.SUB, $.constterm)),
            prec.left(3, seq($.constterm, $.MUL, $.constterm)),
            prec.left(3, seq($.constterm, $.SLASH, $.constterm)),
            prec.left(3, seq($.constterm, $.MOD, $.constterm)),
            prec.right(2, seq($.constterm, $.POW, $.constterm)),
            prec.left(1, seq($.SUB, $.constterm)),
            prec.left(1, seq($.BNOT, $.constterm)),
            seq($.identifier, optional(seq($.LPAREN, optional($._constterm_tuple), $.RPAREN))),
            seq($.LPAREN, optional($._constterm_tuple_comma), $.RPAREN),
            seq($.AT, $.identifier, optional(seq($.LPAREN, optional($._constterm_tuple), $.RPAREN))),
            seq($.VBAR, $.constterm, $.VBAR),
            $.NUMBER,
            $.STRING,
            $.INFIMUM,
            $.SUPREMUM,
        ),

        _constterm_tuple: $ => seq($.constterm, repeat(seq($.COMMA, $.constterm))),
        _constterm_tuple_comma: $ => seq($.constterm, repeat(seq($.COMMA, $.constterm)), optional($.COMMA)),

        term: $ => choice(
            prec.left(8, seq($.term, $.DOTS, $.term)),
            prec.left(7, seq($.term, $.XOR, $.term)),
            prec.left(6, seq($.term, $.QUESTION, $.term)),
            prec.left(5, seq($.term, $.AND, $.term)),
            prec.left(4, seq($.term, $.ADD, $.term)),
            prec.left(4, seq($.term, $.SUB, $.term)),
            prec.left(3, seq($.term, $.MUL, $.term)),
            prec.left(3, seq($.term, $.SLASH, $.term)),
            prec.left(3, seq($.term, $.MOD, $.term)),
            prec.right(2, seq($.term, $.POW, $.term)),
            prec.left(1, seq($.SUB, $.term)),
            prec.left(1, seq($.BNOT, $.term)),
            $.term_pool_trail,
            seq(optional($.AT), $.identifier, optional($.term_pool)),
            seq($.VBAR, $.term_pool_unary, $.VBAR),
            $.NUMBER,
            $.STRING,
            $.INFIMUM,
            $.SUPREMUM,
            $.VARIABLE,
            $.ANONYMOUS,
        ),

        term_tuple: $ => seq($.term, repeat(seq($.COMMA, $.term))),
        term_tuple_trail: $ => seq($.term, repeat(seq($.COMMA, $.term)), optional($.COMMA)),

        term_pool: $ => seq($.LPAREN, optional($.term_tuple), repeat(seq(';', $.term_tuple)), $.RPAREN),
        term_pool_unary: $ => seq($.term, repeat(seq($.SEM, $.term))),
        term_pool_binary: $ => seq($.term, $.COMMA, $.term, repeat(seq($.SEM, $.term, $.COMMA, $.term))),
        term_pool_trail: $ => seq($.LPAREN, optional($.term_tuple_trail), repeat(seq(';', $.term_tuple_trail)), $.RPAREN),

        relation: $ => choice(
            $.GT,
            $.LT,
            $.GEQ,
            $.LEQ,
            $.EQ,
            $.NEQ
        ),

        symbolic_atom: $ => seq(optional($.SUB), $.identifier, optional($.term_pool)),

        comparison: $ => seq($.term, $.relation, $.term, repeat(seq($.relation, $.term))),

        boolean_constant: $ => choice($.TRUE, $.FALSE),

        _simple_atom: $ => choice(
            $.symbolic_atom,
            $.comparison,
            $.boolean_constant,
        ),

        sign: $ => seq($.NOT, optional($.NOT)),

        literal: $ => seq(optional($.sign), $._simple_atom),

        literal_tuple: $ => seq($.literal, repeat(seq($.COMMA, $.literal))),

        _condition: $ => seq($.COLON, optional($.literal_tuple)),

        aggregate_function: $ => choice(
            $.SUM,
            $.SUMP,
            $.MIN,
            $.MAX,
            $.COUNT
        ),

        upper: $ => seq(optional($.relation), $.term),
        lower: $ => seq($.term, optional($.relation)),

        set_aggregate_element: $ => seq($.literal, optional($._condition)),
        set_aggregate_elements: $ => seq($.set_aggregate_element, repeat(seq($.SEM, $.set_aggregate_element))),

        body_aggregate_element: $ => choice(
            $._condition,
            seq($.term_tuple, optional($._condition)),
        ),
        body_aggregate_elements: $ => seq($.body_aggregate_element, repeat(seq($.SEM, $.body_aggregate_element))),

        _body_aggregate: $ => choice(
            seq($.LBRACE, optional($.set_aggregate_elements), $.RBRACE),
            seq($.aggregate_function, $.LBRACE, optional($.body_aggregate_elements), $.RBRACE),
        ),
        body_aggregate: $ => seq(optional($.lower), $._body_aggregate, optional($.upper)),

        head_aggregate_element: $ => seq(optional($.term_tuple), $.COLON, $.literal, optional($._condition)),
        head_aggregate_elements: $ => seq($.head_aggregate_element, repeat(seq($.SEM, $.head_aggregate_element))),

        _head_aggregate: $ => choice(
            seq($.LBRACE, optional($.set_aggregate_elements), $.RBRACE),
            seq($.aggregate_function, $.LBRACE, optional($.head_aggregate_elements), $.RBRACE),
        ),
        head_aggregate: $ => choice(
            seq(optional($.lower), $._head_aggregate, optional($.upper)),
        ),

        conditional_literal: $ => seq($.literal, $._condition),
        conditional_literal_n: $ => seq($.literal, $.COLON, $.literal_tuple),
        conditional_literal_0: $ => seq($.literal, $.COLON),

        _disjunction_element_sep: $ => choice(
            seq($.literal, choice($.COMMA, $.SEM, $.VBAR)),
            seq(alias($.conditional_literal_n, $.conditional_literal), choice($.SEM, $.VBAR)),
            seq(alias($.conditional_literal_0, $.conditional_literal), $.SEM),
        ),

        disjunction: $ => choice(
            seq(repeat1($._disjunction_element_sep), choice($.literal, $.conditional_literal)),
            $.conditional_literal,
        ),

        head: $ => choice(
            $.literal,
            $.disjunction,
            $.head_aggregate,
            $.theory_atom,
        ),

        body_literal: $ => seq(optional($.sign), choice(
            $.body_aggregate,
            $.theory_atom,
            $._simple_atom
        )),

        _body_literal_sep: $ => choice(
            seq($.body_literal, choice($.SEM, $.COMMA)),
            seq(alias($.conditional_literal, $.body_literal), $.SEM),
        ),

        _body_literal: $ => choice($.body_literal, alias($.conditional_literal, $.body_literal)),

        body: $ => seq(optional(seq(repeat($._body_literal_sep), $._body_literal)), $.DOT),
        body_0: $ => $.DOT,

        _colon_body: $ => choice(
            alias($.body_0, $.body),
            seq($.COLON, $.body),
        ),

        signature: $ => prec.dynamic(1, seq(optional($.SUB), $.identifier, $.SLASH, $.NUMBER)),

        _optimize_tuple: $ => seq($.COMMA, $.term_tuple),
        _optimize_weight: $ => seq($.term, optional(seq($.AT, $.term))),

        optimize_element: $ => seq($._optimize_weight, optional($._optimize_tuple), optional($._condition)),
        optimize_elements: $ => seq($.optimize_element, repeat(seq($.SEM, $.optimize_element))),

        identifiers: $ => seq($.identifier, repeat(seq($.COMMA, $.identifier))),

        statement: $ => choice(
            seq($.head, choice($.DOT, seq($.IF, $.body))),
            seq($.IF, $.body),
            seq($.WIF, $.body, $.LBRACK, $._optimize_weight, optional($._optimize_tuple), $.RBRACK),
            seq(choice($.MAXIMIZE, $.MINIMIZE), $.LBRACE, optional($.optimize_elements), $.RBRACE, $.DOT),
            seq($.SHOW, $.DOT),
            seq($.SHOW, $.term, $._colon_body),
            seq($.SHOW, $.signature, $.DOT),
            seq($.DEFINED, $.signature, $.DOT),
            seq($.EDGE, $.LPAREN, $.term_pool_binary, $.RPAREN, $._colon_body),
            seq($.HEURISTIC, $.symbolic_atom, $._colon_body, $.LBRACK, $.term, optional(seq($.AT, $.term)), $.COMMA, $.term, $.RBRACK),
            seq($.PROJECT, $.signature, $.DOT),
            seq($.PROJECT, $.symbolic_atom, $._colon_body),
            seq($.CONST, $.identifier, $.EQ, $.constterm, $.DOT, optional(seq($.LBRACK, $.DEFAULT, $.RBRACK))),
            seq($.SCRIPT, $.LPAREN, $.identifier, $.RPAREN, $.CODE, $.DOT),
            seq($.INCLUDE, choice($.STRING, seq($.LT, $.identifier, $.GT)), $.DOT),
            seq($.BLOCK, $.identifier, optional(seq($.LPAREN, optional($.identifiers), $.RPAREN)), $.DOT),
            seq($.EXTERNAL, $.symbolic_atom, $._colon_body, optional(seq($.LBRACK, $.term, $.RBRACK))),
            seq($.THEORY, $.identifier, $.LBRACE, optional($._theory_definitions), $.RBRACE, $.DOT)
        ),

        // TODO: align with https://github.com/potassco/guide/issues/25
        _theory_op: $ => choice(
            $.THEORY_OP,
            alias($.NOT, $.THEORY_OP)
        ),

        theory_op_list: $ => choice(
            seq($.theory_op_list, $._theory_op),
            $._theory_op
        ),

        theory_term: $ => choice(
            seq($.LBRACE, optional($.theory_opterm_nlist), $.RBRACE),
            seq($.LBRACK, optional($.theory_opterm_nlist), $.RBRACK),
            seq($.LPAREN, optional(seq($.theory_opterm, optional(seq($.COMMA, optional($.theory_opterm_nlist))))), $.RPAREN),
            seq($.identifier, $.LPAREN, optional($.theory_opterm_nlist), $.RPAREN),
            $.identifier,
            $.NUMBER,
            $.STRING,
            $.INFIMUM,
            $.SUPREMUM,
            $.VARIABLE,
        ),

        // TODO: rename
        theory_opterm: $ => choice(
            seq($.theory_opterm, $.theory_op_list, $.theory_term),
            seq($.theory_op_list, $.theory_term),
            $.theory_term
        ),

        // TODO: rename
        theory_opterm_nlist: $ => choice(
            seq($.theory_opterm_nlist, $.COMMA, $.theory_opterm),
            $.theory_opterm
        ),

        theory_atom_element: $ => choice(
            seq($.theory_opterm_nlist, optional($._condition)),
            seq($._condition),
        ),

        theory_elements: $ => seq($.theory_atom_element, repeat(seq($.SEM, $.theory_atom_element))),

        theory_atom_name: $ => seq($.identifier, optional($.term_pool)),

        theory_atom: $ => seq($.AND, $.theory_atom_name, optional(seq($.LBRACE, optional($.theory_elements), $.RBRACE, optional(seq($._theory_op, $.theory_opterm))))),

        theory_operators: $ => seq($._theory_op, repeat(seq($.COMMA, $._theory_op))),

        theory_operator_definition: $ => choice(
            seq($._theory_op, $.COLON, $.NUMBER, $.COMMA, $.UNARY),
            seq($._theory_op, $.COLON, $.NUMBER, $.COMMA, $.BINARY, $.COMMA, choice($.LEFT, $.RIGHT)),
        ),

        theory_operator_definitions: $ => seq($.theory_operator_definition, repeat(seq($.SEM, $.theory_operator_definition))),

        theory_term_definition: $ => seq($.identifier, $.LBRACE, optional($.theory_operator_definitions), $.RBRACE),
        theory_term_definitions: $ => seq($.theory_term_definition, repeat(seq($.SEM, $.theory_term_definition))),

        theory_atom_type: $ => choice($.HEAD, $.BODY, $.ANY, $.DIRECTIVE),

        theory_atom_definition: $ => seq($.AND, $.identifier, $.SLASH, $.NUMBER, $.COLON, $.identifier, $.COMMA, optional(seq($.LBRACE, optional($.theory_operators), $.RBRACE, $.COMMA, $.identifier, $.COMMA)), $.theory_atom_type),
        theory_atom_definitions: $ => seq($.theory_atom_definition, repeat(seq($.SEM, $.theory_atom_definition))),


        _theory_definitions: $ => choice(
            seq($.theory_atom_definitions),
            seq($.theory_term_definition, optional($.theory_atom_definitions)),
        ),

        ////////// TODO: This is taken from tree-sitter-java! ////////////
        // Here we tolerate unescaped newlines in double-quoted and
        // single-quoted string literals.
        // This is legal in typescript as jsx/tsx attribute values (as of
        // 2020), and perhaps will be valid in javascript as well in the
        // future.
        //
        STRING: $ => choice(
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
        //
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
