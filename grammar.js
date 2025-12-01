const binary_expression = function (pre, lhs, op, rhs) {
  if (pre < 0) {
    return prec.right(
      -pre,
      seq(field("left", lhs), field("operator", op), field("right", rhs)),
    );
  }
  return prec.left(
    pre,
    seq(field("left", lhs), field("operator", op), field("right", rhs)),
  );
};

const unary_expression = function (pre, op, rhs) {
  if (pre < 0) {
    return prec.right(-pre, seq(field("operator", op), field("right", rhs)));
  }
  return prec.left(pre, seq(field("operator", op), field("right", rhs)));
};

const ws_rgx = token.immediate(/[\s\r\n]*/);
const identifier_rgx = /[_']*[a-z][A-Za-z0-9_']*/;
const variable_rgx = /[_']*[A-Z][A-Za-z0-9_']*/;

module.exports = grammar({
  name: "clingo",

  extras: ($) => [$.line_comment, $.block_comment, /\s/],

  externals: ($) => [
    $.empty_pool_item_first,
    $.empty_pool_item,
    $.colon,
    $.block_comment,
    $.doc_fragment_string,
    $._doc_token_args,
    $._doc_token_paren,
    $._doc_token_minus,
  ],

  // Note that the conflict below between signature and function in show statements
  // does not necessarily have to be resolved in the grammar. It could also
  // be left to the user of the parser. Then, we could simply delete the
  // "show signature" part of the statement production, along with this conflict.
  conflicts: ($) => [[$.signature, $.function]],

  inline: ($) => [
    $.atom_identifier,
    $._const_tuple_item,
    $._theory_operator,
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

  supertypes: ($) => [
    $.statement,
    $.term,
    $.tuple_pool_item,
    $.theory_root_term,
    $.simple_atom,
    $.head,
    $.literal_sign,
    $.theory_term,
  ],

  rules: {
    source_file: ($) => repeat($.statement),

    // comments

    line_comment: (_) => token(choice(/%[^*\n\r][^\n\r]*/, "%")),

    doc_comment: ($) =>
      seq(
        "%*!",
        ws_rgx,
        field("predicate", $.doc_predicate),
        // no doc_ws
        optional(field("description", $.doc_desc)),
        // no doc_ws
        optional(field("arguments", $.doc_args)),
        // no doc_ws
        token.immediate("*%"),
      ),

    // NOTE: gobbles up trailing whitespace
    variables: ($) =>
      seq(
        alias($.doc_var, $.variable),
        ws_rgx,
        repeat(
          seq(
            token.immediate(","),
            ws_rgx,
            alias($.doc_var, $.variable),
            ws_rgx,
          ),
        ),
      ),

    // NOTE: gobbles up trailing whitespace
    doc_predicate: ($) =>
      seq(
        field("name", alias(token.immediate(identifier_rgx), $.identifier)),
        ws_rgx,
        optional(
          seq(
            alias($._doc_token_paren, "("),
            ws_rgx,
            optional(seq(field("variables", $.variables))),
            // no doc_ws
            token.immediate(")"),
            ws_rgx,
          ),
        ),
      ),

    doc_var: (_) => token.immediate(variable_rgx),

    doc_fragment_emph: (_) => token.immediate(/\*[^`%*_\r\n][^`*_\r\n]*\*/),
    doc_fragment_bold: (_) => token.immediate(/\*\*[^`%*_\r\n][^`*_\r\n]*\*\*/),
    doc_fragment_italic: (_) => token.immediate(/_[^`*_\r\n]+_/),
    doc_fragment_code: (_) => token.immediate(/`[^`*_\r\n]+`/),

    // NOTE: gobbles up trailing whitespace
    doc_desc: ($) =>
      repeat1(
        choice(
          $.doc_fragment_bold,
          $.doc_fragment_emph,
          $.doc_fragment_italic,
          $.doc_fragment_code,
          $.doc_fragment_string,
        ),
      ),

    // NOTE: gobbles up trailing whitespace
    doc_args: ($) => seq($._doc_token_args, ws_rgx, repeat($.doc_arg)),
    // NOTE: gobbles up trailing whitespace
    doc_arg: ($) =>
      seq(
        $._doc_token_minus,
        ws_rgx,
        field("variable", alias($.doc_var, $.variable)),
        // no doc_ws
        token.immediate(":"),
        ws_rgx,
        optional(field("description", $.doc_desc)),
      ),

    // terms

    identifier: (_) => identifier_rgx,

    string: ($) =>
      choice(
        seq(
          '"',
          repeat(
            choice(
              alias($._string_fragment, $.string_fragment),
              alias($._string_escape, $.escape_sequence),
            ),
          ),
          '"',
        ),
      ),

    fstring: ($) =>
      choice(
        seq(
          'f"',
          repeat(
            choice(
              alias($._fstring_fragment, $.string_fragment),
              alias($._fstring_escape, $.escape_sequence),
              $.fstring_field,
            ),
          ),
          '"',
        ),
      ),

    _string_fragment: (_) => token.immediate(prec(1, /[^"\\\x00]+/)),
    _fstring_fragment: (_) => token.immediate(prec(1, /[^{}"\\\x00]+/)),

    _string_escape: (_) => token.immediate(/\\[n"\\]/),
    _fstring_escape: (_) => token.immediate(/\\[n"\\]|\{\{|\}\}/),

    // NOTE: We use immediate tokens here to prevent extra parsing between spec
    // elements. This grammare permits a bit more whitespace before accessors,
    // conversions, and specs as compared to clingo. Matching exactly clingo's
    // behavior would somewhat complicate the grammar.
    fstring_accessor: ($) =>
      repeat1(
        choice(
          alias(/[.][a-z][A-Za-z0-9_]*/, $.identifier),
          seq(
            /\[/,
            alias(token.immediate(/(0|[1-9][0-9]*)/), $.number),
            token.immediate(/\]/),
          ),
        ),
      ),
    fstring_conversion: (_) => seq(/!/, token.immediate(/[rs]?/)),
    // NOTE: We match align and fill together becase the scanner does not
    // support lookahead.
    fstring_align: (_) => token.immediate(/[^\n\x00]?[<>=^]/),
    fstring_sign: (_) => token.immediate(/[-+ ]/),
    fstring_alternate: (_) => token.immediate(/#/),
    fstring_width: (_) => token.immediate(/(0|[1-9][0-9]*)/),
    fstring_grouping: (_) => token.immediate(/[,_]/),
    fstring_type: (_) => token.immediate(/[bcdoxXns]/),

    fstring_spec: ($) =>
      seq(
        /:/,
        optional($.fstring_align),
        optional($.fstring_sign),
        optional($.fstring_alternate),
        optional($.fstring_width),
        optional($.fstring_grouping),
        optional($.fstring_type),
      ),

    fstring_field: ($) =>
      seq(
        "{",
        $.term,
        optional($.fstring_accessor),
        optional($.fstring_conversion),
        optional($.fstring_spec),
        token.immediate("}"),
      ),

    supremum: (_) => token(choice("#sup", "#supremum")),

    infimum: (_) => token(choice("#inf", "#infimum")),

    number: (_) =>
      token(
        choice(
          choice("0", /([1-9][0-9]*)/),
          token(seq("0x", /([0-9A-Fa-f]+)/)),
          token(seq("0o", /([1-7]+)/)),
          token(seq("0b", /([0-1]+)/)),
        ),
      ),

    anonymous: (_) => "_",

    variable: (_) => variable_rgx,

    _const_term: ($) =>
      choice(
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

    const_binary_operation: ($) =>
      choice(
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

    const_unary_operation: ($) =>
      choice(
        unary_expression(4, "-", $._const_term),
        unary_expression(4, "~", $._const_term),
      ),

    const_abs: ($) => seq("|", $._const_term, "|"),

    const_terms: ($) => seq($._const_term, repeat(seq(",", $._const_term))),

    _const_arg_pool: ($) =>
      seq(
        "(",
        field("arguments", optional(alias($.const_terms, $.terms))),
        ")",
      ),

    const_function: ($) =>
      seq(field("name", $.identifier), optional($._const_arg_pool)),

    _const_term_comma: ($) => seq($._const_term, ","),

    _const_terms_trail: ($) =>
      seq($._const_term, repeat1(seq(",", $._const_term)), optional(",")),

    _const_tuple_item: ($) =>
      choice(
        alias(",", $.lone_comma),
        $._const_term,
        alias($._const_term_comma, $.terms),
        alias($._const_terms_trail, $.terms),
      ),

    const_tuple: ($) => seq("(", optional($._const_tuple_item), ")"),

    // based off of clingox.ast operator prec and assoc. values
    binary_operation: ($) =>
      choice(
        binary_expression(0, $.term, "..", $.term),
        binary_expression(1, $.term, "^", $.term),
        binary_expression(1, $.term, "?", $.term),
        binary_expression(1, $.term, "&", $.term),
        binary_expression(2, $.term, "+", $.term),
        binary_expression(2, $.term, "-", $.term),
        binary_expression(3, $.term, "*", $.term),
        binary_expression(3, $.term, "/", $.term),
        binary_expression(3, $.term, "\\", $.term),
        binary_expression(-5, $.term, "**", $.term),
      ),

    unary_operation: ($) =>
      choice(
        unary_expression(4, "-", $.term),
        unary_expression(4, "~", $.term),
      ),

    abs: ($) => seq("|", $.term, repeat(seq(";", $.term)), "|"),

    terms: ($) => seq($.term, repeat(seq(",", $.term))),

    _arg_pool_n: ($) =>
      seq(
        field(
          "arguments",
          choice($.terms, alias($.empty_pool_item_first, $.empty_pool_item)),
        ),
        repeat1(
          seq(";", field("arguments", choice($.terms, $.empty_pool_item))),
        ),
      ),

    _arg_pool: ($) =>
      seq(
        "(",
        choice(field("arguments", optional($.terms)), $._arg_pool_n),
        ")",
      ),

    function: ($) => seq(field("name", $.identifier), optional($._arg_pool)),

    external_function: ($) =>
      seq(
        "@",
        field("name", $.identifier),
        optional(field("arguments", $._arg_pool)),
      ),

    _term_comma: ($) => seq($.term, ","),

    _terms_trail: ($) => seq($.term, repeat1(seq(",", $.term)), optional(",")),

    tuple_pool_item: ($) =>
      choice(
        alias(",", $.lone_comma),
        $.term,
        alias($._term_comma, $.terms),
        alias($._terms_trail, $.terms),
      ),

    _tuple_pool_n: ($) =>
      seq(
        choice(
          $.tuple_pool_item,
          alias($.empty_pool_item_first, $.empty_pool_item),
        ),
        repeat1(seq(";", choice($.tuple_pool_item, $.empty_pool_item))),
      ),

    tuple: ($) =>
      seq("(", choice(optional($.tuple_pool_item), $._tuple_pool_n), ")"),

    term: ($) =>
      choice(
        $.infimum,
        $.supremum,
        $.number,
        $.string,
        $.fstring,
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
    theory_operator: (_) =>
      token(
        choice(
          // the general pattern would be [/!<=>+\-*\\?&@|:;~\^\.]+,
          // but we have to exclude the four operators . : ; :-
          /[/!<=>+\-*\\?&@|~\^]/,
          /[/!<=>+\-*\\?&@|;~\^\.][/!<=>+\-*\\?&@|:;~\^\.]+/,
          /:[/!<=>+*\\?&@|:;~\^\.]/,
          /:[/!<=>+\-*\\?&@|:;~\^\.]{2,}/,
        ),
      ),

    _theory_operator: ($) =>
      choice($.theory_operator, alias($.default_negation, $.theory_operator)),

    theory_operators: ($) => repeat1($._theory_operator),

    theory_terms: ($) => seq($.theory_term, repeat(seq(",", $.theory_term))),

    _theory_arguments: ($) =>
      seq("(", optional(field("arguments", $.theory_terms)), ")"),

    theory_function: ($) =>
      seq(field("name", $.identifier), optional($._theory_arguments)),

    _theory_terms_trail: ($) =>
      choice(
        seq($.theory_term, repeat(seq(",", $.theory_term)), optional(",")),
        alias(",", $.lone_comma),
      ),

    theory_tuple: ($) =>
      seq("(", optional(alias($._theory_terms_trail, $.theory_terms)), ")"),

    theory_list: ($) => seq("[", optional($.theory_terms), "]"),

    theory_set: ($) => seq("{", optional($.theory_terms), "}"),

    theory_unparsed_term: ($) =>
      seq(
        optional($.theory_root_term),
        repeat1(seq($.theory_operators, $.theory_root_term)),
      ),

    theory_term: ($) => choice($.theory_unparsed_term, $.theory_root_term),

    theory_root_term: ($) =>
      choice(
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

    boolean_constant: (_) => token(choice("#true", "#false")),

    atom_identifier: ($) =>
      seq(
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
    symbolic_atom: ($) =>
      seq($.atom_identifier, optional(field("arguments", $._arg_pool))),

    relation: (_) => token(choice(">", "<", ">=", "<=", "=", "!=")),

    comparison: ($) =>
      seq($.term, $.relation, $.term, repeat(seq($.relation, $.term))),

    simple_atom: ($) =>
      choice($.symbolic_atom, $.comparison, $.boolean_constant),

    default_negation: (_) => "not",
    double_default_negation: (_) => "not not",

    literal_sign: ($) => choice($.default_negation, $.double_default_negation),

    literal: ($) =>
      seq(
        optional(field("sign", $.literal_sign)),
        field("atom", $.simple_atom),
      ),

    // aggregates

    condition: ($) => seq($.literal, repeat(seq(",", $.literal))),

    _condition: ($) =>
      seq(alias($.colon, ":"), optional(field("condition", $.condition))),

    aggregate_function: (_) =>
      token(choice("#sum", "#sum+", "#min", "#max", "#count")),

    upper: ($) => seq(optional($.relation), $.term),
    lower: ($) => seq($.term, optional($.relation)),

    set_aggregate_element: ($) =>
      seq(field("literal", $.literal), optional($._condition)),

    set_aggregate_elements: ($) =>
      seq($.set_aggregate_element, repeat(seq(";", $.set_aggregate_element))),

    set_aggregate: ($) =>
      seq(
        optional(field("left", $.lower)),
        "{",
        optional(field("elements", $.set_aggregate_elements)),
        "}",
        optional(field("right", $.upper)),
      ),

    // theory atoms

    theory_element: ($) =>
      choice(
        seq(field("theory_terms", $.theory_terms), optional($._condition)),
        $._condition,
      ),

    theory_elements: ($) =>
      seq($.theory_element, repeat(seq(";", $.theory_element))),

    theory_atom_upper: ($) => seq($._theory_operator, $.theory_term),

    theory_atom: ($) =>
      seq(
        "&",
        field("name", $.identifier),
        optional(field("arguments", $._arg_pool)),
        optional(seq("{", optional(field("elements", $.theory_elements)), "}")),
        optional(field("right", $.theory_atom_upper)),
      ),

    // body literals

    body_aggregate_element: ($) =>
      choice(
        $._condition,
        seq(field("terms", $.terms), optional($._condition)),
      ),

    body_aggregate_elements: ($) =>
      seq($.body_aggregate_element, repeat(seq(";", $.body_aggregate_element))),

    body_aggregate: ($) =>
      seq(
        optional(field("left", $.lower)),
        field("function", $.aggregate_function),
        "{",
        optional(field("elements", $.body_aggregate_elements)),
        "}",
        optional(field("right", $.upper)),
      ),

    body_literal: ($) =>
      seq(
        optional(field("sign", $.literal_sign)),
        field(
          "atom",
          choice(
            $.set_aggregate,
            $.body_aggregate,
            $.theory_atom,
            $.simple_atom,
          ),
        ),
      ),

    _body_literal_sep: ($) =>
      choice(
        seq($.body_literal, choice(";", ",")),
        seq($.conditional_literal, ";"),
      ),

    body: ($) =>
      seq(
        repeat($._body_literal_sep),
        choice($.body_literal, $.conditional_literal),
      ),

    _colon_body: ($) =>
      seq(
        optional(seq(alias($.colon, ":"), optional(field("body", $.body)))),
        ".",
      ),

    // head literals

    head_aggregate_element: ($) =>
      seq(
        optional(field("terms", $.terms)),
        alias($.colon, ":"),
        field("literal", $.literal),
        optional($._condition),
      ),

    head_aggregate_elements: ($) =>
      seq($.head_aggregate_element, repeat(seq(";", $.head_aggregate_element))),

    head_aggregate: ($) =>
      seq(
        optional(field("left", $.lower)),
        field("function", $.aggregate_function),
        "{",
        optional(field("elements", $.head_aggregate_elements)),
        "}",
        optional(field("right", $.upper)),
      ),

    _conditional_literal_n: ($) =>
      seq(
        field("literal", $.literal),
        alias($.colon, ":"),
        field("condition", $.condition),
      ),

    _conditional_literal_0: ($) =>
      seq(field("literal", $.literal), alias($.colon, ":")),

    conditional_literal: ($) =>
      choice($._conditional_literal_n, $._conditional_literal_0),

    _disjunction_element_sep: ($) =>
      choice(
        seq($.literal, choice(",", ";", "|")),
        seq(
          alias($._conditional_literal_n, $.conditional_literal),
          choice(";", "|"),
        ),
        seq(alias($._conditional_literal_0, $.conditional_literal), ";"),
      ),

    disjunction: ($) =>
      choice(
        seq(
          repeat1($._disjunction_element_sep),
          choice($.literal, $.conditional_literal),
        ),
        $.conditional_literal,
      ),

    head: ($) =>
      choice(
        $.literal,
        $.disjunction,
        $.set_aggregate,
        $.head_aggregate,
        $.theory_atom,
      ),

    // statements

    rule: ($) =>
      seq(
        field("head", $.head),
        optional(seq(":-", field("body", $.body))),
        ".",
      ),

    integrity_constraint: ($) =>
      seq(":-", optional(field("body", $.body)), "."),

    _optimize_tuple: ($) => seq(",", $.terms),

    weight: ($) =>
      seq(field("term", $.term), optional(seq("@", field("priority", $.term)))),

    optimize_element: ($) =>
      seq(
        field("weight", $.weight),
        optional(field("terms", $._optimize_tuple)),
        optional($._condition),
      ),
    optimize_elements: ($) =>
      seq($.optimize_element, repeat(seq(";", $.optimize_element))),

    weak_constraint: ($) =>
      seq(
        ":~",
        optional(field("body", $.body)),
        ".",
        "[",
        field("weight", $.weight),
        optional(field("terms", $._optimize_tuple)),
        "]",
      ),

    maximize: ($) =>
      seq(
        choice("#maximize", "#maximise"),
        "{",
        optional(field("elements", $.optimize_elements)),
        "}",
        ".",
      ),

    minimize: ($) =>
      seq(
        choice("#minimize", "#minimise"),
        "{",
        optional(field("elements", $.optimize_elements)),
        "}",
        ".",
      ),

    signature: ($) => seq($.atom_identifier, "/", field("arity", $.number)),

    show: (_) => seq("#show", "."),

    show_term: ($) => seq("#show", field("term", $.term), $._colon_body),

    show_signature: ($) => seq("#show", field("signature", $.signature), "."),

    defined: ($) => seq("#defined", field("signature", $.signature), "."),

    project_signature: ($) =>
      seq("#project", field("signature", $.signature), "."),

    project_atom: ($) =>
      seq("#project", field("atom", $.symbolic_atom), $._colon_body),

    parameters: ($) => seq($.identifier, repeat(seq(",", $.identifier))),

    program: ($) =>
      seq(
        "#program",
        field("name", $.identifier),
        optional(seq("(", optional(field("parameters", $.parameters)), ")")),
        ".",
      ),

    code: (_) =>
      token(repeat(choice(/[^#]/, /#[^e][^#]/, /#e[^n][^#]/, /#en[^d][^#]/))),

    script: ($) =>
      seq(
        "#script",
        "(",
        field("language", $.identifier),
        ")",
        field("code", $.code),
        "#end",
        ".",
      ),

    const_type: (_) => token(choice("default", "override")),

    const: ($) =>
      seq(
        "#const",
        field("name", $.identifier),
        "=",
        field("value", $._const_term),
        ".",
        optional(seq("[", field("type", $.const_type), "]")),
      ),

    edge_pair: ($) => seq($.term, ",", $.term),

    edge: ($) =>
      seq(
        "#edge",
        "(",
        field("edge_pair", $.edge_pair),
        repeat(seq(";", field("edge_pair", $.edge_pair))),
        ")",
        $._colon_body,
      ),

    heuristic: ($) =>
      seq(
        "#heuristic",
        field("atom", $.symbolic_atom),
        $._colon_body,
        "[",
        field("weight", $.weight),
        ",",
        field("type", $.term),
        "]",
      ),

    include: ($) =>
      seq("#include", choice($.string, seq("<", $.identifier, ">")), "."),

    external: ($) =>
      seq(
        "#external",
        field("atom", $.symbolic_atom),
        $._colon_body,
        optional(seq("[", field("type", $.term), "]")),
      ),

    theory_operator_arity: (_) => token("unary"),
    _theory_operator_arity_binary: (_) => token("binary"),

    theory_operator_associativity: (_) => token(choice("left", "right")),

    theory_operator_definition: ($) =>
      choice(
        seq(
          field("operator", $._theory_operator),
          alias($.colon, ":"),
          field("priority", $.number),
          ",",
          field("arity", $.theory_operator_arity),
        ),
        seq(
          field("operator", $._theory_operator),
          alias($.colon, ":"),
          field("priority", $.number),
          ",",
          field(
            "arity",
            alias($._theory_operator_arity_binary, $.theory_operator_arity),
          ),
          ",",
          field("associativity", $.theory_operator_associativity),
        ),
      ),

    theory_operator_definitions: ($) =>
      seq(
        $.theory_operator_definition,
        repeat(seq(";", $.theory_operator_definition)),
      ),

    theory_term_definition: ($) =>
      seq(
        field("name", $.identifier),
        "{",
        optional(field("operators", $.theory_operator_definitions)),
        "}",
      ),

    theory_atom_type: (_) => token(choice("head", "body", "any", "directive")),

    _theory_operators_sep: ($) =>
      seq($._theory_operator, repeat(seq(",", $._theory_operator))),

    theory_atom_definition: ($) =>
      seq(
        "&",
        field("name", $.identifier),
        "/",
        field("arity", $.number),
        alias($.colon, ":"),
        field("theory_term_name", $.identifier),
        ",",
        optional(
          seq(
            "{",
            optional(
              field(
                "operators",
                alias($._theory_operators_sep, $.theory_operators),
              ),
            ),
            "}",
            ",",
            field("guard", $.identifier),
            ",",
          ),
        ),
        field("atom_type", $.theory_atom_type),
      ),

    _theory_definitions: ($) =>
      choice(
        seq(
          $.theory_atom_definition,
          repeat(seq(";", $.theory_atom_definition)),
        ),
        seq(
          $.theory_term_definition,
          repeat(seq(";", $.theory_term_definition)),
          repeat(seq(";", $.theory_atom_definition)),
        ),
      ),

    theory: ($) =>
      seq(
        "#theory",
        field("name", $.identifier),
        "{",
        optional($._theory_definitions),
        "}",
        ".",
      ),

    statement: ($) =>
      choice(
        $.doc_comment,
        $.rule,
        $.integrity_constraint,
        $.weak_constraint,
        $.minimize,
        $.maximize,
        $.show,
        prec.dynamic(1, $.show_term),
        prec.dynamic(2, $.show_signature),
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
        $.theory,
      ),
  },
});
