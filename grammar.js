module.exports = grammar({
  name: 'clingo',
  extras: $ => [$.comment,/\s/],
  
  rules: {

    source_file: $ => repeat($.statement),

    // http://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment/36328890#36328890
    // Taken from tree-sitter-prolog
    comment: $ => token(choice(
      seq('%', /.*/),
      seq(
        '%*',
        /[^*]*\*+([^%*][^*]*\*+)*/,
        '%'
      )
    )),

//     token
  ADD: $ => '+',
    AND: $ => '&',
    EQ: $ => '=',
    AT: $ => '@',
//     BASE        "#base"
    BNOT: $ => '~',
    COLON: $ => ':',
    COMMA: $ => ',',
    CONST: $ => '#const',
    COUNT: $ => '#count',
    CSP: $ => '$',
    CSP_ADD: $ => '$+',
    CSP_SUB: $ => '$-',
    CSP_MUL: $ => '$*',
    CSP_LEQ: $ => '$<=',
    CSP_LT: $ => '$<',
    CSP_GT: $ => '$>',
    CSP_GEQ: $ => '$>=',
    CSP_EQ: $ => '$=',
    CSP_NEQ: $ => '$!=',
//     CUMULATIVE  "#cumulative"
    DISJOINT: $ => '#disjoint',
    DOT: $ => '.',
    DOTS: $ => '..',
//     END         0 "<EOF>"
    EXTERNAL: $ => '#external',
    DEFINED: $ => '#defined',
    FALSE: $ => '#false',
//     FORGET      "#forget"
    GEQ: $ => '>=',
    GT: $ => '>',
    IF: $ => ':-',
    INCLUDE: $ => '#include',
    INFIMUM: $ => '#inf',
    LBRACE: $ => '{',
    LBRACK: $ => '[',
    LEQ: $ => '<=',
    LPAREN: $ => '(',
    LT: $ => '<',
    MAX: $ => '#max',
    MAXIMIZE: $ => '#maximize',
    MIN: $ => '#min',
    MINIMIZE: $ => '#minimize',
    MOD: $ => '\\',
    MUL: $ => '*',
    NEQ: $ => '!=',
    POW: $ => '**',
    QUESTION: $ => '?',
    RBRACE: $ => '}',
    RBRACK: $ => ']',
    RPAREN: $ => ')',
    SEM: $ => ';',
    SHOW: $ => '#show',
    EDGE: $ => '#edge',
    PROJECT: $ => '#project',
    HEURISTIC: $ => '#heuristic',
    SHOWSIG: $ => '#showsig',
    SLASH: $ => '/',
    SUB: $ => '-',
    SUM: $ => '#sum',
    SUMP: $ => '#sum+',
    SUPREMUM: $ => '#sup',
    TRUE: $ => '#true',
    BLOCK: $ => '#program',
//     UBNOT
//     UMINUS
    VBAR: $ => '|',
//     VOLATILE    "#volatile"
    WIF: $ => ':~',
    XOR: $ => '^',
//     PARSE_LP    "<program>"
//     PARSE_DEF   "<define>"
//     ANY         "any"
//     UNARY       "unary"
//     BINARY      "binary"
//     LEFT        "left"
//     RIGHT       "right"
//     HEAD        "head"
//     BODY        "body"
//     DIRECTIVE   "directive"
//     THEORY      "#theory"
//     SYNC        "EOF"


// %token <num>
//     NUMBER     "<NUMBER>"

// %token <str>
//     ANONYMOUS  "<ANONYMOUS>"
//     IDENTIFIER "<IDENTIFIER>"
    SCRIPT: $ => '<SCRIPT>',
    CODE: $ => '<CODE>',
//     STRING     "<STRING>"
//     VARIABLE   "<VARIABLE>"
//     THEORY_OP  "<THEORYOP>"
    NOT: $ => 'not',
    DEFAULT: $ => 'default',
    OVERRIDE: $ => 'override',



    //     constterm
    //     : constterm[a] XOR constterm[b]                    { $$ = BUILDER.term(@$, BinOp::XOR, $a, $b); }
    //     | constterm[a] QUESTION constterm[b]               { $$ = BUILDER.term(@$, BinOp::OR, $a, $b); }
    //     | constterm[a] AND constterm[b]                    { $$ = BUILDER.term(@$, BinOp::AND, $a, $b); }
    //     | constterm[a] ADD constterm[b]                    { $$ = BUILDER.term(@$, BinOp::ADD, $a, $b); }
    //     | constterm[a] SUB constterm[b]                    { $$ = BUILDER.term(@$, BinOp::SUB, $a, $b); }
    //     | constterm[a] MUL constterm[b]                    { $$ = BUILDER.term(@$, BinOp::MUL, $a, $b); }
    //     | constterm[a] SLASH constterm[b]                  { $$ = BUILDER.term(@$, BinOp::DIV, $a, $b); }
    //     | constterm[a] MOD constterm[b]                    { $$ = BUILDER.term(@$, BinOp::MOD, $a, $b); }
    //     | constterm[a] POW constterm[b]                    { $$ = BUILDER.term(@$, BinOp::POW, $a, $b); }
    //     | SUB constterm[a] %prec UMINUS                    { $$ = BUILDER.term(@$, UnOp::NEG, $a); }
    //     | BNOT constterm[a] %prec UBNOT                    { $$ = BUILDER.term(@$, UnOp::NOT, $a); }
    //     | LPAREN RPAREN                                    { $$ = BUILDER.term(@$, BUILDER.termvec(), false); }
    //     | LPAREN COMMA RPAREN                              { $$ = BUILDER.term(@$, BUILDER.termvec(), true); }
    //     | LPAREN consttermvec[a] RPAREN                    { $$ = BUILDER.term(@$, $a, false); }
    //     | LPAREN consttermvec[a] COMMA RPAREN              { $$ = BUILDER.term(@$, $a, true); }
    //     | identifier[a] LPAREN constargvec[b] RPAREN       { $$ = BUILDER.term(@$, String::fromRep($a), $b, false); }
    //     | AT[l] identifier[a] LPAREN constargvec[b] RPAREN { $$ = BUILDER.term(@$, String::fromRep($a), $b, true); }
    //     | VBAR[l] constterm[a] VBAR                        { $$ = BUILDER.term(@$, UnOp::ABS, $a); }
    //     | identifier[a]                                    { $$ = BUILDER.term(@$, Symbol::createId(String::fromRep($a))); }
    //     | AT[l] identifier[a]                              { $$ = BUILDER.term(@$, String::fromRep($a), BUILDER.termvecvec(BUILDER.termvecvec(), BUILDER.termvec()), true); }
    //     | NUMBER[a]                                        { $$ = BUILDER.term(@$, Symbol::createNum($a)); }
    //     | STRING[a]                                        { $$ = BUILDER.term(@$, Symbol::createStr(String::fromRep($a))); }
    //     | INFIMUM[a]                                       { $$ = BUILDER.term(@$, Symbol::createInf()); }
    //     | SUPREMUM[a]                                      { $$ = BUILDER.term(@$, Symbol::createSup()); }
    //     ;
    constterm: $ => choice(
      prec.left(7, seq( $.constterm, $.XOR, $.constterm)),
      prec.left(6, seq( $.constterm, $.QUESTION, $.constterm)),
      prec.left(5, seq( $.constterm, $.AND, $.constterm)),
      prec.left(4, seq( $.constterm, $.ADD, $.constterm)),
      prec.left(4, seq( $.constterm, $.SUB, $.constterm)),
      prec.left(3, seq( $.constterm, $.MUL, $.constterm)),
      prec.left(3, seq( $.constterm, $.SLASH, $.constterm)),
      prec.left(3, seq( $.constterm, $.MOD, $.constterm)),
      prec.right(2, seq( $.constterm, $.POW, $.constterm)),
      prec.left(1, seq($.SUB, $.constterm)),
      prec.left(1, seq($.BNOT, $.constterm)),
      seq($.LPAREN,              $.RPAREN),
      seq($.LPAREN, $.consttermvec, $.RPAREN),
      seq($.LPAREN, $.consttermvec, $.COMMA, $.RPAREN),
      seq($._widentifier,                 $.RPAREN),
      seq($._widentifier, $.nconstargvec, $.RPAREN),
      seq($.AT, $._widentifier,                 $.RPAREN),
      seq($.AT, $._widentifier, $.nconstargvec, $.RPAREN),
      seq($.VBAR, $.constterm, $.VBAR),
      $.identifier,
      seq($.AT, $.identifier,),
      $.NUMBER,
      $.STRING,
      $.INFIMUM,
      $.SUPREMUM,
    ),

    // consttermvec
    //     : constterm[a]                       { $$ = BUILDER.termvec(BUILDER.termvec(), $a);  }
    //     | consttermvec[a] COMMA constterm[b] { $$ = BUILDER.termvec($a, $b);  }
    //     ;
    consttermvec: $ => choice(
      $.constterm,
      seq($.consttermvec, $.COMMA, $.constterm),
    ),

    // constargvec
    //     : consttermvec[a] { $$ = BUILDER.termvecvec(BUILDER.termvecvec(), $a);  }
    //     |                 { $$ = BUILDER.termvecvec(BUILDER.termvecvec(), BUILDER.termvec());  }
    //     ;
    nconstargvec: $ =>
      $.consttermvec,

    // // {{{2 terms including variables

    // term
    //     : term[a] DOTS term[b]                     { $$ = BUILDER.term(@$, $a, $b); }
    //     | term[a] XOR term[b]                      { $$ = BUILDER.term(@$, BinOp::XOR, $a, $b); }
    //     | term[a] QUESTION term[b]                 { $$ = BUILDER.term(@$, BinOp::OR, $a, $b); }
    //     | term[a] AND term[b]                      { $$ = BUILDER.term(@$, BinOp::AND, $a, $b); }
    //     | term[a] ADD term[b]                      { $$ = BUILDER.term(@$, BinOp::ADD, $a, $b); }
    //     | term[a] SUB term[b]                      { $$ = BUILDER.term(@$, BinOp::SUB, $a, $b); }
    //     | term[a] MUL term[b]                      { $$ = BUILDER.term(@$, BinOp::MUL, $a, $b); }
    //     | term[a] SLASH term[b]                    { $$ = BUILDER.term(@$, BinOp::DIV, $a, $b); }
    //     | term[a] MOD term[b]                      { $$ = BUILDER.term(@$, BinOp::MOD, $a, $b); }
    //     | term[a] POW term[b]                      { $$ = BUILDER.term(@$, BinOp::POW, $a, $b); }
    //     | SUB term[a] %prec UMINUS                 { $$ = BUILDER.term(@$, UnOp::NEG, $a); }
    //     | BNOT term[a] %prec UBNOT                 { $$ = BUILDER.term(@$, UnOp::NOT, $a); }
    //     | LPAREN tuplevec[a] RPAREN                { $$ = BUILDER.pool(@$, $a); }
    //     | identifier[a] LPAREN argvec[b] RPAREN    { $$ = BUILDER.term(@$, String::fromRep($a), $b, false); }
    //     | AT identifier[a] LPAREN argvec[b] RPAREN { $$ = BUILDER.term(@$, String::fromRep($a), $b, true); }
    //     | VBAR unaryargvec[a] VBAR                 { $$ = BUILDER.term(@$, UnOp::ABS, $a); }
    //     | identifier[a]                            { $$ = BUILDER.term(@$, Symbol::createId(String::fromRep($a))); }
    //     | AT[l] identifier[a]                      { $$ = BUILDER.term(@$, String::fromRep($a), BUILDER.termvecvec(BUILDER.termvecvec(), BUILDER.termvec()), true); }
    //     | NUMBER[a]                                { $$ = BUILDER.term(@$, Symbol::createNum($a)); }
    //     | STRING[a]                                { $$ = BUILDER.term(@$, Symbol::createStr(String::fromRep($a))); }
    //     | INFIMUM[a]                               { $$ = BUILDER.term(@$, Symbol::createInf()); }
    //     | SUPREMUM[a]                              { $$ = BUILDER.term(@$, Symbol::createSup()); }
    //     | VARIABLE[a]                              { $$ = BUILDER.term(@$, String::fromRep($a)); }
    //     | ANONYMOUS[a]                             { $$ = BUILDER.term(@$, String("_")); }
    //     ;
    term: $ => choice(
      prec.left(8, seq($.term, $.DOTS, $.term)),
      prec.left(7, seq($.term, $.XOR, $.term)),
      prec.left(6, seq($.term, $.QUESTION, $.term)),
      prec.left(5, seq($.term, $.AND, $.term)),
      prec.left(4, seq($.term, $.ADD, $.term)),
      prec.left(4, seq($.term, $.SUB, $.term)),
      prec.left(3, seq($.term, $.MUL, $.term)),
      prec.left(3, seq($.term, $.SLASH, $.term)),
      prec.left(3, seq($.term, '\\\\', $.term)),
      prec.right(2, seq($.term, $.POW, $.term)),
      prec.left(1, seq($.SUB, $.term)),
      prec.left(1, seq($.BNOT, $.term)),
      seq($.LPAREN,              $.RPAREN),
      seq($.LPAREN, $.ntuplevec, $.RPAREN),
      seq($._widentifier, $.argvec, $.RPAREN),
      seq($.AT, $._widentifier, $.argvec, $.RPAREN),
      seq($.VBAR, $.unaryargvec, $.VBAR),
      $.identifier,
      seq($.AT, $.identifier,),
      $.NUMBER,
      $.STRING,
      $.INFIMUM,
      $.SUPREMUM,
      $.VARIABLE,
      $.ANONYMOUS,
    ),
    
    // unaryargvec
    //     : term[a]                    { $$ = BUILDER.termvec(BUILDER.termvec(), $a); }
    //     | unaryargvec[a] SEM term[b] { $$ = BUILDER.termvec($a, $b); }
    //     ;
    unaryargvec: $ => choice(
      $.term,
      seq($.unaryargvec, $.SEM, $.term)
    ),

    // ntermvec
    //     : term[a]                   { $$ = BUILDER.termvec(BUILDER.termvec(), $a); }
    //     | ntermvec[a] COMMA term[b] { $$ = BUILDER.termvec($a, $b); }
    //     ;
    ntermvec: $ => choice(
      $.term,
      seq($.ntermvec, $.COMMA, $.term)
    ),

    // termvec
    //     : ntermvec[a] { $$ = $a; }
    //     |             { $$ = BUILDER.termvec(); }
    //     ;

    // tuple
    //     : ntermvec[a] COMMA { $$ = BUILDER.term(@$, $a, true); }
    //     | ntermvec[a]       { $$ = BUILDER.term(@$, $a, false); }
    //     |             COMMA { $$ = BUILDER.term(@$, BUILDER.termvec(), true); }
    //     |                   { $$ = BUILDER.term(@$, BUILDER.termvec(), false); }
    ntuple: $ => choice(
      seq($.ntermvec, $.COMMA),
      $.ntermvec,
      $.COMMA
    ),

    // tuplevec_sem
    //     :                 tuple[b] SEM { $$ = BUILDER.termvec(BUILDER.termvec(), $b); }
    //     | tuplevec_sem[a] tuple[b] SEM { $$ = BUILDER.termvec($a, $b); }
    tuplevec_sem: $ => choice(
      seq(          $.SEM),
      seq($.ntuple, $.SEM),
      seq($.tuplevec_sem,           $.SEM),
      seq($.tuplevec_sem, $.ntuple, $.SEM),
    ),

    // tuplevec
    //     :                 tuple[b] { $$ = BUILDER.termvec(BUILDER.termvec(), $b); }
    //     | tuplevec_sem[a] tuple[b] { $$ = BUILDER.termvec($a, $b); }
    ntuplevec: $ => choice(
      seq(                $.ntuple),
      seq($.tuplevec_sem, $.ntuple),
    ),

    argvec: $ => choice(
      $.ntermvec,
      seq($.argvec, $.SEM, $.ntermvec,),
    ),

    // binaryargvec
    //     :                       term[a] COMMA term[b] { $$ = BUILDER.termvecvec(BUILDER.termvecvec(), BUILDER.termvec(BUILDER.termvec(BUILDER.termvec(), $a), $b)); }
    //     | binaryargvec[vec] SEM term[a] COMMA term[b] { $$ = BUILDER.termvecvec($vec, BUILDER.termvec(BUILDER.termvec(BUILDER.termvec(), $a), $b)); }
    //     ;
    binaryargvec: $ => choice(
      seq($.term, $.COMMA, $.term),
      seq($.binaryargvec, $.SEM, $.term, $.COMMA, $.term),
    ),

    // // TODO: I might have to create tuples differently
    // //       parse a tuple as a list of terms
    // //       each term is either a tuple or a term -> which afterwards is turned into a pool!

    // // {{{1 literals

    // cmp
    //     : GT     { $$ = Relation::GT; }
    //     | LT     { $$ = Relation::LT; }
    //     | GEQ    { $$ = Relation::GEQ; }
    //     | LEQ    { $$ = Relation::LEQ; }
    //     | EQ     { $$ = Relation::EQ; }
    //     | NEQ    { $$ = Relation::NEQ; }
    //     ;
    cmp: $ => choice(
      $.GT,
      $.LT,
      $.GEQ,
      $.LEQ,
      '==',//EQ
      $.EQ, //EQ
      $.NEQ
    ),

    // atom
    // : identifier[id]                                  { $$ = BUILDER.predRep(@$, false, String::fromRep($id), BUILDER.termvecvec(BUILDER.termvecvec(), BUILDER.termvec())); }
    // | identifier[id] LPAREN argvec[tvv] RPAREN[r]     { $$ = BUILDER.predRep(@$, false, String::fromRep($id), $tvv); }
    // | SUB identifier[id]                              { $$ = BUILDER.predRep(@$, true, String::fromRep($id), BUILDER.termvecvec(BUILDER.termvecvec(), BUILDER.termvec())); }
    // | SUB identifier[id] LPAREN argvec[tvv] RPAREN[r] { $$ = BUILDER.predRep(@$, true, String::fromRep($id), $tvv); }
    // ;
    atom: $ => choice(
      $.identifier,
      seq($._widentifier, $.RPAREN),
      seq($._widentifier, $.argvec, $.RPAREN),
      seq($.SUB, $.identifier),
      seq($.SUB, $._widentifier, $.RPAREN),
      seq($.SUB, $._widentifier, $.argvec, $.RPAREN),
    ),

    // literal
    // :         TRUE                     { $$ = BUILDER.boollit(@$, true); }
    // |     NOT TRUE                     { $$ = BUILDER.boollit(@$, false); }
    // | NOT NOT TRUE                     { $$ = BUILDER.boollit(@$, true); }
    // |         FALSE                    { $$ = BUILDER.boollit(@$, false); }
    // |     NOT FALSE                    { $$ = BUILDER.boollit(@$, true); }
    // | NOT NOT FALSE                    { $$ = BUILDER.boollit(@$, false); }
    // |         atom[a]                  { $$ = BUILDER.predlit(@$, NAF::POS, $a); }
    // |     NOT atom[a]                  { $$ = BUILDER.predlit(@$, NAF::NOT, $a); }
    // | NOT NOT atom[a]                  { $$ = BUILDER.predlit(@$, NAF::NOTNOT, $a); }
    // |         term[l] cmp[rel] term[r] { $$ = BUILDER.rellit(@$, $rel, $l, $r); }
    // |     NOT term[l] cmp[rel] term[r] { $$ = BUILDER.rellit(@$, neg($rel), $l, $r); }
    // | NOT NOT term[l] cmp[rel] term[r] { $$ = BUILDER.rellit(@$, $rel, $l, $r); }
    // | csp_literal[lit]                 { $$ = BUILDER.csplit($lit); }
    // ;
    literal: $ => choice(
      $.TRUE,
      seq($.NOT, $.TRUE),
      seq($.NOT, $.NOT, $.TRUE),
      $.FALSE,
      seq($.NOT, $.FALSE),
      seq($.NOT, $.NOT, $.FALSE),
      $.atom,
      seq($.NOT, $.atom),
      seq($.NOT, $.NOT, $.atom),
      seq($.term, $.cmp, $.term),
      seq($.NOT, $.term, $.cmp, $.term),
      seq($.NOT, $.NOT, $.term, $.cmp, $.term),
      $.csp_literal
    ),

    // csp_mul_term
    //     : CSP term[var] CSP_MUL term[coe] { $$ = BUILDER.cspmulterm(@$, $coe,                     $var); }
    //     | term[coe] CSP_MUL CSP term[var] { $$ = BUILDER.cspmulterm(@$, $coe,                     $var); }
    //     | CSP term[var]                   { $$ = BUILDER.cspmulterm(@$, BUILDER.term(@$, Symbol::createNum(1)), $var); }
    //     | term[coe]                       { $$ = BUILDER.cspmulterm(@$, $coe); }
    //     ;
    csp_mul_term: $ => choice(
      seq($.CSP, $.term, $.CSP_MUL, $.term),
      seq($.term, $.CSP_MUL, $.CSP, $.term),
      seq($.CSP, $.term),
      $.term
    ),

    // csp_add_term
    //     : csp_add_term[add] CSP_ADD csp_mul_term[mul] { $$ = BUILDER.cspaddterm(@$, $add, $mul, true); }
    //     | csp_add_term[add] CSP_SUB csp_mul_term[mul] { $$ = BUILDER.cspaddterm(@$, $add, $mul, false); }
    //     | csp_mul_term[mul]                           { $$ = BUILDER.cspaddterm(@$, $mul); }
    //     ;
    csp_add_term: $ => choice(
      seq($.csp_add_term, $.CSP_ADD, $.csp_mul_term),
      seq($.csp_add_term, $.CSP_SUB, $.csp_mul_term),
      $.csp_mul_term
    ),

    // csp_rel
    //     : CSP_GT  { $$ = Relation::GT; }
    //     | CSP_LT  { $$ = Relation::LT; }
    //     | CSP_GEQ { $$ = Relation::GEQ; }
    //     | CSP_LEQ { $$ = Relation::LEQ; }
    //     | CSP_EQ  { $$ = Relation::EQ; }
    //     | CSP_NEQ { $$ = Relation::NEQ; }
    //     ;
    csp_rel: $ => choice(
      $.CSP_GT,
      $.CSP_LT,
      $.CSP_GEQ,
      $.CSP_LEQ,
      $.CSP_EQ,
      $.CSP_NEQ
    ),

    // csp_literal 
    //     : csp_literal[lit] csp_rel[rel] csp_add_term[b] { $$ = BUILDER.csplit(@$, $lit, $rel, $b); }
    //     | csp_add_term[a]  csp_rel[rel] csp_add_term[b] { $$ = BUILDER.csplit(@$, $a,   $rel, $b); }
    //     ;
    csp_literal: $ => choice(
      seq($.csp_literal, $.csp_rel, $.csp_add_term),
      seq($.csp_add_term, $.csp_rel, $.csp_add_term),
    ),

    // nlitvec
    //     : literal[lit]                    { $$ = BUILDER.litvec(BUILDER.litvec(), $lit); }
    //     | nlitvec[vec] COMMA literal[lit] { $$ = BUILDER.litvec($vec, $lit); }
    //     ;
    nlitvec: $ => choice(
      $.literal,
      seq($.nlitvec, $.COMMA, $.literal)
    ),

    // litvec
    //     : nlitvec[vec] { $$ = $vec; }
    //     |              { $$ = BUILDER.litvec(); }
    //     ;

    // optcondition
    //     : COLON litvec[vec] { $$ = $vec; }
    //     |                   { $$ = BUILDER.litvec(); }
    //     ;
    noptcondition: $ => choice(
      seq($.COLON           ),
      seq($.COLON, $.nlitvec),
    ),

    // aggregatefunction
    //     : SUM   { $$ = AggregateFunction::SUM; }
    //     | SUMP  { $$ = AggregateFunction::SUMP; }
    //     | MIN   { $$ = AggregateFunction::MIN; }
    //     | MAX   { $$ = AggregateFunction::MAX; }
    //     | COUNT { $$ = AggregateFunction::COUNT; }
    //     ;
    aggregatefunction: $ => choice(
      $.SUM,
      $.SUMP,
      $.MIN,
      $.MAX,
      $.COUNT
    ),

    // bodyaggrelem
    //     : COLON litvec[cond]                { $$ = { BUILDER.termvec(), $cond }; }
    //     | ntermvec[args] optcondition[cond] { $$ = { $args, $cond }; }
    //     ;
    bodyaggrelem: $ => choice(
      seq($.COLON,          ), 
      seq($.COLON, $.nlitvec),
      seq($.ntermvec,                ),
      seq($.ntermvec, $.noptcondition),
    ),

    // bodyaggrelemvec
    //     : bodyaggrelem[elem]                          { $$ = BUILDER.bodyaggrelemvec(BUILDER.bodyaggrelemvec(), $elem.first, $elem.second); }
    //     | bodyaggrelemvec[vec] SEM bodyaggrelem[elem] { $$ = BUILDER.bodyaggrelemvec($vec, $elem.first, $elem.second); }
    //     ;
    bodyaggrelemvec: $ => choice(
      $.bodyaggrelem,
      seq($.bodyaggrelemvec, $.SEM, $.bodyaggrelem),
    ),

    // // Note: alternative syntax (without weight)

    // altbodyaggrelem
    //     : literal[lit] optcondition[cond] { $$ = { $lit, $cond }; }
    //     ;
    altbodyaggrelem: $ => choice(
      seq($.literal,                ),
      seq($.literal, $.noptcondition),
    ),

    // altbodyaggrelemvec
    //     : altbodyaggrelem[elem]                             { $$ = BUILDER.condlitvec(BUILDER.condlitvec(), $elem.first, $elem.second); }
    //     | altbodyaggrelemvec[vec] SEM altbodyaggrelem[elem] { $$ = BUILDER.condlitvec($vec, $elem.first, $elem.second); }
    //     ;
    altbodyaggrelemvec: $ => choice(
      $.altbodyaggrelem,
      seq($.altbodyaggrelemvec, $.SEM),
      seq($.altbodyaggrelemvec, $.SEM, $.altbodyaggrelem),
    ),

    // bodyaggregate
    //     : LBRACE RBRACE                                               { $$ = { AggregateFunction::COUNT, true, BUILDER.condlitvec() }; }
    //     | LBRACE altbodyaggrelemvec[elems] RBRACE                     { $$ = { AggregateFunction::COUNT, true, $elems }; }
    //     | aggregatefunction[fun] LBRACE RBRACE                        { $$ = { $fun, false, BUILDER.bodyaggrelemvec() }; }
    //     | aggregatefunction[fun] LBRACE bodyaggrelemvec[elems] RBRACE { $$ = { $fun, false, $elems }; }
    //     ;
    bodyaggregate: $ => choice(
      seq($.LBRACE, $.RBRACE),
      seq($.LBRACE, $.altbodyaggrelemvec, $.RBRACE),
      seq($.aggregatefunction, $.LBRACE, $.RBRACE),
      seq($.aggregatefunction, $.LBRACE, $.bodyaggrelemvec, $.RBRACE),
    ),

    // upper
    //     : term[t]          { $$ = { Relation::LEQ, $t }; }
    //     | cmp[rel] term[t] { $$ = { $rel, $t }; }
    //     |                  { $$ = { Relation::LEQ, TermUid(-1) }; }
    //     ;
    nupper: $ => choice(
      $.term,
      seq($.cmp, $.term),
    ),

    // lubodyaggregate
    //     : term[l]          bodyaggregate[a] upper[u] { $$ = lexer->aggregate($a.fun, $a.choice, $a.elems, lexer->boundvec(Relation::LEQ, $l, $u.rel, $u.term)); }
    //     | term[l] cmp[rel] bodyaggregate[a] upper[u] { $$ = lexer->aggregate($a.fun, $a.choice, $a.elems, lexer->boundvec($rel, $l, $u.rel, $u.term)); }
    //     |                  bodyaggregate[a] upper[u] { $$ = lexer->aggregate($a.fun, $a.choice, $a.elems, lexer->boundvec(Relation::LEQ, TermUid(-1), $u.rel, $u.term)); }
    //     | theory_atom[atom]                          { $$ = lexer->aggregate($atom); }
    //     ;
    lubodyaggregate: $ => choice(
      seq($.term, $.bodyaggregate          ),
      seq($.term, $.bodyaggregate, $.nupper),
      seq($.term, $.cmp, $.bodyaggregate          ),
      seq($.term, $.cmp, $.bodyaggregate, $.nupper),
      seq($.bodyaggregate          ),
      seq($.bodyaggregate, $.nupper),
      $.theory_atom
    ),

    // headaggrelemvec
    //     : headaggrelemvec[vec] SEM termvec[tuple] COLON literal[head] optcondition[cond] { $$ = BUILDER.headaggrelemvec($vec, $tuple, $head, $cond); }
    //     | termvec[tuple] COLON literal[head] optcondition[cond]                          { $$ = BUILDER.headaggrelemvec(BUILDER.headaggrelemvec(), $tuple, $head, $cond); }
    //     ;
    headaggrelemvec: $ => choice(
      seq($.headaggrelemvec, $.SEM,             $.COLON, $.literal,                ),
      seq($.headaggrelemvec, $.SEM, $.ntermvec, $.COLON, $.literal,                ),
      seq($.headaggrelemvec, $.SEM,             $.COLON, $.literal, $.noptcondition),
      seq($.headaggrelemvec, $.SEM, $.ntermvec, $.COLON, $.literal, $.noptcondition),
      seq(            $.COLON, $.literal,                ),
      seq($.ntermvec, $.COLON, $.literal,                ),
      seq(            $.COLON, $.literal, $.noptcondition),
      seq($.ntermvec, $.COLON, $.literal, $.noptcondition),
    ),

    // altheadaggrelemvec
    //     : literal[lit] optcondition[cond]                             { $$ = BUILDER.condlitvec(BUILDER.condlitvec(), $lit, $cond); }
    //     | altheadaggrelemvec[vec] SEM literal[lit] optcondition[cond] { $$ = BUILDER.condlitvec($vec, $lit, $cond); }
    //     ;
    altheadaggrelemvec: $ => choice(
      seq($.literal,                ),
      seq($.literal, $.noptcondition),
      seq($.altheadaggrelemvec, $.SEM, $.literal,                ),
      seq($.altheadaggrelemvec, $.SEM, $.literal, $.noptcondition),
    ),

    // headaggregate
    //     : aggregatefunction[fun] LBRACE RBRACE                        { $$ = { $fun, false, BUILDER.headaggrelemvec() }; }
    //     | aggregatefunction[fun] LBRACE headaggrelemvec[elems] RBRACE { $$ = { $fun, false, $elems }; }
    //     | LBRACE RBRACE                                               { $$ = { AggregateFunction::COUNT, true, BUILDER.condlitvec()}; }
    //     | LBRACE altheadaggrelemvec[elems] RBRACE                     { $$ = { AggregateFunction::COUNT, true, $elems}; }
    //     ;
    headaggregate: $ => choice(
      seq($.aggregatefunction, $.LBRACE, $.RBRACE),
      seq($.aggregatefunction, $.LBRACE, $.headaggrelemvec, $.RBRACE),
      seq($.LBRACE, $.RBRACE),
      seq($.LBRACE, $.altheadaggrelemvec, $.RBRACE),
    ),

    // luheadaggregate
    //     : term[l]          headaggregate[a] upper[u] { $$ = lexer->aggregate($a.fun, $a.choice, $a.elems, lexer->boundvec(Relation::LEQ, $l, $u.rel, $u.term)); }
    //     | term[l] cmp[rel] headaggregate[a] upper[u] { $$ = lexer->aggregate($a.fun, $a.choice, $a.elems, lexer->boundvec($rel, $l, $u.rel, $u.term)); }
    //     |                  headaggregate[a] upper[u] { $$ = lexer->aggregate($a.fun, $a.choice, $a.elems, lexer->boundvec(Relation::LEQ, TermUid(-1), $u.rel, $u.term)); }
    //     | theory_atom[atom]                          { $$ = lexer->aggregate($atom); }
    //     ;
    luheadaggregate: $ => choice(
      seq($.term, $.headaggregate          ),
      seq($.term, $.headaggregate, $.nupper),
      seq($.term, $.cmp, $.headaggregate          ),
      seq($.term, $.cmp, $.headaggregate, $.nupper),
      seq($.headaggregate          ),
      seq($.headaggregate, $.nupper),
      $.theory_atom,
    ),

    // ncspelemvec
    //     :                     termvec[tuple] COLON csp_add_term[add] optcondition[cond] { $$ = BUILDER.cspelemvec(BUILDER.cspelemvec(), @$, $tuple, $add, $cond); }
    //     | cspelemvec[vec] SEM termvec[tuple] COLON csp_add_term[add] optcondition[cond] { $$ = BUILDER.cspelemvec($vec, @$, $tuple, $add, $cond); }
    //     ;
    ncspelemvec: $ => choice(
      seq(                                $.COLON, $.csp_add_term,                ),
      seq(                                $.COLON, $.csp_add_term, $.noptcondition),
      seq(                    $.ntermvec, $.COLON, $.csp_add_term,                ),
      seq(                    $.ntermvec, $.COLON, $.csp_add_term, $.noptcondition),
      seq( $.ncspelemvec,$.SEM,             $.COLON, $.csp_add_term,                ),
      seq( $.ncspelemvec,$.SEM,             $.COLON, $.csp_add_term, $.noptcondition),
      seq( $.ncspelemvec,$.SEM, $.ntermvec, $.COLON, $.csp_add_term,                ),
      seq( $.ncspelemvec,$.SEM, $.ntermvec, $.COLON, $.csp_add_term, $.noptcondition),
    ),

    // cspelemvec
    //     : ncspelemvec[vec] { $$ = $vec; }
    //     |                  { $$ = BUILDER.cspelemvec(); }
    //     ;
    

    // disjoint
    //     :         DISJOINT LBRACE cspelemvec[elems] RBRACE { $$ = { NAF::POS, $elems }; }
    //     | NOT     DISJOINT LBRACE cspelemvec[elems] RBRACE { $$ = { NAF::NOT, $elems }; }
    //     | NOT NOT DISJOINT LBRACE cspelemvec[elems] RBRACE { $$ = { NAF::NOTNOT, $elems }; }
    //     ;
    disjoint: $ => choice(
      seq(            $.DISJOINT, $.LBRACE,                $.RBRACE),
      seq(            $.DISJOINT, $.LBRACE, $.ncspelemvec, $.RBRACE),
      seq(      $.NOT, $.DISJOINT, $.LBRACE,                $.RBRACE),
      seq(      $.NOT, $.DISJOINT, $.LBRACE, $.ncspelemvec, $.RBRACE),
      seq($.NOT, $.NOT, $.DISJOINT, $.LBRACE,                $.RBRACE),
      seq($.NOT, $.NOT, $.DISJOINT, $.LBRACE, $.ncspelemvec, $.RBRACE),
    ),


    // conjunction
    //     : literal[lit] COLON litvec[cond] { $$ = { $lit, $cond }; }
    //     ;
    conjunction: $ => choice(
      seq($.literal, $.COLON,          ),
      seq($.literal, $.COLON, $.nlitvec)
    ),

    // dsym
    //     : SEM
    //     | VBAR
    //     ;
    dsym: $ => choice(
      $.SEM,
      $.VBAR
    ),

    // // NOTE: this is so complicated because VBAR is also used as the absolute function for terms
    // //       due to limited lookahead I found no reasonable way to parse p(X):|q(X)
    // disjunctionsep
    //     : disjunctionsep[vec] literal[lit] COMMA                    { $$ = BUILDER.condlitvec($vec, $lit, BUILDER.litvec()); }
    //     | disjunctionsep[vec] literal[lit] dsym                     { $$ = BUILDER.condlitvec($vec, $lit, BUILDER.litvec()); }
    //     | disjunctionsep[vec] literal[lit] COLON SEM                { $$ = BUILDER.condlitvec($vec, $lit, BUILDER.litvec()); }
    //     | disjunctionsep[vec] literal[lit] COLON nlitvec[cond] dsym { $$ = BUILDER.condlitvec($vec, $lit, $cond); }
    //     | literal[lit] COMMA                                        { $$ = BUILDER.condlitvec(BUILDER.condlitvec(), $lit, BUILDER.litvec()); }
    //     | literal[lit] dsym                                         { $$ = BUILDER.condlitvec(BUILDER.condlitvec(), $lit, BUILDER.litvec()); }
    //     | literal[lit] COLON nlitvec[cond] dsym                     { $$ = BUILDER.condlitvec(BUILDER.condlitvec(), $lit, $cond); }
    //     | literal[lit] COLON SEM                                    { $$ = BUILDER.condlitvec(BUILDER.condlitvec(), $lit, BUILDER.litvec()); }
    //     ;
    disjunctionsep: $ => choice(
      seq($.disjunctionsep, $.literal, $.COMMA),
      seq($.disjunctionsep, $.literal, $.dsym),
      seq($.disjunctionsep, $.literal, $.COLON, $.SEM),
      seq($.disjunctionsep, $.literal, $.COLON, $.nlitvec, $.dsym),
      seq($.literal, $.COMMA),
      seq($.literal, $.dsym),
      seq($.literal, $.COLON, $.nlitvec, $.dsym),
      seq($.literal, $.COLON, $.SEM),
    ),

    // disjunction
    //     : disjunctionsep[vec] literal[lit] optcondition[cond] { $$ = BUILDER.condlitvec($vec, $lit, $cond); }
    //     | literal[lit] COLON litvec[cond]                     { $$ = BUILDER.condlitvec(BUILDER.condlitvec(), $lit, $cond); }
    //     ;
    disjunction: $ => choice(
      seq($.disjunctionsep, $.literal                 ),
      seq($.disjunctionsep, $.literal, $.noptcondition),
      seq($.literal, $.COLON           ),
      seq($.literal, $.COLON, $.nlitvec)
    ),

    // bodycomma
    //     : bodycomma[body] literal[lit] COMMA                      { $$ = BUILDER.bodylit($body, $lit); }
    //     | bodycomma[body] literal[lit] SEM                        { $$ = BUILDER.bodylit($body, $lit); }
    //     | bodycomma[body] lubodyaggregate[aggr] COMMA             { $$ = lexer->bodyaggregate($body, @aggr, NAF::POS, $aggr); }
    //     | bodycomma[body] lubodyaggregate[aggr] SEM               { $$ = lexer->bodyaggregate($body, @aggr, NAF::POS, $aggr); }
    //     | bodycomma[body] NOT[l] lubodyaggregate[aggr] COMMA      { $$ = lexer->bodyaggregate($body, @aggr + @l, NAF::NOT, $aggr); }
    //     | bodycomma[body] NOT[l] lubodyaggregate[aggr] SEM        { $$ = lexer->bodyaggregate($body, @aggr + @l, NAF::NOT, $aggr); }
    //     | bodycomma[body] NOT[l] NOT lubodyaggregate[aggr] COMMA  { $$ = lexer->bodyaggregate($body, @aggr + @l, NAF::NOTNOT, $aggr); }
    //     | bodycomma[body] NOT[l] NOT lubodyaggregate[aggr] SEM    { $$ = lexer->bodyaggregate($body, @aggr + @l, NAF::NOTNOT, $aggr); }
    //     | bodycomma[body] conjunction[conj] SEM                   { $$ = BUILDER.conjunction($body, @conj, $conj.first, $conj.second); }
    //     | bodycomma[body] disjoint[cons] SEM                      { $$ = BUILDER.disjoint($body, @cons, $cons.first, $cons.second); }
    //     |                                                         { $$ = BUILDER.body(); }
    //     ;
    nbodycomma: $ => choice(
      seq(              $.literal, $.COMMA),
      seq($.nbodycomma, $.literal, $.COMMA),
      seq(              $.literal, $.SEM),
      seq($.nbodycomma, $.literal, $.SEM),
      seq(              $.lubodyaggregate, $.COMMA),
      seq($.nbodycomma, $.lubodyaggregate, $.COMMA),
      seq(              $.lubodyaggregate, $.SEM),
      seq($.nbodycomma, $.lubodyaggregate, $.SEM),
      seq(              $.NOT, $.lubodyaggregate, $.COMMA),
      seq($.nbodycomma, $.NOT, $.lubodyaggregate, $.COMMA),
      seq(              $.NOT, $.lubodyaggregate, $.SEM),
      seq($.nbodycomma, $.NOT, $.lubodyaggregate, $.SEM),
      seq(              $.NOT, $.NOT, $.lubodyaggregate, $.COMMA),
      seq($.nbodycomma, $.NOT, $.NOT, $.lubodyaggregate, $.COMMA),
      seq(              $.NOT, $.NOT, $.lubodyaggregate, $.SEM),
      seq($.nbodycomma, $.NOT, $.NOT, $.lubodyaggregate, $.SEM),
      seq(              $.conjunction, $.SEM),
      seq($.nbodycomma, $.conjunction, $.SEM),
      seq(              $.disjoint, $.SEM),
      seq($.nbodycomma, $.disjoint, $.SEM),
    ),

    // bodydot
    //     : bodycomma[body] literal[lit] DOT                      { $$ = BUILDER.bodylit($body, $lit); }
    //     | bodycomma[body] lubodyaggregate[aggr] DOT             { $$ = lexer->bodyaggregate($body, @aggr, NAF::POS, $aggr); }
    //     | bodycomma[body] NOT[l] lubodyaggregate[aggr] DOT      { $$ = lexer->bodyaggregate($body, @aggr + @l, NAF::NOT, $aggr); }
    //     | bodycomma[body] NOT[l] NOT lubodyaggregate[aggr] DOT  { $$ = lexer->bodyaggregate($body, @aggr + @l, NAF::NOTNOT, $aggr); }
    //     | bodycomma[body] conjunction[conj] DOT                 { $$ = BUILDER.conjunction($body, @conj, $conj.first, $conj.second); }
    //     | bodycomma[body] disjoint[cons] DOT                    { $$ = BUILDER.disjoint($body, @cons, $cons.first, $cons.second); }
    //     ;
    bodydot: $ => choice(
      seq(              $.literal, $.DOT),
      seq($.nbodycomma, $.literal, $.DOT),
      seq(              $.lubodyaggregate, $.DOT),
      seq($.nbodycomma, $.lubodyaggregate, $.DOT),
      seq(              $.NOT, $.lubodyaggregate, $.DOT),
      seq($.nbodycomma, $.NOT, $.lubodyaggregate, $.DOT),
      seq(              $.NOT, $.NOT, $.lubodyaggregate, $.DOT),
      seq($.nbodycomma, $.NOT, $.NOT, $.lubodyaggregate, $.DOT),
      seq(              $.conjunction, $.DOT),
      seq($.nbodycomma, $.conjunction, $.DOT),
      seq(              $.disjoint, $.DOT),
      seq($.nbodycomma, $.disjoint, $.DOT),
    ),

    // bodyconddot
    //     : DOT             { $$ = BUILDER.body(); }
    //     | COLON DOT       { $$ = BUILDER.body(); }
    //     | COLON bodydot[body]   { $$ = $body; }
    bodyconddot: $ => choice(
      $.DOT,
      seq($.COLON, $.DOT),
      seq($.COLON, $.bodydot),
    ),

    // head
    //     : literal[lit]            { $$ = BUILDER.headlit($lit); }
    //     | disjunction[elems]      { $$ = BUILDER.disjunction(@$, $elems); }
    //     | luheadaggregate[aggr]   { $$ = lexer->headaggregate(@$, $aggr); }
    //     ;
    head: $ => choice(
      $.literal,
      $.disjunction,
      $.luheadaggregate
    ),

    // statement
    //     : head[hd] DOT            { BUILDER.rule(@$, $hd); }
    //     | head[hd] IF DOT         { BUILDER.rule(@$, $hd); }
    //     | head[hd] IF bodydot[bd] { BUILDER.rule(@$, $hd, $bd); }
    //     | IF bodydot[bd]          { BUILDER.rule(@$, BUILDER.headlit(BUILDER.boollit(@$, false)), $bd); }
    //     | IF DOT                  { BUILDER.rule(@$, BUILDER.headlit(BUILDER.boollit(@$, false)), BUILDER.body()); }
    //     ;
    // statement
    //     : disjoint[hd] IF bodydot[body] { BUILDER.rule(@$, BUILDER.headlit(BUILDER.boollit(@hd, false)), BUILDER.disjoint($body, @hd, inv($hd.first), $hd.second)); }
    //     | disjoint[hd] IF DOT           { BUILDER.rule(@$, BUILDER.headlit(BUILDER.boollit(@hd, false)), BUILDER.disjoint(BUILDER.body(), @hd, inv($hd.first), $hd.second)); }
    //     | disjoint[hd] DOT              { BUILDER.rule(@$, BUILDER.headlit(BUILDER.boollit(@hd, false)), BUILDER.disjoint(BUILDER.body(), @hd, inv($hd.first), $hd.second)); }
    //     ;
    // statement
    //     : WIF bodydot[bd] LBRACK optimizeweight[w] optimizetuple[t] RBRACK { BUILDER.optimize(@$, $w.first, $w.second, $t, $bd); }
    //     | WIF         DOT LBRACK optimizeweight[w] optimizetuple[t] RBRACK { BUILDER.optimize(@$, $w.first, $w.second, $t, BUILDER.body()); }
    //     ;
    // statement
    //     : MINIMIZE LBRACE RBRACE DOT
    //     | MAXIMIZE LBRACE RBRACE DOT
    //     | MINIMIZE LBRACE minelemlist RBRACE DOT
    //     | MAXIMIZE LBRACE maxelemlist RBRACE DOT
    //     ;
    // statement
    //     : SHOWSIG identifier[id] SLASH NUMBER[num] DOT     { BUILDER.showsig(@$, Sig(String::fromRep($id), $num, false), false); }
    //     | SHOWSIG SUB identifier[id] SLASH NUMBER[num] DOT { BUILDER.showsig(@$, Sig(String::fromRep($id), $num, true), false); }
    //     | SHOW DOT                                         { BUILDER.showsig(@$, Sig("", 0, false), false); }
    //     | SHOW term[t] COLON bodydot[bd]                   { BUILDER.show(@$, $t, $bd, false); }
    //     | SHOW term[t] DOT                                 { BUILDER.show(@$, $t, BUILDER.body(), false); }
    //     | SHOWSIG CSP identifier[id] SLASH NUMBER[num] DOT { BUILDER.showsig(@$, Sig(String::fromRep($id), $num, false), true); }
    //     | SHOW CSP term[t] COLON bodydot[bd]               { BUILDER.show(@$, $t, $bd, true); }
    //     | SHOW CSP term[t] DOT                             { BUILDER.show(@$, $t, BUILDER.body(), true); }
    //     ;
    // statement
    //     : DEFINED identifier[id] SLASH NUMBER[num] DOT     { BUILDER.defined(@$, Sig(String::fromRep($id), $num, false)); }
    //     | DEFINED SUB identifier[id] SLASH NUMBER[num] DOT { BUILDER.defined(@$, Sig(String::fromRep($id), $num, true)); }
    // statement
    //     : EDGE LPAREN binaryargvec[args] RPAREN bodyconddot[body] { BUILDER.edge(@$, $args, $body); }
    //     ;
    // statement
    //     : HEURISTIC atom[a] bodyconddot[body] LBRACK term[t] AT term[p] COMMA term[mod] RBRACK { BUILDER.heuristic(@$, $a, $body, $t, $p, $mod); }
    //     | HEURISTIC atom[a] bodyconddot[body] LBRACK term[t]            COMMA term[mod] RBRACK { BUILDER.heuristic(@$, $a, $body, $t, BUILDER.term(@$, Symbol::createNum(0)), $mod); }
    //     ;
    // statement
    //     : PROJECT identifier[name] SLASH NUMBER[arity] DOT     { BUILDER.project(@$, Sig(String::fromRep($name), $arity, false)); }
    //     | PROJECT SUB identifier[name] SLASH NUMBER[arity] DOT { BUILDER.project(@$, Sig(String::fromRep($name), $arity, true)); }
    //     | PROJECT atom[a] bodyconddot[body]                    { BUILDER.project(@$, $a, $body); }
    //     ;
    // statement
    //     : CONST identifier[uid] EQ constterm[rhs] DOT                        { BUILDER.define(@$, String::fromRep($uid), $rhs, true, LOGGER); }
    //     | CONST identifier[uid] EQ constterm[rhs] DOT LBRACK DEFAULT  RBRACK { BUILDER.define(@$, String::fromRep($uid), $rhs, true, LOGGER); }
    //     | CONST identifier[uid] EQ constterm[rhs] DOT LBRACK OVERRIDE RBRACK { BUILDER.define(@$, String::fromRep($uid), $rhs, false, LOGGER); }
    //     ;
    // statement
    //     : SCRIPT LPAREN IDENTIFIER[type] RPAREN CODE[code] DOT { BUILDER.script(@$, String::fromRep($type), String::fromRep($code)); }
    //     ;
    // statement
    //     : INCLUDE    STRING[file]        DOT { lexer->include(String::fromRep($file), @$, false, LOGGER); }
    //     | INCLUDE LT identifier[file] GT DOT { lexer->include(String::fromRep($file), @$, true, LOGGER); }
    //     ;
    // statement
    //     : BLOCK identifier[name] LPAREN idlist[args] RPAREN DOT { BUILDER.block(@$, String::fromRep($name), $args); }
    //     | BLOCK identifier[name] DOT                            { BUILDER.block(@$, String::fromRep($name), BUILDER.idvec()); }
    //     ;
    // statement
    //     : EXTERNAL atom[hd] COLON bodydot[bd]                       { BUILDER.external(@$, $hd, $bd, BUILDER.term(@$, Symbol::createId("false"))); }
    //     | EXTERNAL atom[hd] COLON DOT                               { BUILDER.external(@$, $hd, BUILDER.body(), BUILDER.term(@$, Symbol::createId("false"))); }
    //     | EXTERNAL atom[hd] DOT                                     { BUILDER.external(@$, $hd, BUILDER.body(), BUILDER.term(@$, Symbol::createId("false"))); }
    //     | EXTERNAL atom[hd] COLON bodydot[bd] LBRACK term[t] RBRACK { BUILDER.external(@$, $hd, $bd, $t); }
    //     | EXTERNAL atom[hd] COLON DOT         LBRACK term[t] RBRACK { BUILDER.external(@$, $hd, BUILDER.body(), $t); }
    //     | EXTERNAL atom[hd] DOT               LBRACK term[t] RBRACK { BUILDER.external(@$, $hd, BUILDER.body(), $t); }
    //     ;
    statement: $ => choice(
      seq($.head, $.DOT),
      seq($.head, $.IF, $.DOT),
      seq($.head, $.IF, $.bodydot),
      seq($.IF, $.bodydot),
      seq($.IF, $.DOT),
      seq($.disjoint, $.IF, $.bodydot),
      seq($.disjoint, $.IF, $.DOT),
      seq($.disjoint, $.DOT),
      seq($.WIF, $.bodydot, $.LBRACK, $.optimizeweight,                   $.RBRACK),
      seq($.WIF, $.bodydot, $.LBRACK, $.optimizeweight, $.noptimizetuple, $.RBRACK),
      seq($.MINIMIZE,$.LBRACE,$.RBRACE, $.DOT),
      seq($.MAXIMIZE,$.LBRACE,$.RBRACE, $.DOT),
      seq($.MINIMIZE,$.LBRACE, $.minelemlist, $.RBRACE, $.DOT),
      seq($.MAXIMIZE, $.maxelemlist, $.RBRACE, $.DOT),
      seq($.SHOWSIG, $.identifier, $.SLASH, $.NUMBER ,$.DOT),
      seq($.SHOWSIG, $.SUB, $.identifier, $.SLASH, $.NUMBER ,$.DOT),
      seq($.SHOW,$.DOT),
      seq($.SHOW, $.term, $.COLON, $.bodydot),
      seq($.SHOW, $.term, $.DOT),
      seq($.SHOWSIG, $.CSP, $.identifier, $.SLASH, $.NUMBER ,$.DOT),
      seq($.SHOW, $.CSP, $.term, $.COLON, $.bodydot),
      seq($.SHOW, $.CSP, $.term, $.DOT),
      seq($.DEFINED, $.identifier, $.SLASH, $.NUMBER, $.DOT),
      seq($.DEFINED, $.SUB, $.identifier, $.SLASH, $.NUMBER, $.DOT),
      seq($.EDGE, $.LPAREN, $.binaryargvec, $.RPAREN, $.bodyconddot),
      seq($.HEURISTIC, $.atom, $.bodyconddot, $.LBRACK, $.term, $.AT, $.term, $.COMMA, $.term, $.RBRACK),
      seq($.HEURISTIC, $.atom, $.bodyconddot, $.LBRACK, $.term, $.COMMA, $.term, $.RBRACK),
      seq($.PROJECT, $.identifier, $.SLASH, $.NUMBER, $.DOT),
      seq($.PROJECT, $.SUB, $.identifier, $.SLASH, $.NUMBER, $.DOT),
      seq($.PROJECT, $.atom, $.bodyconddot),
      seq($.CONST, $.identifier, $.EQ, $.constterm, $.DOT),
      seq($.CONST, $.identifier, $.EQ, $.constterm, $.DOT, $.LBRACK, $.DEFAULT, $.RBRACK),
      seq($.CONST, $.identifier, $.EQ, $.constterm, $.DOT, $.LBRACK, $.OVERRIDE, $.RBRACK),
      seq($.SCRIPT, $.LPAREN, $.identifier, $.RPAREN, $.CODE, $.DOT),
      seq($.INCLUDE, $.STRING, $.DOT),
      seq($.INCLUDE, $.LT, $.identifier, $.GT, $.DOT),
      seq($.BLOCK, $.identifier, $.LPAREN,            $.RPAREN, $.DOT),
      seq($.BLOCK, $.identifier, $.LPAREN, $.nidlist, $.RPAREN, $.DOT),
      seq($.BLOCK, $.identifier, $.DOT),
      seq($.EXTERNAL, $.atom, $.COLON, $.bodydot),
      seq($.EXTERNAL, $.atom, $.COLON, $.DOT),
      seq($.EXTERNAL, $.atom, $.DOT),
      seq($.EXTERNAL, $.atom, $.COLON, $.bodydot, $.LBRACK, $.term, $.RBRACK),
      seq($.EXTERNAL, $.atom, $.COLON, $.DOT, $.LBRACK, $.term, $.RBRACK),
      seq($.EXTERNAL, $.atom, $.DOT, $.LBRACK, $.term, $.RBRACK),
    ),

    // optimizetuple
    //     : COMMA ntermvec[vec] { $$ = $vec; }
    //     |                     { $$ = BUILDER.termvec(); }
    //     ;
    noptimizetuple: $ =>
      seq($.COMMA, $.ntermvec),

    // optimizeweight
    //     : term[w] AT term[p] { $$ = {$w, $p}; }
    //     | term[w]            { $$ = {$w, BUILDER.term(@$, Symbol::createNum(0))}; }
    //     ;
    optimizeweight: $ => choice(
      seq($.term, $.AT, $.term),
      $.term
    ),

    // optimizelitvec
    //     : literal[lit]                          { $$ = BUILDER.bodylit(BUILDER.body(), $lit); }
    //     | optimizelitvec[bd] COMMA literal[lit] { $$ = BUILDER.bodylit($bd, $lit); }
    //     ;
    optimizelitvec: $ => choice(
      $.literal,
      seq($.optimizelitvec, $.COMMA, $.literal),
    ),

    // optimizecond
    //     : COLON optimizelitvec[bd] { $$ = $bd; }
    //     | COLON                    { $$ = BUILDER.body(); }
    //     |                          { $$ = BUILDER.body(); }
    //     ;
    noptimizecond: $ => choice(
      seq($.COLON, $.optimizelitvec),
      $.COLON,
    ),

    // maxelemlist
    //     :                 optimizeweight[w] optimizetuple[t] optimizecond[bd] { BUILDER.optimize(@$, BUILDER.term(@w, UnOp::NEG, $w.first), $w.second, $t, $bd); }
    //     | maxelemlist SEM optimizeweight[w] optimizetuple[t] optimizecond[bd] { BUILDER.optimize(@$, BUILDER.term(@w, UnOp::NEG, $w.first), $w.second, $t, $bd); }
    //     ;
    maxelemlist: $ => choice(
      seq($.optimizeweight,                                  ),
      seq($.optimizeweight,                   $.noptimizecond),
      seq($.optimizeweight, $.noptimizetuple,                ),
      seq($.optimizeweight, $.noptimizetuple, $.noptimizecond),
      seq($.maxelemlist, $.SEM, $.optimizeweight,                                  ),
      seq($.maxelemlist, $.SEM, $.optimizeweight,                   $.noptimizecond),
      seq($.maxelemlist, $.SEM, $.optimizeweight, $.noptimizetuple,                ),
      seq($.maxelemlist, $.SEM, $.optimizeweight, $.noptimizetuple, $.noptimizecond),
    ),

    // minelemlist
    //     :                 optimizeweight[w] optimizetuple[t] optimizecond[bd] { BUILDER.optimize(@$, $w.first, $w.second, $t, $bd); }
    //     | minelemlist SEM optimizeweight[w] optimizetuple[t] optimizecond[bd] { BUILDER.optimize(@$, $w.first, $w.second, $t, $bd); }
    //     ;
    minelemlist: $ => choice(
      seq($.optimizeweight,                                  ),
      seq($.optimizeweight,                   $.noptimizecond),
      seq($.optimizeweight, $.noptimizetuple,                ),
      seq($.optimizeweight, $.noptimizetuple, $.noptimizecond),
      seq($.minelemlist, $.SEM, $.optimizeweight,                                  ),
      seq($.minelemlist, $.SEM, $.optimizeweight,                   $.noptimizecond),
      seq($.minelemlist, $.SEM, $.optimizeweight, $.noptimizetuple,                ),
      seq($.minelemlist, $.SEM, $.optimizeweight, $.noptimizetuple, $.noptimizecond),
    ),

    // define
    //     : identifier[uid] EQ constterm[rhs] {  BUILDER.define(@$, String::fromRep($uid), $rhs, false, LOGGER); }
    //     ;

    // nidlist 
    //     : nidlist[list] COMMA identifier[id] { $$ = BUILDER.idvec($list, @id, String::fromRep($id)); }
    //     | identifier[id]                     { $$ = BUILDER.idvec(BUILDER.idvec(), @id, String::fromRep($id)); }
    //     ;
    nidlist: $ => choice(
      seq($.nidlist, $.COMMA, $.identifier),
      $.identifier,
    ),

    // idlist 
    //     :               { $$ = BUILDER.idvec(); }
    //     | nidlist[list] { $$ = $list; }
    //     ;


    // // {{{1 theory

    // theory_op
    //     : THEORY_OP[op]  { $$ = $op; }
    //     | NOT[not]       { $$ = $not; }
    //     ;

    // // {{{2 theory atoms

    // theory_op_list
    //     : theory_op_list[ops] theory_op[op] { $$ = BUILDER.theoryops($ops, String::fromRep($op)); }
    //     | theory_op[op]                     { $$ = BUILDER.theoryops(BUILDER.theoryops(), String::fromRep($op)); }
    //     ;

    // theory_term
    //     : LBRACE theory_opterm_list[list] RBRACE                              { $$ = BUILDER.theorytermset(@$, $list); }
    //     | LBRACK theory_opterm_list[list] RBRACK                              { $$ = BUILDER.theoryoptermlist(@$, $list); }
    //     | LPAREN RPAREN                                                       { $$ = BUILDER.theorytermtuple(@$, BUILDER.theoryopterms()); }
    //     | LPAREN theory_opterm[term] RPAREN                                   { $$ = BUILDER.theorytermopterm(@$, $term); }
    //     | LPAREN theory_opterm[opterm] COMMA RPAREN                           { $$ = BUILDER.theorytermtuple(@$, BUILDER.theoryopterms(BUILDER.theoryopterms(), @opterm, $opterm)); }
    //     | LPAREN theory_opterm[opterm] COMMA theory_opterm_nlist[list] RPAREN { $$ = BUILDER.theorytermtuple(@$, BUILDER.theoryopterms(@opterm, $opterm, $list)); }
    //     | identifier[id] LPAREN theory_opterm_list[list] RPAREN               { $$ = BUILDER.theorytermfun(@$, String::fromRep($id), $list); }
    //     | identifier[id]                                                      { $$ = BUILDER.theorytermvalue(@$, Symbol::createId(String::fromRep($id))); }
    //     | NUMBER[num]                                                         { $$ = BUILDER.theorytermvalue(@$, Symbol::createNum($num)); }
    //     | STRING[str]                                                         { $$ = BUILDER.theorytermvalue(@$, Symbol::createStr(String::fromRep($str))); }
    //     | INFIMUM                                                             { $$ = BUILDER.theorytermvalue(@$, Symbol::createInf()); }
    //     | SUPREMUM                                                            { $$ = BUILDER.theorytermvalue(@$, Symbol::createSup()); }
    //     | VARIABLE[var]                                                       { $$ = BUILDER.theorytermvar(@$, String::fromRep($var)); }
    //     ;

    // theory_opterm
    //     : theory_opterm[opterm] theory_op_list[ops] theory_term[term] { $$ = BUILDER.theoryopterm($opterm, $ops, $term); }
    //     | theory_op_list[ops] theory_term[term]                       { $$ = BUILDER.theoryopterm($ops, $term); }
    //     | theory_term[term]                                           { $$ = BUILDER.theoryopterm(BUILDER.theoryops(), $term); }
    //     ;

    // theory_opterm_nlist
    //     : theory_opterm_nlist[list] COMMA theory_opterm[opterm] { $$ = BUILDER.theoryopterms($list, @opterm, $opterm); }
    //     | theory_opterm[opterm]                                 { $$ = BUILDER.theoryopterms(BUILDER.theoryopterms(), @opterm, $opterm); }
    //     ;

    // theory_opterm_list
    //     : theory_opterm_nlist[list] { $$ = $list; }
    //     |                           { $$ = BUILDER.theoryopterms(); }
    //     ;

    // theory_atom_element 
    //     : theory_opterm_nlist[list] disable_theory_lexing optcondition[cond] { $$ = { $list, $cond }; }
    //     |                           disable_theory_lexing COLON litvec[cond] { $$ = { BUILDER.theoryopterms(), $cond }; }
    //     ;

    // theory_atom_element_nlist
    //     : theory_atom_element_nlist[list] enable_theory_lexing SEM theory_atom_element[elem] { $$ = BUILDER.theoryelems($list, $elem.first, $elem.second); }
    //     | theory_atom_element[elem]                                                          { $$ = BUILDER.theoryelems(BUILDER.theoryelems(), $elem.first, $elem.second); }
    //     ;

    // theory_atom_element_list
    //     : theory_atom_element_nlist[list] { $$ = $list; }
    //     |                                 { $$ = BUILDER.theoryelems(); }
    //     ;

    // theory_atom_name
    //     : identifier[id]                                  { $$ = BUILDER.term(@$, String::fromRep($id), BUILDER.termvecvec(BUILDER.termvecvec(), BUILDER.termvec()), false); }
    //     | identifier[id] LPAREN argvec[tvv] RPAREN[r]     { $$ = BUILDER.term(@$, String::fromRep($id), $tvv, false); }
    theory_atom_name: $ => choice(
      $.identifier,
      seq($._widentifier, $.RPAREN),
      seq($._widentifier, $.argvec, $.RPAREN),
   
    ),

    // theory_atom
    //     : AND theory_atom_name[name] { $$ = BUILDER.theoryatom($name, BUILDER.theoryelems()); }
    //     | AND theory_atom_name[name] enable_theory_lexing LBRACE theory_atom_element_list[elems] enable_theory_lexing RBRACE                                     disable_theory_lexing { $$ = BUILDER.theoryatom($name, $elems); }
    //     | AND theory_atom_name[name] enable_theory_lexing LBRACE theory_atom_element_list[elems] enable_theory_lexing RBRACE theory_op[op] theory_opterm[opterm] disable_theory_lexing { $$ = BUILDER.theoryatom($name, $elems, String::fromRep($op), @opterm, $opterm); }
    //     ;
    theory_atom: $ => choice(
      seq($.AND, $.theory_atom_name),
      // TODO
      // seq($.AND, $.theory_atom_name, $.enable_theory_lexing, $.LBRACE, $.theory_atom_element_list, $.enable_theory_lexing ,$.RBRACE, $.disable_theory_lexing),
      // seq($.AND, $.theory_atom_name, $.enable_theory_lexing, $.LBRACE, $.theory_atom_element_list, $.enable_theory_lexing ,$.RBRACE, $.theory_op, $.theory_opterm, $.disable_theory_lexing),
    ),

    // // {{{2 theory definition

    // theory_operator_nlist
    //     : theory_op[op]                                  { $$ = BUILDER.theoryops(BUILDER.theoryops(), String::fromRep($op)); }
    //     | theory_operator_nlist[ops] COMMA theory_op[op] { $$ = BUILDER.theoryops($ops, String::fromRep($op)); }
    //     ;

    // theory_operator_list
    //     : theory_operator_nlist[ops] { $$ = $ops; }
    //     |                            { $$ = BUILDER.theoryops(); }
    //     ;

    // theory_operator_definition
    //     : theory_op[op] enable_theory_definition_lexing COLON NUMBER[arity] COMMA UNARY              { $$ = BUILDER.theoryopdef(@$, String::fromRep($op), $arity, TheoryOperatorType::Unary); }
    //     | theory_op[op] enable_theory_definition_lexing COLON NUMBER[arity] COMMA BINARY COMMA LEFT  { $$ = BUILDER.theoryopdef(@$, String::fromRep($op), $arity, TheoryOperatorType::BinaryLeft); }
    //     | theory_op[op] enable_theory_definition_lexing COLON NUMBER[arity] COMMA BINARY COMMA RIGHT { $$ = BUILDER.theoryopdef(@$, String::fromRep($op), $arity, TheoryOperatorType::BinaryRight); }
    //     ;

    // theory_operator_definition_nlist
    //     : theory_operator_definition[def]                                                                 { $$ = BUILDER.theoryopdefs(BUILDER.theoryopdefs(), $def); }
    //     | theory_operator_definition_nlist[defs] enable_theory_lexing SEM theory_operator_definition[def] { $$ = BUILDER.theoryopdefs($defs, $def); }
    //     ;

    // theory_operator_definiton_list
    //     : theory_operator_definition_nlist[defs] { $$ = $defs; }
    //     |                                        { $$ = BUILDER.theoryopdefs(); }
    //     ;

    // theory_definition_identifier
    //     : identifier[id] { $$ = $id; }
    //     | LEFT           { $$ = String::toRep("left"); }
    //     | RIGHT          { $$ = String::toRep("right"); }
    //     | UNARY          { $$ = String::toRep("unary"); }
    //     | BINARY         { $$ = String::toRep("binary"); }
    //     | HEAD           { $$ = String::toRep("head"); }
    //     | BODY           { $$ = String::toRep("body"); }
    //     | ANY            { $$ = String::toRep("any"); }
    //     | DIRECTIVE      { $$ = String::toRep("directive"); }
    //     ;

    // theory_term_definition
    //     : theory_definition_identifier[name] enable_theory_lexing LBRACE theory_operator_definiton_list[ops] enable_theory_definition_lexing RBRACE { $$ = BUILDER.theorytermdef(@$, String::fromRep($name), $ops, LOGGER); }
    //     ;

    // theory_atom_type
    //     : HEAD      { $$ = TheoryAtomType::Head; }
    //     | BODY      { $$ = TheoryAtomType::Body; }
    //     | ANY       { $$ = TheoryAtomType::Any; }
    //     | DIRECTIVE { $$ = TheoryAtomType::Directive; }
    //     ;

    // theory_atom_definition
    //     : AND theory_definition_identifier[name] SLASH NUMBER[arity] COLON theory_definition_identifier[termdef] COMMA
    //       enable_theory_lexing LBRACE theory_operator_list[ops] enable_theory_definition_lexing RBRACE COMMA theory_definition_identifier[guarddef] COMMA theory_atom_type[type] { $$ = BUILDER.theoryatomdef(@$, String::fromRep($name), $arity, String::fromRep($termdef), $type, $ops, String::fromRep($guarddef)); }
    //     | AND theory_definition_identifier[name] SLASH NUMBER[arity] COLON theory_definition_identifier[termdef] COMMA                                    theory_atom_type[type] { $$ = BUILDER.theoryatomdef(@$, String::fromRep($name), $arity, String::fromRep($termdef), $type); }
    //     ;

    // theory_definition_nlist
    //     : theory_definition_list[defs] SEM theory_atom_definition[def] { $$ = BUILDER.theorydefs($defs, $def); }
    //     | theory_definition_list[defs] SEM theory_term_definition[def] { $$ = BUILDER.theorydefs($defs, $def); }
    //     | theory_atom_definition[def] { $$ = BUILDER.theorydefs(BUILDER.theorydefs(), $def); }
    //     | theory_term_definition[def] { $$ = BUILDER.theorydefs(BUILDER.theorydefs(), $def); }
    //     ;

    // theory_definition_list
    //     : theory_definition_nlist[defs] { $$ = $defs; }
    //     |                               { $$ = BUILDER.theorydefs(); }
    //     ;

    // statement
    //     : THEORY identifier[name] enable_theory_definition_lexing LBRACE theory_definition_list[defs] RBRACE disable_theory_lexing DOT { BUILDER.theorydef(@$, String::fromRep($name), $defs, LOGGER); }
    //     ;

    // // {{{2 lexing

    // enable_theory_lexing
    //     : { lexer->theoryLexing(TheoryLexing::Theory); }
    //     ;

    // enable_theory_definition_lexing
    //     : { lexer->theoryLexing(TheoryLexing::Definition); }
    //     ;

    // disable_theory_lexing
    //     : { lexer->theoryLexing(TheoryLexing::Disabled); }
    //     ;


    // infimum: $ => token(seq($.INFIMUM, optional('imum'))),
    // supremum: $ => token(seq($.SUPREMUM, optional('remum'))),
    ANONYMOUS: $ => '_',
    VARIABLE: $ => token(seq(repeat('_'), /[A-Z]/, repeat(/[A-Za-z0-9_']/))),
    identifier: $ => token(seq(repeat('_'), /[a-z]/, repeat(/[A-Za-z0-9_]/))),
    //Introduced to disallow white space after identifier followed by a bracket ie. not 'bla ()' but 'bla()'
    _widentifier: $ => seq($.identifier,alias(token.immediate('('), $.LPAREN)),
    


    NUMBER: $ => choice(
      $.dec,
      $.hex,
      $.oct,
      $.bin,
    ),
    dec: $ => choice('0',/([1-9][0-9]*)/),
    hex: $ =>  token(seq('0x',/([0-9A-Fa-f]+)/)),
    oct: $ =>  token(seq('0o',/([1-7]+)/)),
    bin: $ =>  token(seq('0b',/([0-1]+)/)),
 
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
      ),
      seq(
        "'",
        repeat(choice(
          alias($.unescaped_single_string_fragment, $.string_fragment),
          $.escape_sequence
        )),
        "'"
      )
    ),

    // Workaround to https://github.com/tree-sitter/tree-sitter/issues/1156
    // We give names to the token() constructs containing a regexp
    // so as to obtain a node in the CST.
    //
    unescaped_double_string_fragment: $ =>
      token.immediate(prec(1, /[^"\\]+/)),

    // same here
    unescaped_single_string_fragment: $ =>
      token.immediate(prec(1, /[^'\\]+/)),

    escape_sequence: $ => token.immediate(seq(
      '\\',
      choice(
        /[^xu0-7]/,
        /[0-7]{1,3}/,
        /x[0-9a-fA-F]{2}/,
        /u[0-9a-fA-F]{4}/,
        /u{[0-9a-fA-F]+}/
      )
    )),
  }
});
