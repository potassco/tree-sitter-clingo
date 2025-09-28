; head :- a,
;         b.
; head :-
;   a,
;   b.
;
; We use the error node here to also indent partial rules.
([(ERROR) (statement)] @indent.align
  (#contains? @indent.align ":-")
  (#set! indent.open_delimiter ":-")
  (#set! indent.close_delimiter ".")
  (#set! indent.increment 3))

; similar to rule but for weak constraints
([(ERROR) (statement)] @indent.align
  (#contains? @indent.align ":~")
  (#set! indent.open_delimiter ":~")
  (#set! indent.close_delimiter ".")
  (#set! indent.increment 3))

; similar to rule but for statements with a condition
([(ERROR) (statement)] @indent.align
  (#not-contains? @indent.align ":-")
  (#not-contains? @indent.align ":~")
  (#set! indent.open_delimiter ":")
  (#set! indent.close_delimiter ".")
  (#set! indent.increment 2))

; #minimize { a: a; 
;             b: b; 
;             c: c;
;             d: d }.
; Also applies to nested aggregates and theory atoms. We do not treat the error
; node here. An editor that ensures balanced braces should help.
([(minimize) (maximize) (set_aggregate) (body_aggregate) (head_aggregate) (theory_atom)] @indent.align
  (#set! indent.open_delimiter "{")
  (#set! indent.close_delimiter "}")
  (#set! indent.increment 2))

; indent conditional literals
((conditional_literal) @indent.align
                       (#set! indent.open_delimiter ":") 
                       (#set! indent.increment 2))

; indent conditions
((_ ":" . (condition))@indent.align
       (#set! indent.open_delimiter ":") 
       (#set! indent.increment 2))
