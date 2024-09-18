#lang eopl

#|
AUTOR: CARLOS FERNANDO PADILLA MESA - 202059962

<circuito> ::= circ_simple ({cable}*)
                           ({cable}*)
                           <chip>
            simple-circuit (in out chip)

           ::= circ_comp <circuito> {<circuito>}+
                         input {cable}*
                         output {cable}*
            complex-circuit (circ lcircs in out)

<chip> ::= <chip_prim>
           prim-chip (chip-prim)

       ::= chip (--> {(port)}*)
                (<-- {(port)}*)
                <circuito>
           comp-chip(in out circ)

<chip prim> ::= prim_or
                chip-or()

            ::= prim_and
                chip-and() 

            ::= prim_not
                chip-not()

            ::= prim_xor
                chip-xor()
            
            ::= prim_nand
                chip-nand()

            ::= prim_nor
                chip-nor()

            ::= prim_xnor
                chip-xnor()
|#

;;REPRESENTACION
(define-datatype circuito circuito?
  (simple-circuit 
    (in (list-of symbol?))
    (out (list-of symbol?))
    (chip chip?))
  (complex-circuit 
    (circ circuito?)
    (lcircs (list-of circuito?))
    (in (list-of symbol?))
    (out (list-of symbol?)))
)

(define-datatype chip chip?
  (prim-chip
    (chip-prim chip-prim?))
  (comp-chip 
    (in (list-of symbol?))
    (out (list-of symbol?))
    (circ circuito?))
)

(define-datatype chip-prim chip-prim?
    (chip-or)
    (chip-and)
    (chip-not)
    (chip-xor)
    (chip-nand)
    (chip-nor)
    (chip-xnor)
)

;;EJEMPLOS
;;AREA DEL PROGRAMADOR
