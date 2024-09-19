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
(define half-substractor
   '(comp-chip
        (INA INB)
        (DIFF BORROW)
        (complex-circuit
            (simple-circuit (a b) (f) (prim-chip (chip-xor)))
            (list
                (simple-circuit (c) (d) (prim-chip (chip-not)))
                (simple-circuit (d e) (g) (prim-chip (chip-and)))
            )
            (a b c e)
            (f g)
        )
    )
)

(define latch
    '(comp-chip
        (INR INS)
        (OUTQ OUTQN)
        (complex-circuit
            (simple-circuit (a b) (c) (prim-chip (chip-nor)))
            (list
                (simple-circuit (c d) (b) (prim-chip (chip-nor)))
            )
            (a b c d)
            (b c)
        )
    )
)

(define circuito2
    '(complex-circuit
        (simple-circuit (a b) (c) (prim-chip (chip-xor)))
        (list
            (simple-circuit (c) (d) (prim-chip (chip-not)))
            (simple-circuit (d e) (g) (prim-chip (chip-and)))
        )
        (a b c e)
        (f g)
    )
)

(define circuito1
    '(complex-circuit
        (simple-circuit 
            (w x y z)
            (e f)
            (comp-chip
                (INA INB INC IND)
                (OUTE OUTF)
                (complex-circuit
                    (simple-circuit (a b) (e) (prim-chip (chip-nand)))
                    (list
                        (simple-circuit (c d) (f) (prim-chip (chip-nand)))
                    )
                    (a b c d)
                    (e f)
                )
            )
        )
        (list
            (simple-circuit 
                (e f)
                (g)
                (comp-chip
                    (INE INF)
                    (OUTG)
                    (complex-circuit
                        (simple-circuit (e f) (g) (prim-chip (chip-nor)))
                        (list
                            (simple-circuit (g) (h) (prim-chip (chip-not)))
                        )
                        (e f)
                        (h)
                    )
                )
            )
        )
        (w x y z)
        (v)
    )
)