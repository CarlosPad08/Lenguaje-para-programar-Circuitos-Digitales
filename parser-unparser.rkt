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

;;PARSER
(define parser-circuit
    (lambda (exp)
        (cond
            [(eq? (car exp) 'simple-circuit)
                (simple-circuit (cadr exp)
                                (caddr exp)
                                (parser-chip (cadddr exp))
                )
            ]
            [(eq? (car exp) 'complex-circuit)
                (complex-circuit (parser-circuit (cadr exp))
                                 (map parser-circuit (cdr (caddr exp)))
                                 (cadddr exp)
                                 (car (cddddr exp))
                )
            ]
            [else parser-chip exp]
        )
    )
)

(define parser-chip
    (lambda (exp)
        (cond
            [(eq? (car exp) 'prim-chip) 
                (prim-chip (parser-chip-prim (cadr exp)))]
            [(eq? (car exp) 'comp-chip)
                (comp-chip (cadr exp)
                           (caddr exp)
                           (parser-circuit (cadddr exp)))]
        )
    )
)

(define parser-chip-prim
    (lambda (exp)
        (cond
            [(eq? (car exp) 'chip-or) 
                (chip-or)]
            [(eq? (car exp) 'chip-and)
                (chip-and)]
            [(eq? (car exp) 'chip-not)
                (chip-not)]
            [(eq? (car exp) 'chip-xor)
                (chip-xor)]
            [(eq? (car exp) 'chip-nand)
                (chip-nand)]
            [(eq? (car exp) 'chip-nor)
                (chip-nor)]
            [(eq? (car exp) 'chip-xnor)
                (chip-xnor)]
            )
    )
)

;;EJEMPLOS
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

(define circuitoBien
    '(simple-circuit (a b) (c) (prim-chip (chip-xor)))
)

(define circuitoPrueba
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