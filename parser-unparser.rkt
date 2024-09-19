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
(define parser
    (lambda (exp)
        (cond
            [(eq? (car exp) 'simple-circuit)
                (simple-circuit (cadr exp)
                                (caddr exp)
                                (parser-chip (cadddr exp))
                )
            ]
            [(eq? (car exp) 'complex-circuit)
                (complex-circuit (parser (cadr exp))
                                 (map parser (cdr (caddr exp)))
                                 (cadddr exp)
                                 (car (cddddr exp))
                )
            ]
            [(eq? (car exp) 'comp-chip) (parser-chip exp)]
            [(eq? (car exp) 'prim-chip) (parser-chip exp)]
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
                           (parser (cadddr exp)))]
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

;;UNPARSER
(define unparser
    (lambda (ast)
        (cases circuito ast
            (simple-circuit (in out chip)
                (list 'simple-circuit in out (unparser-chip chip))
            )
            (complex-circuit (circ lcircs in out)
                (list 'complex-circuit (unparser circ)
                                       (map unparser lcircs)
                                       in
                                       out
                )
            )
        )
    )
)

(define unparser-chip
    (lambda (ast)
        (cases chip ast
            (prim-chip (chip-prim)
                (list 'prim-chip (unparser-chip-prim chip-prim))
            )
            (comp-chip (in out circ)
                (list 'comp-chip in out (unparser circ))
            )
        )
    )
)

(define unparser-chip-prim
    (lambda (ast)
        (cases chip-prim ast
            (chip-or () (list 'chip-or))
            (chip-and () (list 'chip-and))
            (chip-not () (list 'chip-not))
            (chip-xor () (list 'chip-xor))
            (chip-nand () (list 'chip-nand))
            (chip-nor () (list 'chip-nor))
            (chip-xnor () (list 'chip-xnor))
        )
    )
)

;;AREA DEL PROGRAMADOR
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

(define circuito2
    '(complex-circuit
        (simple-circuit
            (a b c)
            (z)
            (comp-chip
                (INA INB INC)
                (OUTD OUTE)
                (complex-circuit
                    (simple-circuit (m q) (s) (prim-chip (chip-and))
                    )
                    (list
                        (simple-circuit (n) (q) (prim-chip (chip-not)))
                        (simple-circuit (o) (r) (prim-chip (chip-not)))
                        (simple-circuit (r) (p) (prim-chip (chip-and)))
                    )
                    (m n o p)
                    (s t)
                )
            )
        )
        (list
            (simple-circuit (x y) (z) (prim-chip (chip-or)))
        )
        (a b c)
        (z)
    )
)
