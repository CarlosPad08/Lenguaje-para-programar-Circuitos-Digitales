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

;;CONSTRUCTORES
(define simple-circuit
    (lambda (in out chip)
        (list 'simple-circuit in out chip)
    )
)

(define complex-circuit
    (lambda (circ lcircs in out)
        (list 'complex-circuit circ lcircs in out)
    )
)

(define prim-chip
    (lambda (chip-prim)
        (list 'prim-chip chip-prim)
    )
)

(define comp-chip
    (lambda (in out circ)
        (list 'comp-chip in out circ)
    )
)

(define chip-or
    (lambda ()
        (list 'chip-or)
    )
)

(define chip-and
    (lambda ()
        (list 'chip-and)
    )
)

(define chip-not
    (lambda ()
        (list 'chip-not)
    )
)

(define chip-xor
    (lambda ()
        (list 'chip-xor)
    )
)

(define chip-nand
    (lambda ()
        (list 'chip-nand)
    )
)

(define chip-nor
    (lambda ()
        (list 'chip-nor)
    )
)

(define chip-xnor
    (lambda ()
        (list 'chip-xnor)
    )
)

;;PREDICADOS
(define simple-circuit?
    (lambda (x)
        (eq? (car x) 'simple-circuit)
    )
)

(define complex-circuit?
    (lambda (x)
        (eq? (car x) 'complex-circuit)
    )
)

(define prim-chip?
    (lambda (x)
        (eq? (car x) 'prim-chip)
    )
)

(define comp-chip?
    (lambda (x)
        (eq? (car x) 'comp-chip)
    )
)

(define chip-or?
    (lambda (x)
        (eq? (car x) 'chip-or)
    )
)

(define chip-and?
    (lambda (x)
        (eq? (car x) 'chip-and)
    )
)

(define chip-not?
    (lambda (x)
        (eq? (car x) 'chip-not)
    )
)

(define chip-xor?
    (lambda (x)
        (eq? (car x) 'chip-xor)
    )
)

(define chip-nand?
    (lambda (x)
        (eq? (car x) 'chip-nand)
    )
)

(define chip-nor?
    (lambda (x)
        (eq? (car x) 'chip-nor)
    )
)

(define chip-xnor?
    (lambda (x)
        (eq? (car x) 'chip-xnor)
    )
)

;;EXTRACTORES
(define simple-circuit->in
    (lambda (x)
        (cadr x)
    )
)

(define simple-circuit->out
    (lambda (x)
        (caddr x)
    )
)

(define simple-circuit->chip
    (lambda (x)
        (cadddr x)
    )
)

(define complex-circuit->circ
    (lambda (x)
        (cadr x)
    )
)

(define complex-circuit->lcircs
    (lambda (x)
        (caddr x)
    )
)

(define complex-circuit->in
    (lambda (x)
        (cadddr x)
    )
)

(define complex-circuit->out
    (lambda (x)
        (car (cadddr x))
    )
)

(define prim-chip->chip-prim
    (lambda (x)
        (cadr x)
    )
)

(define comp-chip->in
    (lambda (x)
        (cadr x)
    )
)

(define comp-chip->out
    (lambda (x)
        (caddr x)
    )
)

(define comp-chip->circ
    (lambda (x)
        (cadddr x)
    )
)

;;EJEMPLOS
;;AREA DEL PROGRAMADOR
(define half-substractor
    (comp-chip
        '(INA INB)
        '(DIFF BORROW)
        (complex-circuit
            (simple-circuit '(a b) '(f) (prim-chip (chip-xor)))
            (list
                (simple-circuit '(c) '(d) (prim-chip (chip-not)))
                (simple-circuit '(d e) '(g) (prim-chip (chip-and)))
            )
            '(a b c e)
            '(f g)
        )
    )
)

(define latch
    (comp-chip
        '(INR INS)
        '(OUTQ OUTQN)
        (complex-circuit
            (simple-circuit '(a b) '(c) (prim-chip (chip-nor)))
            (list
                (simple-circuit '(c d) '(b) (prim-chip (chip-nor)))
            )
            '(a b c d)
            '(b c)
        )
    )
)

(define circuito1
    (complex-circuit
        (simple-circuit 
            '(w x y z)
            '(e f)
            (comp-chip
                '(INA INB INC IND)
                '(OUTE OUTF)
                (complex-circuit
                    (simple-circuit '(a b) '(e) (prim-chip (chip-nand)))
                    (list
                        (simple-circuit '(c d) '(f) (prim-chip (chip-nand)))
                    )
                    '(a b c d)
                    '(e f)
                )
            )
        )
        (list
            (simple-circuit 
                '(e f)
                '(g)
                (comp-chip
                    '(INE INF)
                    '(OUTG)
                    (complex-circuit
                        (simple-circuit '(e f) '(g) (prim-chip (chip-nor)))
                        (list
                            (simple-circuit '(g) '(h) (prim-chip (chip-not)))
                        )
                        '(e f)
                        '(h)
                    )
                )
            )
        )
        '(w x y z)
        '(v)
    )
)
