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
        (lambda (signal)
            (cond
                [(= signal 0) 'simple-circuit]
                [(= signal 1) in]
                [(= signal 2) out]
                [(= signal 3) chip]
                [else (eopl:error 'simple-circuit "Invalid signal")]
            )
        )
    )
)

(define complex-circuit
    (lambda (circ lcircs in out)
        (lambda (signal)
            (cond
                [(= signal 0) 'complex-circuit]
                [(= signal 1) circ]
                [(= signal 2) lcircs]
                [(= signal 3) in]
                [(= signal 4) out]
                [else (eopl:error 'complex-circuit "Invalid signal")]
            )
        )
    )
)

(define prim-chip
    (lambda (chip-prim)
        (lambda (signal)
            (cond
                [(= signal 0) 'prim-chip]
                [(= signal 1) chip-prim]
                [else (eopl:error 'prim-chip "Invalid signal")]
            )
        )
    )
)

(define comp-chip
    (lambda (in out circ)
        (lambda (signal)
            (cond
                [(= signal 0) 'comp-chip]
                [(= signal 1) in]
                [(= signal 2) out]
                [(= signal 3) circ]
                [else (eopl:error 'comp-chip "Invalid signal")]
            )
        )
    )
)

(define chip-or
    (lambda ()
        (lambda (signal)
            (cond
                [(= signal 0) 'chip-or]
                [else (eopl:error 'chip-or "Invalid signal")]
            )
        )
    )
)

(define chip-and
    (lambda ()
        (lambda (signal)
            (cond
                [(= signal 0) 'chip-and]
                [else (eopl:error 'chip-and "Invalid signal")]
            )
        )
    )
)

(define chip-not
    (lambda ()
        (lambda (signal)
            (cond
                [(= signal 0) 'chip-not]
                [else (eopl:error 'chip-not "Invalid signal")]
            )
        )
    )
)

(define chip-xor
    (lambda ()
        (lambda (signal)
            (cond
                [(= signal 0) 'chip-xor]
                [else (eopl:error 'chip-xor "Invalid signal")]
            )
        )
    )
)

(define chip-nand
    (lambda ()
        (lambda (signal)
            (cond
                [(= signal 0) 'chip-nand]
                [else (eopl:error 'chip-nand "Invalid signal")]
            )
        )
    )
)

(define chip-nor
    (lambda ()
        (lambda (signal)
            (cond
                [(= signal 0) 'chip-nor]
                [else (eopl:error 'chip-nor "Invalid signal")]
            )
        )
    )
)

(define chip-xnor
    (lambda ()
        (lambda (signal)
            (cond
                [(= signal 0) 'chip-xnor]
                [else (eopl:error 'chip-xnor "Invalid signal")]
            )
        )
    )
)

;;PREDICADOS
(define simple-circuit?
    (lambda (x)
        (eq? (x 0) 'simple-circuit)
    )
)

(define complex-circuit?
    (lambda (x)
        (eq? (x 0) 'complex-circuit)
    )
)

(define prim-chip?
    (lambda (x)
        (eq? (x 0) 'prim-chip)
    )
)

(define comp-chip?
    (lambda (x)
        (eq? (x 0) 'comp-chip)
    )
)

(define chip-or?
    (lambda (x)
        (eq? (x 0) 'chip-or)
    )
)

(define chip-and?
    (lambda (x)
        (eq? (x 0) 'chip-and)
    )
)

(define chip-not?
    (lambda (x)
        (eq? (x 0) 'chip-not)
    )
)

(define chip-xor?
    (lambda (x)
        (eq? (x 0) 'chip-xor)
    )
)

(define chip-nand?
    (lambda (x)
        (eq? (x 0) 'chip-nand)
    )
)

(define chip-nor?
    (lambda (x)
        (eq? (x 0) 'chip-nor)
    )
)

(define chip-xnor?
    (lambda (x)
        (eq? (x 0) 'chip-xnor)
    )
)

;;EXTRACTORES
(define simple-circuit->in
    (lambda (x)
        (x 1)
    )
)

(define simple-circuit->out
    (lambda (x)
        (x 2)
    )
)

(define simple-circuit->chip
    (lambda (x)
        (x 3)
    )
)

(define complex-circuit->circ
    (lambda (x)
        (x 1)
    )
)

(define complex-circuit->lcircs
    (lambda (x)
        (x 2)
    )
)

(define complex-circuit->in
    (lambda (x)
        (x 3)
    )
)

(define complex-circuit->out
    (lambda (x)
        (x 4)
    )
)

(define comp-chip->in
    (lambda (x)
        (x 1)
    )
)

(define comp-chip->out
    (lambda (x)
        (x 2)
    )
)

(define comp-chip->circ
    (lambda (x)
        (x 3)
    )
)

(define prim-chip->chip-prim
    (lambda (x)
        (x 1)
    )
)

;;EJEMPLOS
;;AREA DEL PROGRAMADOR
(define circ1
    (simple-circuit '(a b) '(c) (chip-or))
)

(define circ2
    (simple-circuit '(a b) '(c) (chip-and))
)