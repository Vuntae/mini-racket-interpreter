#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./interp.rkt"))

(test (parse 'x) (id 'x))
(test (parse 10) (num 10))
(test (parse 'true) (bool #t))
(test (parse 'false) (bool #f))
(test (parse "Hola") (str "Hola"))

(test (parse '{sub1 1}) (op sub1 (list (num 1))))
(test/exn (parse '{sub1 1 2}) "")

(test (parse '{num? 1}) (op number? (list (num 1))))
(test/exn (parse '{num? 2 1}) "")

(test (parse '{modulo 1 2}) (op modulo (list (num 1) (num 2))))
(test/exn (parse '{modulo 1 2 3}) "")

(test (parse '{min 1}) (op min (list (num 1))))
(test/exn (parse '{min}) "")

(test (parse '{if {zero? 10} {fun {x} {* x x}} 0})
      (iF (op zero? (list (num 10)))
           (fun (list 'x) (op * (list (id 'x) (id 'x))))
           (num 0)))

(test (parse '{{fun {x} {zero? x}} {+ 10 1}})
      (app (fun (list 'x) (op zero? (list (id 'x))))
            (list (op + (list (num 10) (num 1))))))

(test (parse '{if {{fun {} true}} 0 1})
      (iF (app (fun '() (bool #t)) '())
           (num 0)
           (num 1)))

(test/exn (parse '{fun {x x} x}) "")

(test (lookup 'x (cons-env 'y (numV 10) (cons-env 'x (numV 1) (mt-env))))
      (numV 1))

(test (lookup 'foo (cons-env 'x (numV 1)
                             (cons-env 'y (boolV #t)
                                       (cons-env 'foo
                                                 (closureV '(x y)
                                                           (op + (list (id 'x) (id 'y)))
                                                           (mt-env))
                                                 (mt-env)))))
      (closureV '(x y) (op + (list (id 'x) (id 'y))) (mt-env)))

(test/exn (lookup 'x (mt-env)) "")

(test/exn (lookup 'x (cons-env 'y (numV 1) (cons-env 'z (boolV #t) (mt-env)))) "")

(define (prueba expr)
  (interp (parse expr) (mt-env)))

(test (prueba '{{fun {x y} {+ x y y}} 1 10}) (numV 21))

(test/exn (prueba '{{fun {x y z} {* x y z}} 1 1}) "")

(test/exn (prueba '{{fun {x x} x} 1}) "")

(test (prueba '{{fun {x}
                     {{fun {y}
                           {+ x y}} {+ x x}}} 10}) (numV 30))

(test/exn (prueba '{{fun {} x}}) "")

(test (interp (parse '{fun {x} x}) (cons-env 'x (boolV #f)
                                                 (cons-env 'y (strV "s")
                                                           (mt-env))))
      (closureV '(x) (id 'x) (cons-env 'x (boolV #f)
                                                 (cons-env 'y (strV "s")
                                                           (mt-env)))))

(test/exn (prueba '{{fun {x} x} x}) "")

(test (prueba '{{fun {} "Hola"}}) (strV "Hola"))

;; Verificar estructura del if {if <expr> <expr> <expr>}
(test/exn (prueba '{if}) "")

(test (prueba '{{fun {x} {if {zero? x} 0 1}} 10}) (numV 1))

(test (prueba '{if {zero? 0} {fun {} x} {fun {x} x}})
      (closureV '() (id 'x) (mt-env)))

(test (prueba '{if {and {zero? 0} {< 1 10}}
                   "Si"
                   "No"})
      (strV "Si"))
