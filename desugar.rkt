#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; desugar :: CFSBWAE -> CFSBAE
(define (desugar expr)
  ;; Si expr no es una lista, la convierte en una lista
  (let* ([expr-list (if (list? expr) expr (list expr))]) 
  (match (car expr-list)
    ;; Comparamos la estructura de la expresión para saber en qué caso estamos
    [(numS n) (num n)]
    [(idS i) (id i)]
    [(boolS b) (bool b)]
    [(strS s) (str s)]
    [(opS procedure args) (op procedure (map desugar args))] ;; Todos los procedimientos tienen la operación (procedure) y los argumentos (args) por lo que podemos tratarlas a todas de la misma forma
    [(withS bindings body)
     ;; Transforma los 'with' en una aplicación de función
     (app (fun (map binding-id bindings) (desugar body)) 
          (map (lambda (b) (desugar (binding-value b))) bindings))]
    [(with*S bindings body)
     ;; Transforma los 'with*' en una serie de aplicaciones de función anidadas
     (foldr (lambda (binding acc) ;; Utilizamos foldr para anidar los with*
              (app (fun (list (binding-id binding)) acc)
                   (list (desugar (binding-value binding)))))
            (desugar body)
            bindings)]
    [(funS params body)
     (fun params (desugar body))] 
    [(appS func args)
     (app (desugar func) (map desugar args))]
    [(iFS test-expr then-expr else-expr)
     (iF (desugar test-expr)
         (desugar then-expr)
         (desugar else-expr))] ;; Funciona similar a la función cond->if pero con un solo if
    [(conDS conditions else-expr) 
     (cond->if conditions (desugar else-expr))]))) ;; Aquí utilizamos la funciónn auxiliar cond->if para anidar las clausulas del cond

;; Función auxiliar para transformar cond en if anidados
(define (cond->if conditions else-expr)
  (match conditions
    ['() else-expr] ;; Caso base, sin condiciones siempre se regresa la expresión
    [(cons (condition test-expr then-expr) rest)
     (iF (desugar test-expr) ;; Se hace el desugar de la primera expresion a comparar
         (desugar then-expr) ;; Y se hace el desugar de la primera a evaluar 
         (cond->if rest else-expr))])) ;; Se llama recursivamente a 'cond->if' con el resto de las condiciones

