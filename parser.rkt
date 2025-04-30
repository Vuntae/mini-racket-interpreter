#lang plai

(require "grammars.rkt")

;; parse :: s-exp -> CFSBWAE
(define (parse s-exp)
  (match s-exp
    ;; Literales se devuelven como tal
    [(? number? n) (num n)] 
    [(? symbol? s) ;; Si es un símbolo, se verifica si es 'true o 'false, de lo contrario se devuelve como un identificador.
     (cond
      [(eq? s 'true) (bool #t)]
      [(eq? s 'false) (bool #f)]
      [else (id s)])]
    [(? boolean? b) (bool b)] 
    [(? string? st) (str st)] 
    [else (let* ([cabeza (first s-exp)] [args (rest s-exp)]) ;; Si no es ninguno de los anteriores, entonces separamos la cabeza de la lista para verificar qué tipo de expresión es y 
    (match cabeza ;; Si la cabeza no es una lista, se verifica qué operación es y se aplica la función parse a cada uno de los argumentos.
    ['+ (op + (map parse args))]
    ['- (op - (map parse args))]
    ['* (op * (map parse args))]
    ['/ (op / (map parse args))]
    ;; Operaciones logicas
    ['and (if (>= (length args) 2) 
      (op my-and (map parse args))
      (error 'parse "La función my-and es binaria o más y debe recibir al menos dos argumentos"))]
    ['or (if (>= (length args) 2) 
      (op my-or (map parse args))
      (error 'parse "La función my-and es binaria o más y debe recibir al menos dos argumentos"))]
    ;; Se verifica que las operaciones reciban el numero correcto de argumentos o se regresa un error 
    ['sub1 (if (> (length args) 1)
      (error 'parse "La función sub1 es unaria y debe recibir exactamente un argumento")
      (op sub1 (map parse args)))]
    ['add1 (if (= (length args) 1) 
      (op add1 (map parse args))
      (error 'parse "La función add1 es unaria y debe recibir exactamente un argumento"))]
    ;; para operaciones sobre Strings
    ['str-length (if (= (length args) 1)
      (op string-length (map parse args))
      (error 'parse "La función str-length es unaria y debe recibir exactamente un argumento"))]
    ;; Faltan first y last
    ['str-last (if (= (length args) 1) 
      (op (lambda (s) (string-ref s (- (string-length s) 1))) (map parse args))
      (error 'parse "La función str-last es unaria y debe recibir exactamente un argumento"))]
    ['> (if (>= (length args) 2)
      (op > (map parse args))
      (error 'parse "La función > es binaria o más y debe recibir al menos dos argumentos"))]
    ['< (if (>= (length args) 2) 
      (op < (map parse args))
      (error 'parse "La función < es binaria o más y debe recibir al menos dos argumentos"))]
    ['<= (if (>= (length args) 2) 
      (op <= (map parse args))
      (error 'parse "La función <= es binaria o más y debe recibir al menos dos argumentos"))]
    ['>= (if (>= (length args) 2) 
      (op >= (map parse args))
      (error 'parse "La función >= es binaria o más y debe recibir al menos dos argumentos"))]
    ['= (if (>= (length args) 2) 
      (op = (map parse args))
      (error 'parse "La función = es binaria o más y debe recibir al menos dos argumentos"))]
    ['zero? (if (= (length args) 1) 
      (op zero? (map parse args))
      (error 'parse "La función zero? es unaria y debe recibir exactamente un argumento"))]
    ['modulo (if (= (length args) 2) 
      (op modulo (map parse args))
      (error 'parse "La función modulo es binaria y debe recibir dos argumentos"))]
    ['num? (if (> (length args) 1) 
      (error 'parse "La función num? es unaria y debe recibir exactamente un argumento")
      (op number? (map parse args)))]
    ['min (if (>= (length args) 1)
      (op min (map parse args))
      (error 'parse "La función min debe recibir al menos un argumento"))]
    ['if (if (and (list? args) (= (length args) 3)) ;; Si la operación es 'if, se verifica si los argumentos son una lista y su longitud es 3, de lo contrario se lanza un error.
        (let* ([test-expr (parse (first args))]
               [then-expr (parse (second args))]
               [else-expr (parse (third args))])
          (iF test-expr then-expr else-expr))
        (error 'parse "La construcción iF debe tener la forma {iF test-expr then-expr else-expr}"))]
    ['fun (if (and (list? args) (= (length args) 2)) ;; Si la operación es 'fun, se verifica si los argumentos son una lista y su longitud es 2, de lo contrario se lanza un error.
        (let* ([params (if (list? (first args)) (first args) (list (first args)))]
               [body (parse (second args))])
          (if (eq? (length params) (length (remover-duplicados params)))
              (fun params body)
              (error 'parse "Los parámetros de la función no deben repetirse.")))
        (error 'parse "La construcción fun debe tener la forma {fun {param1 param2 ...} body}"))]
    ['rec (if (and (list? args) (list? (first args)))
      (let* ([bindings (map (lambda (b) 
                        (if (and (list? b) (= (length b) 2)) 
                          b 
                          (error 'parse "Cada binding debe ser una lista de dos elementos"))) ;; verificamod con la lambda que cada enlace sea una lista de dos elementos. Si no es así, lanza un error 
                   (first args))]
             [body (second args)]
             [bindings-parsed (map parse-binding bindings)] ;; Mapea la función 'parse-binding' a cada enlace para parsearlos individualmente.
             [body-parsed (parse body)])
        
        (if (equal? bindings (remover-duplicados bindings)) ;; Verificamos que no haya identificadores duplicados en los enlaces.
            (rec bindings-parsed body-parsed)
            (error 'parse "Los identificadores deben ser únicos en los bindings de 'rec")))
      (error 'parse "La construcción 'rec' debe tener la forma (rec {{<id> <expr>}+} <expr>)"))]

    [else (app (parse cabeza) (map parse args))] ;; Si no es ninguno de los anteriores entonces lo tratamos como una aplicaciond de funcion
    ))
    ]))

;; remover-duplicados :: list -> list. Esta función nos sirve para verificar que no haya id's repetidos en las funciones
(define (remover-duplicados lst)
  (define (auxiliar lst res seen)
    (cond
      [(null? lst) (reverse res)] ;; Si la lista está vacía, se devuelve la lista de resultados.
      [(member (car lst) seen) (auxiliar (cdr lst) res seen)] ;; Si el primer elemento de la lista ya se ha visto, se llama a la función auxiliar con el resto de la lista.
      [else (auxiliar (cdr lst) (cons (car lst) res) (cons (car lst) seen))])) ;; Si el primer elemento de la lista no se ha visto, se añade a la lista de resultados y a la lista de elementos vistos.
  (auxiliar lst '() '())) ;; Se llama a la función auxiliar con la lista original, una lista de resultados vacía y una lista de elementos vistos vacía.

;; auxiliar para parsear bindings 
(define (parse-binding b)
  (binding (first b) (parse (second b))))
