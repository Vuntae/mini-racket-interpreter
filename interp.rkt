#lang plai

(require (file "./grammars.rkt"))


;; Definimos los tipos de valores que nuestro intérprete puede manejar
(define-type Value
  [numV (literal number?)]  ;; Números
  [boolV (literal boolean?)]  ;; Booleanos
  [strV (literal string?)]  ;; Cadenas de texto
  [closureV (param (listof symbol?)) (cuerpo CFSBAE?) (env Env?)])

  
(define (boxed-Value? v)
  (and (box? v) (Value? (unbox v))))

;; Definimos los tipos de entornos que nuestro intérprete puede manejar
(define-type Env
  [mt-env]  ;; Entorno vacío
  [cons-env (id symbol?) (valor Value?) (resto Env?)]
  [cons-rec-env (id symbol?) (valor boxed-Value?) (resto Env?)])  ;; Nuevo tipo de entorno para manejar la recursión)


;; Función principal de interpretación
(define (interp ast env)
  (type-case CFSBAE ast
    [id (i) (lookup i env)]  ;; Si es un identificador, buscamos su valor en el entorno
    ;; Las literales, las expresiones de un solo elemento, las devolvemos como tal
    [num (literal) (numV literal)]  
    [bool (literal) (boolV literal)]  
    [str (literal) (strV literal)]  
    [op (operator args)  ;; Si es una operación, evaluamos los argumentos y aplicamos el operador
         (let ([arg-values (map (lambda (arg) (Value->primitive (interp arg env))) args)])
           (primitive->Value (apply operator arg-values)))]
    [fun (param body) (closureV param body env)]  ;; Si es una función, creamos un cierre con los parámetros, el cuerpo de la función y el entorno actual
    [with (binding body) (let* ([id (first binding)]  ;; Si es una construcción 'with', creamos un nuevo entorno con el valor de la expresión asociado al identificador y evaluamos el cuerpo en ese nuevo entorno
                            [value (interp (second binding) env)]
                            [new-env (cons-rec-env id value env)])
                        (interp body new-env))]
    [rec (bindings body)
         (let* ([env-rec (actualizar-ambiente bindings env)])  ;; Usamos la función actualizar-ambiente para manejar la recursión
           (interp body env-rec))]
    [app (fun argumento)  ;; Si es una aplicación de función, evaluamos la función y los argumentos, y luego aplicamos la función a los argumentos
     (let* ([closure (interp fun env)]
            [cuerpo-closure (closureV-cuerpo closure)]
            [param-closure (closureV-param closure)]
            [env-closure (closureV-env closure)]
            [arg-values (map (lambda (arg) (interp arg env)) argumento)]) ;; Evalúa los argumentos de la función en el ambiente 
       (if (not (= (length param-closure) (length arg-values)))
           (error 'interp "Número incorrecto de argumentos");; Verificamos que la función recibe los argumentos que pide 
           (let ([new-env (crear-ambiente-aux param-closure arg-values env-closure)])
             (interp cuerpo-closure new-env))))]
    [iF (test-expr then-expr else-expr) (interp-iF-aux test-expr then-expr else-expr env)]))  ;; Si es una construcción 'if', evaluamos la expresión de prueba y luego evaluamos la rama correspondiente según el resultado


;; Funciones auxiliares

;; cyclically-bind-and-interp
(define (cyclically-bind-and-interp id value env)
  (let ([contenedor (box (numV 1))]) ;; Usamos un valor arbitrario para inicializar la caja
    (let ([ambiente (cons-rec-env id contenedor env)])
      (let ([valor (interp value ambiente)])
        (set-box! contenedor valor) ;; Actualizamos la caja con el valor interpretado
        ambiente))))

;; Actualizar ambiente 
(define (actualizar-ambiente bindings env)
  (if (null? bindings)
      env  ;; si no hay bindings, devolvemos el entorno actualizado
      (let* ([binding (car bindings)]
             [id (binding-id binding)]
             [valor (binding-value binding)])  
        (let ([nuevo-env (if (fun? valor) ;; Decide cómo actualizar el entorno basado en si el valor es una función.
                             (cyclically-bind-and-interp id valor env) ;; Si el valor es una función, utiliza 'cyclically-bind-and-interp' para manejar la recursión.
                             (cons-env id (interp valor env) env))]) ;; Si el valor no es una función, interpreta el valor y añade el nuevo enlace al entorno.
          (actualizar-ambiente (cdr bindings) nuevo-env)))))

(define (interp-args args env) ;; Función auxiliar para interpretar argumentos
  (if (null? args)
      '()  
      (let ((first-arg (interp (car args) env))  ;; Interpreta el primer argumento
            (rest-args (interp-args (cdr args) env)))  ;; Interpreta el resto de los argumentos recursivamente
        (cons first-arg rest-args))))  


(define (lookup search-id env) ;; Función auxiliar para buscar un identificador en el entorno
  (type-case Env env
    [mt-env () (error 'interp (string-append "Variable libre " (symbol->string search-id)))]
    [cons-env (i v r) (if (equal? i search-id)  ;; Si el identificador coincide con el buscado, devuelve su valor
                          v
                          (lookup search-id r))] ;; Si no coincide, busca en el resto del entorno
    [cons-rec-env (i v r) (if (equal? i search-id)
                          (unbox v) 
                          (lookup search-id r))]))  

(define (primitive->Value p) ;; Función auxiliar para convertir un valor primitivo a un valor de nuestro intérprete
  (cond
    [(number? p) (numV p)]  ;; Si es un número, devuelve un número de nuestro intérprete
    [(boolean? p) (boolV p)]))  ;; Si es un booleano, devuelve un booleano de nuestro intérprete


(define (Value->primitive v) ;; Función auxiliar para convertir un valor de nuestro intérprete a un valor primitivo
  (type-case Value v
    [numV (l) l] 
    [boolV (l) l]  
    [else (error 'interp "Closure usado como valor")]))  ;; Si es un cierre, lanza un error


(define (cond->if condiciones else-expr)
  (match condiciones
    ['() else-expr] 
    [(cons x xs) (iF (condition-test-expr x)  
                     (condition-then-expr x)
                     (cond->if xs else-expr))]))


(define (crear-ambiente-aux params args env) ;; Función auxiliar para crear un nuevo entorno a partir de una lista de parámetros y argumentos
  (if (null? params)
      env
      (let* ([param (car params)]  ;; Toma el primer parámetro
             [arg (car args)]  ;; Toma el primer argumento
             [resto-params (cdr params)]  ;; Toma el resto de los parámetros
             [resto-args (cdr args)])  ;; Toma el resto de los argumentos
        (cons-env param arg (crear-ambiente-aux resto-params resto-args env)))))  ;; Crea un nuevo entorno con el primer parámetro asociado al primer argumento y el resto del entorno creado recursivamente

;; Función auxiliar para interpretar una construcción 'if'
(define (interp-iF-aux test-expr then-expr else-expr env)
  (let ([test-result (interp test-expr env)])  ;; Interpreta la expresión de prueba
    (cond [(equal? test-result (boolV #t)) (interp then-expr env)]  ;; Si el resultado es verdadero, interpreta la expresión 'then'
          [(equal? test-result (boolV #f)) (interp else-expr env)])))  ;; Si el resultado es falso, interpreta la expresión 'else'
