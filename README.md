# README: Intérprete de un Lenguaje Funcional en Racket

Un intérprete para un pequeño lenguaje funcional con soporte para funciones de primera clase, recursión mutua, operaciones básicas, y estructuras de control, implementado en Racket.

---

## Características Principales
- **Parsing de expresiones** con sintaxis similar a Scheme/Racket.
- **Tipos de datos**: números, booleanos, strings, y funciones.
- **Operaciones integradas**: `sub1`, `add1`, `zero?`, `number?`, `modulo`, `min`, `+`, `*`, `<`, `/`, etc.
- **Funciones anónimas** (`fun`) con closures que capturan el entorno léxico.
- **Estructuras de control**: `if` condicional.
- **Recursión mutua** con `rec` para definir variables vinculadas mutuamente.
- **Manejo de errores**: 
  - Variables libres no definidas.
  - Aridad incorrecta en llamadas a funciones.
  - Parámetros duplicados en funciones.

---

## Ejemplos de Uso

### Funciones y Closures
```racket
{{fun {x y} {+ x y y}} 1 10}  ; Evalúa a 21
{{fun {x} {{fun {y} {+ x y}} {+ x x}}} 10}  ; Evalúa a 30
```

### Recursión Mutua (`rec`)
```racket
{rec {[x 2] [y 3]} {+ x y}}       ; Evalúa a 5
{rec {[a 10] [b {+ a 5}]} {* a b}} ; Evalúa a 150
```

### Condicionales
```racket
{if {zero? 0} "Sí" "No"}  ; Evalúa a "Sí"
{if {and {zero? 0} {< 1 10}} "Verdad" "Falso"}  ; Evalúa a "Verdad"
```

---

## Instalación y Ejecución
1. **Requisitos**: [Racket](https://racket-lang.org/) (v8.x o superior).
2. Clona el repositorio:
   ```bash
   git clone https://github.com/tu-usuario/mini-racket-interpreter.git
   ```
3. Ejecuta las pruebas incluidas:
   ```bash
   racket test.rkt
   ```

---

## Estructura del Proyecto
- `grammars.rkt`: Define la gramática abstracta (AST) del lenguaje.
- `parser.rkt`: Implementa el parser para convertir expresiones en nodos del AST.
- `interp.rkt`: Contiene el intérprete que evalúa las expresiones.
- `test.rkt`: Suite de pruebas para verificar el comportamiento del intérprete.

---

## Detalles Técnicos
- **Entornos léxicos**: Se manejan con `mt-env` (entorno vacío) y `cons-env` (entorno extendido).
- **Closures**: Las funciones capturan el entorno en el que se definen.
- **Recursión mutua**: `rec` utiliza `cons-rec-env` para vincular variables en un entorno compartido.

---

## Licencia
MIT License. Ver [LICENSE](LICENSE) para más detalles.