# Scheme Simple Calculator

This is a simple calculator implemented in Scheme. It supports:

- Arithmetic operations: `+`, `-`, `*`, `/`
- Variable assignment using `(set! x 5)`
- Variable usage in expressions: `(+ x 2)`
- Nested expressions and recursion
- Basic error handling for undefined variables and invalid input

## How to Run

1. Start the calculator:
   ```bash
   scheme --load calculator.scm
   ```

2. Type an expression like:
   ```
   (* (+ 1 2) 3)
   ```

## Supported Expressions (Examples)

```scheme
(+ 2 3)            ; => 5
(- 10 4)           ; => 6
(* 3 4)            ; => 12
(/ 10 2)           ; => 5
(* (+ 1 2) 3)      ; nested expression => 9
```