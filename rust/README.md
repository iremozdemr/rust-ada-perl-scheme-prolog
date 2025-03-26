# Rust Calculator Interpreter

This is a simple calculator interpreter implemented in Rust. It supports:

- Arithmetic operations: `+`, `-`, `*`, `/`
- Variable assignments: `x = 5`
- Variable usage: `x + 2`
- Parentheses: `(x + 3) * 2`
- Unary minus: `-x`
- Error handling (e.g., division by zero, undefined variable)

## How to Run

1. Compile the Rust file:
   ```bash
   rustc calculator.rs -o out
   ```

2. Run the program:
   ```bash
   ./out
   ```

3. Type expressions such as:
   ```
   x = 5
   x + 2
   (x + 3) * 2
   2+3
   2 + 3
   2 +3
   -2+3
   (7*6)-2+3
   ```

4. Type `exit` to quit the program.