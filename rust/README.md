# Rust Calculator Interpreter

This is a simple calculator interpreter implemented in Rust. It supports:

- Arithmetic operations: `+`, `-`, `*`, `/`
- Variable assignments: `x = 5`
- Variable usage: `x + 2`
- Parentheses: `(x + 3) * 2`
- Unary minus: `-x`
- Error handling (e.g., division by zero, undefined variable)

## How to Run

1. Install rust:
    ```bash
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    ```

2. Compile the Rust file:
   ```bash
   rustc calculator.rs -o out
   ```

3. Run the program:
   ```bash
   ./out
   ```

4. Type expressions such as:
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

5. Type `exit` to quit the program.