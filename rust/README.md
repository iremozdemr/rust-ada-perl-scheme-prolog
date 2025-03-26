# Rust Simple Calculator

This is a basic calculator program written in Rust. It supports the following operations:

- Arithmetic operations: `+`, `-`, `*`, `/`
- Variable assignments: `x = 5`
- Variable usage: `x + 2`
- Parentheses
- Unary minus:
- Error handling (e.g., division by zero, undefined variable)

## How to Run

1. Install rust (https://rustup.rs)

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