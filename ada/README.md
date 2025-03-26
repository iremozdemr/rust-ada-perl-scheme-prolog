# ADA Simple Calculator

This is a basic calculator program written in Ada. It supports the following operations:

- Addition (`+`)
- Subtraction (`-`)
- Multiplication (`*`)
- Division (`/`), with division-by-zero handling
- Works with **floating point numbers**

## Example

```
enter the first number: 2
enter the second number: 2.3
enter the operator (+, -, *, /): +
result: 4.30
```

## How to Run Locally

1. Navigate to the `ada` directory:
   ```bash
   cd ada
   ```

2. Compile the program using GNAT:
   ```bash
   gnatmake calculator.adb
   ```

3. Run the compiled executable:
   ```bash
   ./calculator
   ```

## Run Online

You can also run this code using an online Ada compiler:

[Run on JDoodle](https://www.jdoodle.com/execute-ada-online)