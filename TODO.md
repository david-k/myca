# Open
- **Syntax:**
  - The following is ambiguous:
    ```
    proc foo() -> struct X {
        ...
    }
    ```
    The braces could either belong to `foo` or to `X`

  - Improve ergonomics of mutable variables/references: `&mut` ⇒ `!` and
    `let mut` ⇒ `mut`
  - Use braces instead of parens for type parameter list:
    `Option'(i32)` ⇒ `Option'{i32}`

- **Diagnostics:**
  - For better error messages, it should be possible to map a TypeDeductionVar
    back to the TypeParameterVar it was generated for

- **Semantics:**
  - Integrate `is_cast_ok()` into the unifier
  - Support nested pattern matching

- **Codegen (C):**
  - Assign a unique ID to each variable. Solves the following issue:

    ```c
    01    int hello = 3;
    02    {
    03        // The reference to `hello` on the RHS refers to the declaration on
    04        // line 5. In other words, we are initializing `hello` with itself.
    05        int hello = hello;
    06    }
    ```

# Closed
