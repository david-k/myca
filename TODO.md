- **Misc:**
  - Create a context-free grammar and check for ambiguity using a LL or LR
    parser generator.
    https://www.chiark.greenend.org.uk/~sgtatham/quasiblog/parsing/

- **Language:**
  - The following should not be allowed:
    ```myca
    struct S
    {
        case T {},
        case U {},
    }

    let mut t = S.T();
    let p: ^mut S = &mut t;
    p^ := S.U(); // violates type safety: `t` is of type `S.T` but now contains a value of type `S.U`
    ```

  - If `foo` is a proc, then `proc.PARAMS` should refer to a struct representing
    the procs parameters. If we also support struct splatting, the following
    becomes possible:

    ```myca
    proc foo(a: i32, b: i8 = 3, c: bool = false) { ... }

    let params = foo.PARAMS(a: 999);
    // Complex logic filling out the remaing parameters
    foo(params...);
    ```

  - Integrate `is_cast_ok()` into the unifier
  - Support nested pattern matching
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
