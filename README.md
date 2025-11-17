# Myca

Myca is a low-level language with manual memory management. First and foremost,
the goal is to develop a language that is ergonomic and fun to use. Second, I
want to experiment with compile-time memory safety so I can sleep at night.

Development is still at a very early stage. The main features currently implemented are:
- struct types that combine the functionality of both sum and product types
- union types with set-like semantics
- generics
- type inference
- order-independent type/procedure declarations (no forward-declarations needed)
- basic pattern matching
- a C backend

## Examples

```
proc factorial(n: u32) -> u32
{
	if n == 0 {
		return 1;
	}

	return n * factorial(n - 1);
}
```

An generic `Option` type similar to the one from Rust and other languages:

```
// Structs in Myca can be used for both sum and product types. Here, we create a
// sum type.
struct Option'T
{
    // By marking this variant as implicit, we can assign values of type T
    // directly to Option'T without having to wrap it in Option.Some
    case implicit Some{value: T},

    case None,
}

// In contrast to Rust, the different variants of a sum type are represented by
// their own type.
// Since Option'(T).Some and Option'(T).None are used so often, we create type
// aliases for them:
typealias Some'T = Option'(T).Some;
typealias None'T = Option'(T).None;

// ?T is a shorthand for Option'T
proc unwrap_or'T(opt: ?T, fallback: T) -> T
{
	match opt {
		case Some(let value) { return value; }
		case None { return fallback; }
	}
}

proc safe_div(a: i32, b: i32) -> ?i32
{
    if b == 0 {
        return None;
    }

    return a / b; // Equivalent to Some(a / b)
}

proc main() -> i32
{
	return unwrap_or(safe_div(13, 2), -1);
}
```

Defining a simple AST for expressions:

```
struct Type
{
    case Bool,
    case Int,
}

struct BinaryOp
{
    case Add,
    case Sub,
    case Mul,
    case Div,

    case And,
    case Or,
}

struct Expr
{
    case Bool{value: bool},
    case Int{value: i32},
    case Binary{left: ^Expr, right: ^Expr, op: BinaryOp},

    // `type` is automatically added to the constructors of the above cases
    type: ?Type = None,
}
```

## Building

The only dependencies are CMake and a C++23 compiler (tested with GCC and
clang). Additionally, Python is needed to run the tests.

```sh
git clone https://github.com/david-k/myca/
cd myca
cmake -B build
cmake --build build

# Running the tests
make test -C build/

# Compiling a *.myca file
./build/myca hello.myca -o hello.c && gcc hello.c -o hello
```
