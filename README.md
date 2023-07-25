# Caescript
A dialect of the [Monkey programming language](https://monkeylang.org/) written in Rust, has both interpreted and compiled implementation

## What’s Monkey?

Monkey has a C-like syntax, supports **variable bindings**, **prefix** and **infix operators**, has **first-class** and **higher-order functions**, can handle **closures** with ease and has **integers**, **booleans**, **arrays** and **hashes** built-in.
reading through [Writing An Interpreter In Go](https://interpreterbook.com/) and [Writing A Compiler In Go](https://compilerbook.com/) for more details

## How to use
### With custom install
- Build release
```
$ cargo build --release --bin caescript
```
- Build release using the Compiler implementation
```
$ cargo build --release --features=vm  --bin caescript
```
- Running the REPL
```bash
$ ./caescript
```

- Running the Interpreter/Compiler
```bash
$ ./caescript [vm/eval] examples/hello.cae
```

### With online playground
[playground](https://caelansar.github.io/caescript-web/)

## Syntax
### Table of Contents

- [Summary](#summary)
- [Syntax overview](#syntax-overview)
    - [If](#if)
    - [For Loop](#for)
    - [Operators](#operators)
    - [Return](#return)
- [Variable bindings](#variable-bindings)
- [Literals](#literals)
    - [Integer](#integer)
    - [Float](#float)
    - [Boolean](#boolean)
    - [String](#string)
    - [Array](#array)
    - [Hashes](#hashes)
    - [Function](#function)
- [Built-in Functions](#built-in-functions)
    - [`puts(<arg1>, <arg2>, ...): Null`](#putsarg1-arg2--null)
    - [`len(<arg>): Intger`](#lenarg-intger)
    - [`first(<arg>): Object`](#firstarg-object)
    - [`last(<arg>): Object`](#lastarg-object)
    - [`rest(<arg>): Array`](#restarg-array)
    - [`push(<arg1>, <arg2>): Array`](#pusharg1-arg2-array)

### Summary
- Integers, booleans, strings, arrays, hash maps
- Arithmetic expressions
- Let statements
- First-class and higher-order functions
- Built-in functions
- Recursion
- Closures

### Syntax overview

An example of Fibonacci function.

```
let fibonacci = fn(x) {
  if (x == 0) {
    0;
  } else {
    if (x == 1) {
      1;
    } else {
      fibonacci(x - 1) + fibonacci(x - 2);
    }
  }
};

fibonacci(10);
```

#### If

It supports the general `if`. `else` exists, but` else if` does not exist.

```
if (true) {
  10;
} else {
  5;
}
```

#### For

It supports `break`/`continue` in for loop block

```
let sum = 0;
let i = 5;
for (i>0) {
    if (i == 4) {
        break;
    }
    sum += i;
    i -= 1;
}
sum // 5
```

#### Operators

It supports the general operations.

```
1 + 2 + (3 * 4) - (10 / 5);
1.0 + 2.1;
!true;
!false;
+10;
-5;
1 % 2;
true && false;
"Hello" + " " + "World";
a += 1;
```

#### Return

It returns the value immediately. No further processing will be executed.

```
if (true) {
  return;
}
```

```
let identity = fn(x) {
  return x;
};

identity("Monkey");
```

### Variable bindings

Variable bindings, such as those supported by many programming languages, are implemented. Variables can be defined using the `let` keyword.

**Format:**

```
let <identifier> = <expression>;
```

**Example:**

```
let x = 0;
let y = 10;
let foobar = add(5, 5);
let alias = foobar;
let identity = fn(x) { x };
```

### Literals

Five types of literals are implemented.

#### Integer

`Integer` represents an integer value.

**Format:**

```
[-]?[1-9][0-9]*;
```

**Example:**

```
10;
1234;
```

#### Float

`Float` represents a float value.

**Format:**

```
[-]?[1-9][0-9]*\.[1-9][0-9]*;
```

**Example:**

```
1.0;
12.34;
```

#### Boolean

`Boolean` represents a general boolean types.

**Format:**

```
true | false;
```

**Example:**

```
true;
false;

let truthy = !false;
let falsy = !true;
```

#### String

`String` represents a string. Only double quotes can be used.

**Format:**

```
"<value>";
```

**Example:**

```
"Monkey Programming Language";
"Hello" + " " + "World";
```

#### Array

`Array` represents an ordered contiguous element. Each element can contain different data types.

**Format:**

```
[<expression>, <expression>, ...];
```

**Example:**

```
[1, 2, 3 + 3, fn(x) { x }, add(2, 2), true];
```

```
let arr = [1, true, fn(x) { x }];

arr[0];
arr[1];
arr[2](10);
arr[1 + 1](10);
```

#### Hashes

`Hash` expresses data associating keys with values.

**Format:**

```
{ <expression>: <expression>, <expression>: <expression>, ... };
```

**Example:**

```
let hash = {
  "name": "cae",
  "age": 24,
  true: "a boolean",
  99: "an integer"
};

hash["name"];
hash["a" + "ge"];
hash[true];
hash[99];
hash[100 - 1];
```

#### Function

`Function` supports functions like those supported by other programming languages. Support both anonymous and named functions.

**Format:**

```
fn (<parameter one>, <parameter two>, ...) { <block statement> };
fn name(<parameter one>, <parameter two>, ...) { <block statement> };
```

**Example:**

```
let add = fn(x, y) {
  return x + y;
};

add(10, 20);
```

```
fn add(x, y) {
  x + y;
};

add(10, 20);
```

If `return` does not exist, it returns the result of the last evaluated expression.

```
let addThree = fn(x) { x + 3 };
let callTwoTimes = fn(x, f) { f(f(x)) };

callTwoTimes(3, addThree);
```

Passing around functions, higher-order functions and closures will also work.

### Built-in Functions

#### `puts(<arg1>, <arg2>, ...): Null`

It outputs the specified value to `stdout`. In the case of Playground, it is output to `console`.

```
puts("Hello");
puts("World!");
```

#### `len(<arg>): Intger`

For `String`, it returns the number of characters. If it's `Array`, it returns the number of elements.

```
len("caescript");
len([0, 1, 2]);
```

#### `first(<arg>): Object`

Returns the element at the beginning of `Array`.

```
first([0, 1, 2]);
```

#### `last(<arg>): Object`

Returns the element at the last of `Array`.

```
last([0, 1, 2]);
```

#### `rest(<arg>): Array`

Returns a new `Array` with the first element removed.

```
rest([0, 1, 2]);
```

#### `push(<arg1>, <arg2>): Array`

Returns a new `Array` with the element specified at the end added.

```
push([0, 1], 2);
```

## License
Caescript is under [MIT](LICENSE)
