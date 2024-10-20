# funLang

A statically typed programming language implemented with the LLVM ecosystem created for fun.

FunLang is implemented in C++20 and heavily depends on LLVM libraries. Code generation is handled entirely by LLVM and
there is heavy use of its ADT and Support libraries. This is essentially a front-end for LLVM.

## Building

- A C++20 compiler is required.
- Supported CMake versions: 3.14 and above.
- LLVM will be automatically downloaded and installed for this project if it isn't present on your system already.
    - By default, it will only build and install LLVM for this project only. You will not be able to use this LLVM
      installation for your other projects.
- I've only successfully built this on Ubuntu Linux.

### Build Instructions

~~~
git clone https://github.com/smallp-o-p/funLang.git <directory of choice>
cd <directory of choice>/funLang
mkdir build
cd build
cmake ..
~~~

A main driver has not been implemented yet. This will only build targets and should allow you to run tests.

## Features

- Strong type system
    - No implicit type promotions; All type conversions must be explicit
    - Error return types for error handling
    - Operations (details to come)
        - Arithmetic
        - Comparisons
        - Logical
        - Bitwise
- Data Types
    - Primitives: i32, i64, f32, f64, char, string and arrays
    - Pointers
    - Construct your own data types with *structs*
    - Sum type with enums (Pattern matching with *match* expressions)
    - A basic trait system
- Control Flow
    - *If-else* statements
    - *For*, *while*, *infinite* loops

## Examples

### Fibonacci sequence

~~~
i32 fib(i32 n){
  if n == 0 {
    return 0;
  }
  elif n == 1 {
    return 1;
  }
  return fib(n-1) + fib(n-2);
}  
~~~

### Struct

~~~
struct foo {
  i32 a;
  i32 b;
}

impl foo { // <-- This is not implemented yet. 
  i32 sum(){
    return a + b;
  }
}

i32 main() {
 foo Obj = foo(1,2);
 i32 ObjSum = foo.sum();
 return 0;
}
~~~

### Pointers

~~~ 
i32 main() {
  *i32 PtrToI32 = malloc(32); // <-- malloc will be an external call to C stdlib.
  @PtrToI32 = 2;
//^ dereference operator is @
  return 0;
}
~~~
