# funLang
A statically typed programming language implemented with the LLVM ecosystem created for fun.

FunLang is implemented with LLVM's srcManager for the front-end stuff like error reporting and source file management. I expect it to go through a few rounds of MLIR to represent some of the cool features I have in mind before finally emitting LLVM IR that can be run where LLVM is supported.  

Rest of the stuff to come lol
# Scope of The Project 

To keep this language simple and easy enough to follow, the set of features
will be basic, imagine C with some creature-comforts that I really like picked from other languages like Rust and Zig.

## Features (to implement)❗

- Strong type system
  - Error return types for error handling
- Operations
  - All the basic ones you expect, arithmetic, comparisons
- Data Types
  - Primities: i32, i64, f32, f64, char, string and arrays 
  - Construct your own data types with *structs*
  - Sum type with enums
  - A basic trait system
- Control Flow
  - *If-else* statements
  - *For*, *while*, *infinite* loops    

## Examples

soon (tm)


## Current Progress/The Plan 📝

| Section                           | Implementation            | Tests                     | Documentation/Write-up |
|-----------------------------------|---------------------------|---------------------------|------------------------|
| Lexer                             | Done ✅                   | Done ✅                   | In Progress ⚠️          |
| Parser/Syntax-Driven Translation thing                            | In Progress ⚠️            | In Progress  ⚠️                 | Not Started ❌         |
| Code Generation                   | In Progress ⚠️            | Not Started ❌           | Not Started ❌         |
| Not Thrown-together System Design | In Progress ⚠️ (Perpetual) | In Progress ⚠️ (Perpetual) |                        |
