include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_subdirectory(Lex)
add_subdirectory(Parse)
add_subdirectory(Sema)
add_subdirectory(Basic)
add_subdirectory(AST)
