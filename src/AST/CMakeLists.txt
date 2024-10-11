add_library(AST STATIC Type.cpp Stmt.cpp Decl.cpp)

target_include_directories(AST PUBLIC "../../include" ${LLVM_INCLUDE_DIR})

target_link_libraries(AST PUBLIC Sema Basic ${llvm_libs})
