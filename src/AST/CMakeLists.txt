add_library(AST Decl.cpp Stmt.cpp Expr.cpp Type.cpp)

target_sources(AST PUBLIC
        FILE_SET ast_modules
        TYPE CXX_MODULES
        FILES AST.cppm Decl.cppm Type.cppm Stmt.cppm Expr.cppm)

target_include_directories(AST PUBLIC ${LLVM_INCLUDE_DIRS})

target_link_libraries(AST PUBLIC Sema Basic ${LLVM_LIBS})
