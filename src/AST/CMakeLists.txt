add_library(AST)
target_sources(AST PUBLIC
        FILE_SET ast_modules
        TYPE CXX_MODULES
        FILES AST.cppm Decl.cppm Type.cppm Stmt.cppm Expr.cppm)
target_sources(AST PRIVATE Stmt.cpp Expr.cpp Decl.cpp Type.cpp)

target_include_directories(AST PUBLIC ${LLVM_INCLUDE_DIRS} ../../include)

target_link_libraries(AST PUBLIC Basic ${LLVM_LIBS})
