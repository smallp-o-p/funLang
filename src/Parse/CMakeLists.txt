add_library(Parse STATIC ParseDecl.cpp ParseExpr.cpp ParseStmt.cpp)

target_include_directories(Parse
        PUBLIC
        ${LLVM_INCLUDE_DIRS}
        ../../include)

target_sources(Parse PRIVATE
        FILE_SET parse_modules
        TYPE CXX_MODULES
        FILES Parse.cppm)

target_link_libraries(Parse PUBLIC Lex Basic AST Sema)

message("Parser current directory is: ${CMAKE_CURRENT_LIST_DIR}")
