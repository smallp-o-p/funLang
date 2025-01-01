add_library(Lex)
target_sources(Lex PUBLIC
        FILE_SET lex_modules
        TYPE CXX_MODULES
        FILES Lex.cppm Lex.Token.cppm)
target_include_directories(Lex
        PUBLIC
        ../../include
        ${LLVM_INCLUDE_DIRS})

target_link_libraries(Lex PUBLIC Basic Diag ${LLVM_LIBS})
message("Lexer current directory is: ${CMAKE_CURRENT_LIST_DIR}")
