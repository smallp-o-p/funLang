set(LEX_HEADER_LIST "${funLang_SOURCE_DIR}/include/Lex/Lex.hpp")

add_library(Lex)
target_sources(Lex PUBLIC
        FILE_SET lex_modules
        TYPE CXX_MODULES
        FILES Lex.ixx Lex.Token.ixx)
target_include_directories(Lex
        PUBLIC
        ${LLVM_INCLUDE_DIRS}
        ../../include)

target_link_libraries(Lex PUBLIC Basic ${LLVM_LIBS})
message("Lexer current directory is: ${CMAKE_CURRENT_LIST_DIR}")
