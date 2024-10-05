set(LEX_HEADER_LIST "${funLang_SOURCE_DIR}/include/Lex/Lex.hpp")
add_library(Lex "" Lex.cpp ${LEX_HEADER_LIST})

target_include_directories(Lex
        PUBLIC
        ${LLVM_INCLUDE_DIRS}
        ../../include)
target_link_libraries(Lex PUBLIC Basic ${llvm_libs})
message("Lexer current directory is: ${CMAKE_CURRENT_LIST_DIR}")
