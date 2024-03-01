add_library(Lex "")

target_sources(Lex 
    PRIVATE 
        ${CMAKE_CURRENT_LIST_DIR}/Lex.cpp
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}/Lex.hpp)

target_include_directories(Lex 
    PUBLIC
        ${LLVM_INCLUDE_DIRS} 
        ${CMAKE_CURRENT_LIST_DIR})
target_link_libraries(Lex PUBLIC Basic ${llvm_libs})
message("Lexer current directory is: ${CMAKE_CURRENT_LIST_DIR}")
