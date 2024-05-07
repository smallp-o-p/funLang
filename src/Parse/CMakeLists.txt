add_library(AST "")
target_sources(
        AST
        PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/AST.cpp
        PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}/AST.hpp
)

target_link_libraries(AST PUBLIC Lex Basic Sema)

add_library(Parse "")
target_sources(
        Parse
        PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/Parse.cpp
        PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}/Parse.hpp
)

target_include_directories(Parse
        PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
)
target_link_libraries(Parse PUBLIC Lex Basic AST Sema)

message("Parser current directory is: ${CMAKE_CURRENT_LIST_DIR}")
