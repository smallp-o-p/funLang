add_library(Parse "")
target_sources(
    Parse 
    PRIVATE 
        ${CMAKE_CURRENT_LIST_DIR}/Parse.cpp
        ${CMAKE_CURRENT_LIST_DIR}/AST.cpp
    PUBLIC 
        ${CMAKE_CURRENT_LIST_DIR}/Parse.hpp
        ${CMAKE_CURRENT_LIST_DIR}/AST.hpp
    )

target_include_directories(Parse 
    PUBLIC 
        ${CMAKE_CURRENT_LIST_DIR}
)
target_link_libraries(Parse PUBLIC Lex Basic)

message("Parser current directory is: ${CMAKE_CURRENT_LIST_DIR}")
