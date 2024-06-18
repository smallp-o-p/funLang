set(PARSE_HEADER_INCLUDES "${funLang_SOURCE_DIR}/include/Parse/Parse.hpp")
add_library(Parse "" Parse.cpp ${PARSE_HEADER_INCLUDES})

target_include_directories(Parse
        PUBLIC
        ${LLVM_INCLUDE_DIRS}
        ../../include)
target_link_libraries(Parse PUBLIC Lex Basic AST Sema)

message("Parser current directory is: ${CMAKE_CURRENT_LIST_DIR}")
