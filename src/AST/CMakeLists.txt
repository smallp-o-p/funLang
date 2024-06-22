set(HEADER_LIST "${funLang_SOURCE_DIR}/include/AST/AST.hpp")
add_library(AST STATIC AST.cpp ${HEADER_LIST})

target_include_directories(AST PUBLIC ../../include)
target_link_libraries(AST PUBLIC Sema)
