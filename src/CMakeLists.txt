add_library(Lex Lex.cpp)
add_library(Parse Parse.cpp)
add_library(AST_stuff ${CMAKE_CURRENT_SOURCE_DIR}/AST/AST.cpp)
add_subdirectory(AST)
include_directories(includes)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
llvm_map_components_to_libnames(llvm_libs support core irreader)
message(STATUS "LLVM linked!")

target_include_directories(AST_stuff PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/includes)
target_include_directories(Lex PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/includes)
