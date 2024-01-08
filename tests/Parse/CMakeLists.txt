configure_file(./FnsParseTest.txt ${CMAKE_CURRENT_BINARY_DIR})

include_directories(${CMAKE_SOURCE_DIR}/src/includes)

add_executable(FnsParse EXCLUDE_FROM_ALL ParseFns.cpp)

target_link_libraries(FnsParse PUBLIC Lex)
target_link_libraries(FnsParse PUBLIC Parse)
target_link_libraries(FnsParse PUBLIC AST_stuff)
target_link_libraries(FnsParse PUBLIC ${llvm_libs})
add_test(FunctionsParse FnsParse)

add_custom_target(ParseTests COMMAND ${CMAKE_CTEST_COMMAND} DEPENDS FnsParse)
