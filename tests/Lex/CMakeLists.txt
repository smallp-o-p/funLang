set(CMAKE_BUILD_TYPE Debug)
include(GoogleTest)
configure_file(lex_Basic.txt ${CMAKE_CURRENT_BINARY_DIR})
configure_file(lex_IntNumLits.txt ${CMAKE_CURRENT_BINARY_DIR})
configure_file(lex_Keywords.txt ${CMAKE_CURRENT_BINARY_DIR})
configure_file(lex_RealNumLits.txt ${CMAKE_CURRENT_BINARY_DIR})
configure_file(lex_StringLits.txt ${CMAKE_CURRENT_BINARY_DIR})
configure_file(lex_ExampleFunc.txt ${CMAKE_CURRENT_BINARY_DIR})
configure_file(lex_withErrors.txt ${CMAKE_CURRENT_BINARY_DIR})

add_executable(LexTests LexTests.cpp)
target_link_libraries(LexTests PRIVATE Lex GTest::gtest_main)
add_test(LexTests LexTests)
