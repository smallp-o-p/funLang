set(CMAKE_BUILD_TYPE Debug)
include(GoogleTest)
set(DEFINE_LIST -DBASIC="${CMAKE_CURRENT_LIST_DIR}/lex_Basic.txt"
    -DINTNUMLITS="${CMAKE_CURRENT_LIST_DIR}/lex_IntNumLits.txt"
    -DKEYWORDS="${CMAKE_CURRENT_LIST_DIR}/lex_Keywords.txt"
    -DREALNUMLITS="${CMAKE_CURRENT_LIST_DIR}/lex_RealNumLits.txt"
    -DSTRINGLITS="${CMAKE_CURRENT_LIST_DIR}/lex_StringLits.txt"
    -DERRORS="${CMAKE_CURRENT_LIST_DIR}/lex_withErrors.txt"
    -DFUNC="${CMAKE_CURRENT_LIST_DIR}/lex_ExampleFunc.txt")

add_executable(LexTests LexTests.cpp)
target_link_libraries(LexTests PRIVATE Lex GTest::gtest_main)
add_compile_definitions(${DEFINE_LIST})
add_test(LexTests LexTests)
