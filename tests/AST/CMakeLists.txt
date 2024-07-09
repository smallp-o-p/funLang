add_executable(ASTTest astTests.cpp)
include(GoogleTest)
target_link_libraries(ASTTest PRIVATE AST GTest::gtest_main)

add_test(ASTTest ASTTest)