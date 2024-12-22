include(GoogleTest)

add_executable(ScopeTest ScopeTests.cpp)
target_link_libraries(ScopeTest PRIVATE Sema GTest::gtest_main)

add_test(ScopeTests ScopeTest)
