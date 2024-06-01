set(CMAKE_BUILD_TYPE Debug)
include(GoogleTest)
configure_file(fnTest1.fun ${CMAKE_CURRENT_BINARY_DIR})
configure_file(fnTest2.fun ${CMAKE_CURRENT_BINARY_DIR})
configure_file(compilationUnit.fun ${CMAKE_CURRENT_BINARY_DIR})
configure_file(bigCompilationUnit.fun ${CMAKE_CURRENT_BINARY_DIR})
configure_file(assignExpr.fun ${CMAKE_CURRENT_BINARY_DIR})
configure_file(./structTest.fun ${CMAKE_CURRENT_BINARY_DIR})
configure_file(cmpExpr.fun ${CMAKE_CURRENT_BINARY_DIR})

add_executable(ParseTests EXCLUDE_FROM_ALL ParseTests.cpp)

target_link_libraries(ParseTests PRIVATE Parse GTest::gtest_main)
add_test(ParseTests ParseTests)
