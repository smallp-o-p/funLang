include(GoogleTest)
configure_file(./FnsParseTest.txt ${CMAKE_CURRENT_BINARY_DIR})
configure_file(./FnsParseTest2.txt ${CMAKE_CURRENT_BINARY_DIR})
configure_file(./FnsParseTest3.txt ${CMAKE_CURRENT_BINARY_DIR})
configure_file(./FnsParseTest4.fun ${CMAKE_CURRENT_BINARY_DIR})
configure_file(./FnsParseTest5.fun ${CMAKE_CURRENT_BINARY_DIR})

add_executable(ParseTests EXCLUDE_FROM_ALL ParseTests.cpp)

target_link_libraries(ParseTests PRIVATE GTest::gtest_main)
target_link_libraries(ParseTests PRIVATE Parse)
gtest_discover_tests(ParseTests)
add_test(ParseTests ParseTests)

get_property(dirs DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY INCLUDE_DIRECTORIES)
