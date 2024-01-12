if(BUILD_TESTING)
  include(GoogleTest)
  include_directories(${CMAKE_SOURCE_DIR}/src/includes)
  configure_file(lex_Basic.txt ${CMAKE_CURRENT_BINARY_DIR})
  configure_file(lex_IntNumLits.txt ${CMAKE_CURRENT_BINARY_DIR})
  configure_file(lex_Keywords.txt ${CMAKE_CURRENT_BINARY_DIR})
  configure_file(lex_RealNumLits.txt ${CMAKE_CURRENT_BINARY_DIR})
  configure_file(lex_StringLits.txt ${CMAKE_CURRENT_BINARY_DIR})
  configure_file(lex_ExampleFunc.txt ${CMAKE_CURRENT_BINARY_DIR})

  add_executable(LexTesting EXCLUDE_FROM_ALL LexTests.cpp)

  target_link_libraries(LexTesting PRIVATE Lex)
  target_link_libraries(LexTesting PRIVATE GTest::gtest_main)

  add_test(LexTesting LexTesting)
  gtest_discover_tests(LexTesting)
endif()
