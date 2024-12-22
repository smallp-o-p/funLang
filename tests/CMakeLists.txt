  include_directories(
          "
          ${CMAKE_SOURCE_DIR}/src/Lex" "${CMAKE_SOURCE_DIR}/src/Parse")
  add_subdirectory(Lex)
  add_subdirectory(Parse)
  add_subdirectory(AST)
  add_subdirectory(Sema)
  include(GoogleTest)
  add_custom_target(check-funlang DEPENDS ParseTests LexTests ASTTest)
