set(SEMA_HEADER_LIST "${funLang_SOURCE_DIR}/include/Sema/Sema.hpp")
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_library(Sema)

target_sources(Sema PRIVATE Sema.cpp)
target_sources(Sema PUBLIC
        FILE_SET sema_modules
        TYPE CXX_MODULES
        FILES Sema.cppm Scope.cppm
)
target_include_directories(Sema
        PUBLIC
        ${LLVM_INCLUDE_DIRS}
)
target_link_libraries(Sema PUBLIC AST Lex Basic)
message("Sema current directory is: ${CMAKE_CURRENT_LIST_DIR}")
