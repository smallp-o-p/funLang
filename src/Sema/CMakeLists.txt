set(SEMA_HEADER_LIST "${funLang_SOURCE_DIR}/include/Sema/Sema.hpp")
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_library(Sema "" Sema.cpp ${SEMA_HEADER_LIST})

target_include_directories(Sema
        PUBLIC
        ${LLVM_INCLUDE_DIRS}
        ../../include
)
target_link_libraries(Sema PUBLIC AST)
message("Sema current directory is: ${CMAKE_CURRENT_LIST_DIR}")
