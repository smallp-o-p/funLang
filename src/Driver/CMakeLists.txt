set(DRIVER_HEADER_INCLUDES "${funLang_SOURCE_DIR}/include/Driver/Driver.hpp")
add_executable(Drive "" Driver.cpp ${DRIVER_HEADER_INCLUDES})

target_include_directories(Drive
            PUBLIC
            ${LLVM_INCLUDE_DIRS}
            ../../include)

target_link_libraries(Drive PUBLIC Lex Parse AST Sema)

message("Driver current directory is: ${CMAKE_CURRENT_LIST_DIR}")

