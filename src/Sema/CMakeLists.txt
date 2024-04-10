include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_library(Sema "")
target_sources(
        Sema
        PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/Visitor.cpp
        ${CMAKE_CURRENT_LIST_DIR}/Visitor.hpp
        PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}/Visitor.hpp
)

target_include_directories(Sema
        PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
)
target_link_libraries(Sema PUBLIC Parse)
message("Sema current directory is: ${CMAKE_CURRENT_LIST_DIR}")
