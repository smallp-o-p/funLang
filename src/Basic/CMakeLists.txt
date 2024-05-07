add_library(Basic ""
        Basic.cpp
        Basic.hpp)

target_sources(Basic
        PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/Diag.cpp
        ${CMAKE_CURRENT_LIST_DIR}/Basic.cpp
        PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}/Diag.hpp
        ${CMAKE_CURRENT_LIST_DIR}/Basic.hpp
)

target_include_directories(Basic
        PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
        ${CMAKE_CURRENT_LIST_DIR}/defs
)

message("Basic current directory is: ${CMAKE_CURRENT_LIST_DIR}")
