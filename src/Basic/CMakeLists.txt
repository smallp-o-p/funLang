set(BASIC_HEADER_LIST "${funLang_SOURCE_DIR}/include/Basic/Basic.hpp")
set(DIAG_HEADER_LIST "${funLang_SOURCE_DIR}/include/Basic/Diag.hpp")
add_library(Basic ""
        Basic.cpp
        ${BASIC_HEADER_LIST})

add_library(Diag ""
        Diag.cpp
        ${DIAG_HEADER_LIST})

target_include_directories(Basic
        PUBLIC
        ../../include
        ../../include/Basic/defs
)
target_include_directories(Diag
        PUBLIC
        ../../include
)
message("Basic current directory is: ${CMAKE_CURRENT_LIST_DIR}")
