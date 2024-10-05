set(BASIC_HEADER_LIST "${funLang_SOURCE_DIR}/include/Basic/Basic.hpp"
        Basic.ixx
        IdentifierTable.ixx
        Diag.ixx)
set(DIAG_HEADER_LIST "${funLang_SOURCE_DIR}/include/Basic/Diag.hpp")
add_library(Basic ""
        Basic.cpp
        ${BASIC_HEADER_LIST})

target_sources(Basic PUBLIC
        FILE_SET basic_modules
        TYPE CXX_MODULES
        FILES Basic.ixx IdentifierTable.ixx Diag.ixx)

target_include_directories(Basic
        PUBLIC
        ../../include
        ../../include/Basic/defs
)
message("Basic current directory is: ${CMAKE_CURRENT_LIST_DIR}")
