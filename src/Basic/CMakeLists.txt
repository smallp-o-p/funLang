add_library(Basic "")

target_sources(Basic
        PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/Diag.cpp
        ${CMAKE_CURRENT_LIST_DIR}/TokenTags.cpp
        ${CMAKE_CURRENT_LIST_DIR}/OperationKinds.cpp
        PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}/Diag.hpp
        ${CMAKE_CURRENT_LIST_DIR}/TokenTags.hpp
        ${CMAKE_CURRENT_LIST_DIR}/OperationKinds.hpp
)

target_include_directories(Basic
        PUBLIC
        ${CMAKE_CURRENT_LIST_DIR})
message("Basic current directory is: ${CMAKE_CURRENT_LIST_DIR}")
