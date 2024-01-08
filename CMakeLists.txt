cmake_minimum_required(VERSION 3.1-3.8)
project(funLang)

set(CMAKE_CXX_STANDARD 20)
include(CTest)
find_package(LLVM REQUIRED CONFIG)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
llvm_map_components_to_libnames(llvm_libs support core irreader)

message(STATUS "Found LLVM! ${LLVM_PACKAGE_VERSION}")
add_subdirectory(src)
add_subdirectory(tests)
set(Headers "${CMAKE_SOURCE_DIR}/src/includes")
