cmake_minimum_required(VERSION 3.1-3.8)
project(funLang)

set(CMAKE_CXX_STANDARD 20)
include(CTest)
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM! ${LLVM_PACKAGE_VERSION}")
add_subdirectory(src)
add_subdirectory(tests)
set(Headers "${CMAKE_SOURCE_DIR}/src/includes")
