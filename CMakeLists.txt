cmake_minimum_required(VERSION 3.14)
project(funLang
    VERSION 0.0.1
    DESCRIPTION "A beginner's programming language"
)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(CTest)
include(FetchContent)

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(llvm_libs support core irreader)
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/refs/tags/v1.14.0.zip
)
FetchContent_MakeAvailable(googletest)

message(STATUS "Found LLVM! ${LLVM_PACKAGE_VERSION}")
add_subdirectory(src)
add_subdirectory(tests)
