cmake_minimum_required(VERSION 3.14)
project(funLang
    VERSION 0.0.1
    DESCRIPTION "A beginner's programming language"
)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if("${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")
  message("Using Clang")
  add_compile_options(-fstandalone-debug)
elseif("${CMAKE_CXX_COMPILER_ID}" MATCHES "GNU")
  message("Using GCC")
endif()

enable_testing()
include(CTest)
include(FetchContent)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "LLVM found: ${LLVM_PACKAGE_VERSION}")
message(STATUS "LLVM includes: ${LLVM_INCLUDE_DIRS}")
message(STATUS "LLVM definitions: ${LLVM_DEFINITIONS}")
message(STATUS "LLVM tools: ${LLVM_TOOLS_BINARY_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
set(CMAKE_EXPORT_COMPILE_COMMANDS ON )

llvm_map_components_to_libnames(llvm_libs support core irreader)
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/refs/tags/v1.14.0.zip
)
FetchContent_MakeAvailable(googletest)
add_subdirectory(src)
add_subdirectory(tests)
