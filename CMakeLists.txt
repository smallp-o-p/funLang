cmake_minimum_required(VERSION 3.28 FATAL_ERROR)
project(funLang
        VERSION 0.0.1
        DESCRIPTION "A somewhat fun programming language"
        LANGUAGES C CXX
)

set(CMAKE_EXPORT_COMPILE_COMMANDS)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_SCAN_FOR_MODULES ON)
set_property(GLOBAL PROPERTY USE_FOLDERS ON)
add_compile_options(-fno-rtti)

set(CMAKE_CXX_FLAGS "-Wall -Wextra")
set(CMAKE_CXX_FLAGS_RELEASE "-O2")
if(NOT CMAKE_BUILD_TYPE)
        set(CMAKE_BUILD_TYPE Debug)
endif()

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    add_compile_options($<$<CONFIG:Debug>:-fstandalone-debug>)
elseif(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    add_compile_options($<$<CONFIG:Debug>:-g>)
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
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

cmake_policy(SET CMP0135 NEW)
FetchContent_Declare(
        googletest
        URL https://github.com/google/googletest/archive/refs/tags/v1.14.0.zip)
FetchContent_MakeAvailable(googletest)

add_subdirectory(src)
add_subdirectory(tests)
add_subdirectory(include)
