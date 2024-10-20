cmake_minimum_required(VERSION 3.28 FATAL_ERROR)

project(funLang
        VERSION 0.0.1
        DESCRIPTION " A somewhat fun programming language"
        LANGUAGES C CXX
)
include(FetchContent)

find_package(LLVM QUIET CONFIG)
if (NOT LLVM_FOUND) # build llvm automatically
    set(LLVM_URL https://github.com/llvm/llvm-project/releases/download/llvmorg-19.1.0/llvm-project-19.1.0.src.tar.xz)
    message(STATUS "LLVM not found on system. Checking if already built.")
    message("LLVM Release URL: ${LLVM_URL}")
    FetchContent_Declare(LLVM
            URL ${LLVM_URL}
            FIND_PACKAGE_ARGS NAMES LLVM)
    FetchContent_MakeAvailable(LLVM)
    set(llvm_install_dir ${llvm_BINARY_DIR}/Release)
    if (NOT EXISTS ${llvm_install_dir})
        message(STATUS "LLVM not built, building release from ${LLVM_URL}.")
        execute_process(COMMAND cmake " -S${llvm_SOURCE_DIR}/llvm" "-DCMAKE_BUILD_TYPE=Release" "-B${llvm_BINARY_DIR}" "-DCMAKE_INSTALL_PREFIX=${llvm_install_dir}" "-G ${CMAKE_GENERATOR}")
        execute_process(COMMAND cmake "--build" "${llvm_BINARY_DIR}" "--target install")
    endif ()
    include(${llvm_BINARY_DIR}/lib/cmake/llvm/LLVMConfig.cmake)
    execute_process(COMMAND "./llvm-config" "--ldflags" WORKING_DIRECTORY "${llvm_install_dir}/bin" OUTPUT_VARIABLE LLVM_LD_FLAGS OUTPUT_STRIP_TRAILING_WHITESPACE)
    execute_process(COMMAND "./llvm-config" "--cppflags" WORKING_DIRECTORY "${llvm_install_dir}/bin" OUTPUT_VARIABLE LLVM_CXX_FLAGS OUTPUT_STRIP_TRAILING_WHITESPACE)
else ()
    message(STATUS "LLVM found on system.")
endif ()

FetchContent_Declare(
        googletest
        URL https://github.com/google/googletest/archive/refs/tags/v1.14.0.zip)
FetchContent_MakeAvailable(googletest)
find_package(LLVM REQUIRED CONFIG)

set(CMAKE_EXPORT_COMPILE_COMMANDS)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_SCAN_FOR_MODULES ON)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)
message(STATUS "Using ${CMAKE_CXX_COMPILER_ID} ${CMAKE_CXX_COMPILER_VERSION}")

if ("${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")
    add_compile_options(-fstandalone-debug)
elseif ("${CMAKE_CXX_COMPILER_ID}" MATCHES "GNU")
    message("GCC compilation known to fail. Aborting." FATAL_ERROR)
endif ()

add_compile_options(${LLVM_CXX_FLAGS})
add_link_options(${LLVM_LD_FLAGS})
add_definitions(-fno-rtti)

enable_testing()
include(CTest)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
cmake_policy(SET CMP0135 NEW)
message("LLVM Package Version: ${LLVM_PACKAGE_VERSION}")
message("LLVM Include directories: ${LLVM_INCLUDE_DIRS}")
message("LLVM CXX Flags: ${LLVM_CXX_FLAGS}")
message("LLVM LD Flags: ${LLVM_LD_FLAGS}")
include_directories(${LLVM_INCLUDE_DIRS})

add_subdirectory(src)
add_subdirectory(tests)
add_subdirectory(include)
