add_library(Basic)
target_sources(Basic PUBLIC
        FILE_SET basic_modules
        TYPE CXX_MODULES
        FILES
        Basic.cppm
        Constants.cppm
        IdentifierTable.cppm
        Result.cppm
        Concepts.cppm
        Casting.cppm
)
add_subdirectory(llvm)
add_subdirectory(std)
target_link_libraries(Basic PRIVATE llvm_modules std_modules)
message("Basic current directory is: ${CMAKE_CURRENT_LIST_DIR}")
