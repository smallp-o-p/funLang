add_library(Basic)
target_sources(Basic PUBLIC
        FILE_SET basic_modules
        TYPE CXX_MODULES
        FILES Basic.cppm IdentifierTable.cppm Diag.cppm Result.cppm)
target_link_libraries(Basic PRIVATE ${LLVM_LIBS})
message("Basic current directory is: ${CMAKE_CURRENT_LIST_DIR}")
