add_library(Basic ""
        ${BASIC_HEADER_LIST})

target_sources(Basic PUBLIC
        FILE_SET basic_modules
        TYPE CXX_MODULES
        FILES Basic.ixx IdentifierTable.ixx Diag.ixx Result.ixx)
target_link_libraries(Basic PRIVATE ${LLVM_LIBS})
message("Basic current directory is: ${CMAKE_CURRENT_LIST_DIR}")
