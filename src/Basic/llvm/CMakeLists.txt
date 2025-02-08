add_library(llvm_modules)
target_sources(llvm_modules PUBLIC
        FILE_SET llvm_mods
        TYPE CXX_MODULES
        FILES
        llvm.cppm
)
target_link_libraries(llvm_modules PRIVATE ${LLVM_LIBS})