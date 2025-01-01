add_library(Diag)

target_sources(Diag PUBLIC
        FILE_SET diag_modules
        TYPE CXX_MODULES
        FILES Diag.cppm
)

target_include_directories(Diag PRIVATE ../../include/Basic/defs)
target_link_libraries(Diag PRIVATE Basic)