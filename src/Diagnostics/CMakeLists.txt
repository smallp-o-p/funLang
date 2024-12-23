add_library(Diags)

target_sources(Diags PUBLIC
        FILE_SET diag_modules
        TYPE CXX_MODULES
        FILES Diag.cppm
)

target_include_directories(Diags ../../include/Basic/defs)
target_link_libraries(Diags PRIVATE Basic Lex)