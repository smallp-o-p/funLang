add_library(std_modules)
target_sources(std_modules PUBLIC
        FILE_SET std_mods
        TYPE CXX_MODULES
        FILES
        std_modules.cppm
)
