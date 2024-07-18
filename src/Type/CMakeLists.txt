set(TYPE_HEADER_LIST "${funLang_SOURCE_DIR}/include/Type/Type.hpp")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_library(Type "" Type.cpp ${TYPE_HEADER_LIST})

target_include_directories(Type PUBLIC ${LLVM_INCLUDE_DIRS} ../../include)
target_link_libraries(Type PUBLIC Basic ${llvm_libs})