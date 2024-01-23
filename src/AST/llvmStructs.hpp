#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
using namespace llvm;
extern std::unique_ptr<llvm::LLVMContext> ctx;
extern std::unique_ptr<llvm::IRBuilder<>> builder;
extern std::unique_ptr<llvm::Module> translationUnit;

extern StructType *strType;
