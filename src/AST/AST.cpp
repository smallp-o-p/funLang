#include "AST.hpp"
#include "ASTVisitor.hpp"
#include "Lex.hpp"
#include "Parse.hpp"
#include "llvmStructs.hpp"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include <cstdarg>
#include <cstdint>
#include <memory>
#include <string>
using namespace llvm;

typeNode::~typeNode() {}
funcNode::~funcNode() {}
functionsNode::~functionsNode() {}
programNode::~programNode() {}

llvm::Value *ASTNodeVisitor::logCodegenError(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  std::vprintf(fmt, args);
  va_end(args);
  return nullptr;
}

Value *
ASTNodeVisitor::codegenProgram(const std::unique_ptr<programNode> &program) {}
Value *ASTNodeVisitor::codegenFunctions(
    const std::unique_ptr<functionsNode> &functions) {
  for (std::unique_ptr<funcNode> &func : functions->getFuncs()) {
  }
}

Value *ASTNodeVisitor::codegenFunc(funcNode &func) {
  Function *f = translationUnit->getFunction(func.getProto()->getId());
  if (!f) {
    f = codegenProto(*func.getProto());
  }
  if (!f) {
    return nullptr;
  }
  BasicBlock *bb = BasicBlock::Create(*ctx, "entry", f);
}

std::vector<Type *> processArgs(std::vector<std::unique_ptr<argNode>> &args) {
  if (args.size() == 0) {
    return {};
  }
  std::vector<Type *> argTypes = {};
  for (std::unique_ptr<argNode> &arg : args) {
    switch (arg->typeNode().getType()) {
    case VOID:
      argTypes.push_back(Type::getVoidTy(*ctx));
      break;
    case CHAR:
      argTypes.push_back(Type::getInt8Ty(*ctx));
      break;
    case BOOL:
      argTypes.push_back(Type::getInt1Ty(*ctx));
      break;
    case i32:
      argTypes.push_back(Type::getInt32Ty(*ctx));
      break;
    case i64:
      argTypes.push_back(Type::getInt64Ty(*ctx));
      break;
    case f32:
      argTypes.push_back(Type::getFloatTy(*ctx));
      break;
    case f64:
      argTypes.push_back(Type::getDoubleTy(*ctx));
      break;
    case STRING:
      argTypes.push_back(PointerType::get(strType, 0));
      break;
    case INVALID:
      break;
    }
  }
  return argTypes;
}
Function *ASTNodeVisitor::codegenProto(protoNode &proto) {
  std::vector<Type *> processed_args =
      processArgs(proto.getArgsNode()->getArgs());
  uint8_t argsSize = processed_args.size();
  Type *retType;
  switch (proto.getTypePtr()->getType()) {
  case VOID: {
    retType = Type::getVoidTy(*ctx);
    break;
  }
  case BOOL:
    retType = Type::getInt1Ty(*ctx);
    break;
  case CHAR:
    retType = Type::getInt8Ty(*ctx);
    break;
  case i32:
    retType = Type::getInt32Ty(*ctx);
    break;
  case i64:
    retType = Type::getInt64Ty(*ctx);
    break;
  case f32:
    retType = Type::getFloatTy(*ctx);
    break;
  case f64:
    retType = Type::getDoubleTy(*ctx);
    break;
  case STRING:
    retType = PointerType::get(strType, 0);
    break;
  case INVALID: {
    logCodegenError("Problem w/ type in prototype declaration of func '%s'",
                    proto.getId().c_str());
    return nullptr;
  }
  }
  FunctionType *fType = FunctionType::get(retType, processed_args, false);
  Function *f = Function::Create(fType, Function::InternalLinkage,
                                 proto.getId(), translationUnit.get());

  for (auto &&[arg, name] : zip(f->args(), proto.getArgsNode()->getArgs())) {
    arg.setName(name->getId());
  }
  return f;
}

Value *
ASTNodeVisitor::codegenPrimary(const std::unique_ptr<primaryNode> &primary) {}

int main() {
  ctx = std::make_unique<llvm::LLVMContext>();
  strType = llvm::StructType::create(
      *ctx, {Type::getInt32Ty(*ctx), PointerType::getInt8PtrTy(*ctx)},
      "String");
  builder = std::make_unique<llvm::IRBuilder<>>(*ctx);
  translationUnit = std::make_unique<llvm::Module>(Lexer::fileName, *ctx);
}
