#include "Codegen.hpp"
extern std::string fileName;
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
  BasicBlock *bb =
      BasicBlock::Create(*ctx, func.getProto()->getId().append("-entry"), f);
  builder->SetInsertPoint(bb);
  std::map<std::string, Value *> vals;
  for (auto &arg : f->args()) {
    vals[std::string(arg.getName())] = &arg;
  }
  Value *retVal = codegenCompoundStmt(func.getCompound(), vals);
  if (!retVal) {
    return nullptr;
  }
  builder->CreateRet(retVal);
  verifyFunction(*f);
  return f;
}

std::vector<Type *> processArgs(std::vector<std::unique_ptr<argNode>> &args) {
  if (args.size() == 0) {
    return {};
  }
  std::vector<Type *> argTypes = {};
  for (std::unique_ptr<argNode> &arg : args) {
    switch (arg->getTypeVal()) {
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
  switch (proto.getTypeVal()) {
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
