#pragma once
#include "AST.hpp"
#include "llvmStructs.hpp"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

class ASTNodeVisitor {
public:
  llvm::Value *codegenProgram(const std::unique_ptr<programNode> &program);
  llvm::Value *
  codegenFunctions(const std::unique_ptr<functionsNode> &functions);
  llvm::Value *codegenFunc(funcNode &func);
  llvm::Function *codegenProto(protoNode &proto);
  llvm::Value *
  codegenCompoundStmt(const std::unique_ptr<compoundStmtNode> &compound,
                      std::map<std::string, llvm::Value *> &availableVars);
  llvm::Value *
  codegenSimpleList(const std::unique_ptr<simpleListNode> &simpleList);
  llvm::Value *
  codegenSimpleStmt(const std::unique_ptr<simpleStmtNode> &simpleStmt);
  llvm::Value *codegenDeclare(const std::unique_ptr<declareNode> &decl);
  llvm::Value *codegenExpr(const std::unique_ptr<exprNode> &expr);
  llvm::Value *codegenRet(const std::unique_ptr<returnNode> &ret);
  llvm::Value *codegenAssign(const std::unique_ptr<assignExprNode> &assign);
  llvm::Value *codegenEq(const std::unique_ptr<eqExprNode> &eq);
  llvm::Value *codegenCmp(const std::unique_ptr<cmpExprNode> &cmp);
  llvm::Value *codegenAdd(const std::unique_ptr<addExprNode> &add);
  llvm::Value *codegenMultDiv(const std::unique_ptr<multdivNode> &multdiv);
  llvm::Value *codegenUnary(const std::unique_ptr<unaryNode> &unary);
  llvm::Value *codegenPrimary(const std::unique_ptr<primaryNode> &primary);
  llvm::Value *codegenFnCall(const std::unique_ptr<fnCallNode> &fncall);
  llvm::Value *logCodegenError(const char *str, ...);
};
std::vector<llvm::Type *>
processArgs(std::vector<std::unique_ptr<argNode>> &args);
