#pragma once
#include "AST.hpp"
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include "llvm/ADT/StringMap.h"
namespace funLang {
class SemaAnalyzer;
class Scope;

class Scope {
  llvm::StringMap<std::shared_ptr<Decl>> symTable;
  std::shared_ptr<Scope> parentScope;

public:
  Scope() : parentScope(nullptr) {}
  explicit Scope(std::shared_ptr<Scope> parent)
	  : parentScope(std::move(parent)), symTable(llvm::StringMap<std::shared_ptr<Decl>>()) {}

  bool insert(std::shared_ptr<Decl> decl) {
	switch (decl->getKind()) {
	case Decl::DK_VAR: {
	  if (VarDecl *var = llvm::dyn_cast<VarDecl>(decl.get())) {

		return symTable.insert(std::pair<llvm::StringRef, std::shared_ptr<Decl>>(var->getName().getIdentifier(),
																				 decl)).second;
	  }
	  return false;
	}
	case Decl::DK_FN: {
	  if (auto *fn = llvm::dyn_cast<FunctionNode>(decl.get())) {
		return symTable.insert(std::pair<llvm::StringRef, std::shared_ptr<Decl>>(fn->getName().getLexeme(),
																				 decl)).second;
	  }
	  return false;
	}
	case Decl::DK_STRUCT: {
	  assert(true && "structs not implemented \n");
	}
	}
	return false;
  }

  std::shared_ptr<Scope> &getParent() {
	return parentScope;
  }

  std::shared_ptr<Decl> find(llvm::StringRef varName) {
	auto found = symTable.find(varName);
	if (found==symTable.end()) {
	  return nullptr;
	}
	return found->second;
  }
};

class SemaAnalyzer {
private:
  std::shared_ptr<Scope> currentScope;
  std::shared_ptr<DiagEngine> diags;
public:
  explicit SemaAnalyzer(std::shared_ptr<DiagEngine> diag)
	  : currentScope(std::make_shared<Scope>()), diags(std::move(diag)) {}

  void enterScope() {
	std::shared_ptr<Scope> innerScope = std::make_shared<Scope>(currentScope);
	currentScope = innerScope;
  }

  void exitScope() {
	assert(currentScope->getParent() && "Attempted to exit global scope.\n");
	currentScope = currentScope->getParent();
  }

  bool actOnVarDeclStmt(VarDeclStmt &decl) {
	if (!currentScope->insert(decl.toDecl())) {
	  diags->emitDiagMsg(decl.getTok().getFromPtr(), diag::err_var_redefinition, decl.getName());
	  return false;
	}
	if (decl.getExpr().getResultingType()!=decl.getDeclType()) {
	  diags->emitDiagMsg(decl.getTok().getFromPtr(), diag::err_incompatible_type_var_decl, decl.getName());
	}
	return true;
  }

  bool actOnFunctions(FunctionsNode &fncs) {
	std::unordered_map<std::string, std::shared_ptr<FunctionNode>> &fns = fncs.getFnMap();
	return std::all_of(fns.cbegin(),
					   fns.cend(),
					   [this](auto fn) { return currentScope->insert(std::move(fn.second)); });
  }

  bool actOnFnDecl(FunctionNode &fn) {
	return false;
  }

  // type check an expr: check if all operands are the same type as the left most operand
  bool actOnExpr(Expr &expr) {
	return false;
  }

  bool actOnFnCall(fnCallNode &fnCall) {
	bool failed = false;
	Decl *fn;
	if (!(fn = lookup(fnCall.getName().getIdentifier()))) {
	  return false;
	}
	auto fn_casted = llvm::dyn_cast<FunctionNode>(fn);
	if (fn_casted->getProto()->getNumArgs()!=fnCall.getArgs()->getSize()) {
	  diags->emitDiagMsg(fnCall.getName().getFromPtr(),
						 diag::err_wrong_number_of_parameters,
						 fnCall.getName().getLexeme());
	  failed = true;
	}

	for (std::unique_ptr<Expr> &arg : fnCall.getArgs()->getArgsVec()) {
	}

	return failed;
  }

  Decl *lookup(llvm::StringRef var) {
	std::shared_ptr<Scope> s = currentScope;
	while (s) {
	  Decl *found = s->find(var).get();

	  if (found) {
		return found;
	  } else {
		s = s->getParent();
	  }
	}
	return nullptr;
  }
};
}