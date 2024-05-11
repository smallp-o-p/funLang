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

  bool insert(const std::shared_ptr<Decl> &decl) {
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
  std::vector<FnCallNode *> non_resolved;
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

  void actOnVarDecl(VarDeclStmt &decl) {
	if (!currentScope->insert(decl.toDecl())) {
	  diags->emitDiagMsg(decl.getTok().getFromPtr(), diag::err_var_redefinition, decl.getName());
	}
	if (decl.getExpr().getResultingType().first!=decl.getDeclType()) {
	  diags->emitDiagMsg(decl.getTok().getFromPtr(),
						 diag::err_incompatible_type_var_decl,
						 decl.getName(),
						 decl.getExpr().getResultingType().second);
	}
  }

  bool actOnCompilationUnit(FunctionsNode &fncs) {
	std::unordered_map<std::string, std::shared_ptr<FunctionNode>> &fns = fncs.getFnMap();
	bool errs = false;
	for (FnCallNode *fn : non_resolved) {
	  if (!currentScope->find(fn->getName().getIdentifier())) {
		diags->emitDiagMsg(fn->getLoc(), diag::err_fn_not_found, fn->getName().getLexeme());
		errs = true;
	  }
	}
	return errs;
  }

  bool actOnNameUsage(Token &identifier) {
	assert(identifier.getTag()==Basic::tok::Tag::identifier && "Trying to lookup a tag that isn't an identifier");
	if (!lookup(identifier.getLexeme())) {
	  diags->emitDiagMsg(identifier.getFromPtr(), diag::err_var_not_found, identifier.getLexeme());
	  return false;
	}
	return true;
  }

  bool actOnFnDecl(FunctionNode &fn) {
	return false;
  }

  bool actOnReturnStmt() {
	return true;
  }

  bool actOnUnaryOp(UnaryOp &unary) {
	auto type_pair = unary.getResultingType();
	switch (type_pair.first) {
	case Basic::Data::i32:
	case Basic::Data::i64:
	case Basic::Data::f32:
	case Basic::Data::f64: return true;
	default:
	  diags->emitDiagMsg(unary.getLoc(),
						 diag::err_incompatible_binary_operands,
						 Basic::Op::getUnaryOpSpelling(unary.getOp()), unary.getResultingType().second);
	  return false;
	}
  }

  bool actOnBinaryOp(BinaryOp &bin) {
	if (bin.getLhs().getResultingType().first==Basic::Data::invalid
		|| bin.getRhs().getResultingType().first==Basic::Data::invalid) {
	  return true; // pretend there's no error
	}
	if (bin.getLhs().getResultingType().first==bin.getRhs().getResultingType().first) {
	  bin.setType(bin.getLhs().getResultingType().first);
	  return true;
	} else {
	  diags->emitDiagMsgRange(bin.getLhs().getLoc(),
							  bin.getRhs().getLoc(),
							  diag::err_incompatible_binary_operands,
							  bin.getLhs().getResultingType().second,
							  bin.getRhs().getResultingType().second);
	  return false;
	};
  }

  bool actOnFnCall(const FnCallNode &fnCall) {
	bool failed = false;
	Decl *fn;
	if (!(fn = lookup(fnCall.getName().getIdentifier()))) {
	  non_resolved.push_back(&fnCall);
	  return false;
	}

	auto fn_casted = llvm::dyn_cast<FunctionNode>(fn);
	if (fn_casted->getProto()->getNumArgs()!=fnCall.getArgs()->getSize()) {
	  diags->emitDiagMsg(fnCall.getName().getFromPtr(),
						 diag::err_wrong_number_of_parameters,
						 fnCall.getName().getLexeme());
	  failed = true;
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