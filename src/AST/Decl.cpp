//
// Created by will on 7/24/24.
//

#include "AST/Decl.hpp"

funLang::Decl *funLang::DeclContext::lookup(llvm::StringMapEntry<std::nullopt_t> *Name) {
  auto Found = Decls.find(Name);
  if (Found == Decls.end()) {
	return nullptr;
  }
  return Found->second.get();
}

funLang::Type *funLang::FunctionDecl::getTypePtr() { return ReturnType->getTypePtr(); }
