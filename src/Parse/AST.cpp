#include "AST.hpp"
#include "Lex.hpp"
#include "Basic.hpp"
#include "Sema.hpp"
std::unordered_map<std::string,
				   std::shared_ptr<FunctionNode>> &CompilationUnit::getFuncs() { return funcs->getFnMap(); }
auto &CompilationUnit::getGlobs() { return globalSymbols; }
std::unordered_map<std::string, std::shared_ptr<FunctionNode>> &FunctionsNode::getFnMap() {
  return fnMap;
}
TypeNode::TypeNode(Token &tok) : identifier(tok) {
  using namespace Basic;
  switch (tok.getTag()) {
  case tok::Tag::kw_bool:type = Data::Type::bool_;
	break;
  case tok::Tag::kw_i32:type = Data::Type::i32;
	break;
  case tok::Tag::kw_i64:type = Data::Type::i64;
	break;
  case tok::Tag::kw_string:type = Data::Type::string;
  case tok::Tag::kw_f32:type = Data::Type::f32;
	break;
  case tok::Tag::kw_f64:type = Data::Type::f64;
	break;
  case tok::Tag::identifier:type = Data::Type::ident;
	identifier = tok;
	break;
  case tok::Tag::kw_void:type = Data::Type::void_;
	break;
  default:type = Data::Type::invalid;
	break;
  }
}

llvm::StringRef VarDeclStmt::getName() { return name.getIdentifier(); }

Expr &VarDeclStmt::getExpr() { return *expr; }
Token &VarDeclStmt::getTok() {
  return name;
}

llvm::StringRef leafNode::getLexeme() { return tok.getLexeme(); }

Basic::tok::Tag leafNode::getTag() {
  return Basic::tok::exclaimequal;
}

std::vector<std::unique_ptr<Stmt>> &CompoundStmt::getStmts() {
  return stmts;
}
size_t PrototypeNode::getNumArgs() {
  return args->getArgList().size();
}

void Expr::accept(funLang::SemaAnalyzer &v) {
  v.actOnExpr(*this);
}

void Expr::setType(Basic::Data::Type toSet) {

}
Basic::Data::Type Expr::getResultingType() {
  assert(resultingType!=Basic::Data::Type::invalid && "Type not evaluated for expr\n");
  return resultingType;
}
