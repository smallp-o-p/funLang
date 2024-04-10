#include "AST.hpp"
#include "Lex.hpp"
#include "TokenTags.hpp"
std::unordered_map<std::string, std::unique_ptr<FunctionNode>> & ProgramNode::getFuncs() { return funcs->getFnMap(); }
auto &ProgramNode::getGlobs() { return globalSymbols; }
std::unordered_map<std::string, std::unique_ptr<FunctionNode>> &FunctionsNode::getFnMap() {
  return fnMap;
}
TypeNode::TypeNode(Token &tok) : identifier(tok) {
  using namespace Basic;
  switch (tok.getTag()) {
  case tok::Tag::kw_bool:type = DataTypes::BOOL;
	break;
  case tok::Tag::kw_i32:type = DataTypes::i32;
	break;
  case tok::Tag::kw_i64:type = DataTypes::i64;
	break;
  case tok::Tag::kw_string:type = DataTypes::STRING;
  case tok::Tag::kw_f32:type = DataTypes::f32;
	break;
  case tok::Tag::kw_f64:type = DataTypes::f64;
	break;
  case tok::Tag::identifier:type = DataTypes::IDENT;
	identifier = tok;
	break;
  case tok::Tag::kw_void:type = DataTypes::VOID;
	break;
  default:type = DataTypes::INVALID;
	break;
  }
}

llvm::StringRef VarDecl::getName() { return name.getIdentifier(); }
Expr &VarDecl::getExpr() { return *expr; }

llvm::StringRef leafNode::getLexeme() { return tok.getLexeme(); }
Basic::tok::Tag leafNode::getTag() {
  return Basic::tok::exclaimequal;
}
