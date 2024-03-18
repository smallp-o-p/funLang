#include "AST.hpp"
#include "Lex.hpp"
#include "TokenTags.hpp"
auto &ProgramNode::getFuncs() { return funcs; }
auto &ProgramNode::getGlobs() { return globalSymbols; }

TypeNode::TypeNode(Token tok) {
  using namespace Basic;
  switch (tok.getTag()) {
  case tok::Tag::kw_bool:
    type = DataTypes::BOOL;
    break;
  case tok::Tag::kw_i32:
    type = DataTypes::i32;
    break;
  case tok::Tag::kw_i64:
    type = DataTypes::i64;
    break;
  case tok::Tag::kw_string:
    type = DataTypes::STRING;
  case tok::Tag::kw_f32:
    type = DataTypes::f32;
    break;
  case tok::Tag::kw_f64:
    type = DataTypes::f64;
    break;
  case tok::Tag::identifier:
    type = DataTypes::IDENT;
    user_defined = tok.getIdentifier();
    break;
  case tok::Tag::kw_void:
    type = DataTypes::VOID;
    break;
  default:
    type = DataTypes::INVALID;
    break;
  }
}

std::string &VarDecl::getName() { return name; }
Expr &VarDecl::getExpr() { return *expr; }

const std::string &leafNode::getLexeme() { return tok.getIdentifier(); }
