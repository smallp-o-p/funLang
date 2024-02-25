#include "AST.hpp"
#include "Lex.hpp"
#include <string_view>
#include <unordered_map>
auto &ProgramNode::getFuncs() { return funcs; }
auto &ProgramNode::getGlobs() { return globalSymbols; }

TypeNode::TypeNode(Lexer::Token &tok) {
  using namespace Lexer;
  switch (tok.syntactic_category) {
  case Lexer::Tag::BOOL:
    type = DataTypes::BOOL;
    break;
  case Lexer::Tag::I32:
    type = DataTypes::i32;
    break;
  case Lexer::Tag::I64:
    type = DataTypes::i64;
    break;
  case Lexer::Tag::STRING:
    type = DataTypes::STRING;
  case Lexer::Tag::F32:
    type = DataTypes::f32;
    break;
  case Lexer::Tag::F64:
    type = DataTypes::f64;
    break;
  case Lexer::Tag::IDENTIFIER:
    type = DataTypes::USERDEFINED;
    user_defined = tok.lexeme;
    break;
  default:
    type = DataTypes::VOID;
    break;
  }
}

std::string &VarDecl::getName() { return name; }
Expr &VarDecl::getExpr() { return *expr; }

std::string_view leafNode::getLexeme() { return tok.lexeme; }
