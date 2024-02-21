#include "ParseTree.hpp"
#include "Lex.hpp"
#include <string_view>
#include <unordered_map>
auto &ProgramNode::getFuncs() { return funcs; }
auto &ProgramNode::getGlobs() { return globalSymbols; }
ProgramNode::ProgramNode(std::unique_ptr<FunctionsNode> fncs) {
  funcs = std::move(fncs);
  globalSymbols = nullptr;
}
ProgramNode::ProgramNode(
    std::unique_ptr<FunctionsNode> fncs,
    std::unique_ptr<std::unordered_map<std::string, int>> globs) {
  funcs = std::move(fncs);
  globalSymbols = std::move(globs);
}

TypeNode::TypeNode(Lexer::LexerToken &tok) {
  using namespace Lexer;
  switch (tok.syntactic_category) {
  case BOOL:
    type = Parse::DataTypes::BOOL;
    break;
  case I32:
    type = Parse::DataTypes::i32;
    break;
  case I64:
    type = Parse::DataTypes::i64;
    break;
  case STRING:
    type = Parse::DataTypes::STRING;
  case F32:
    type = Parse::DataTypes::f32;
    break;
  case F64:
    type = Parse::DataTypes::f64;
    break;
  case IDENTIFIER:
    type = Parse::DataTypes::USERDEFINED;
    user_defined = tok.lexeme;
    break;
  default:
    type = Parse::DataTypes::VOID;
    break;
  }
}

FunctionsNode::FunctionsNode(
    std::vector<std::unique_ptr<FunctionNode>> &fnList) {
  fns = fnList;
}

PrototypeNode::PrototypeNode(std::unique_ptr<TypeNode> fnType,
                             std::string &name,
                             std::unique_ptr<ArgumentsNode> argsList) {
  type = std::move(fnType);
  name = name;
  args = std::move(argsList);
}

ArgumentsNode::ArgumentsNode(std::vector<std::unique_ptr<ArgNode>> &args) {
  argList = std::move(args);
}

ArgNode::ArgNode(std::unique_ptr<TypeNode> t, const std::string &id) {
  argName = id;
  type = std::move(t);
}

CompoundStmt::CompoundStmt(std::vector<std::unique_ptr<Stmt>> &simples) {
  stmts = simples;
}

VarDecl::VarDecl(std::unique_ptr<TypeNode> t, std::string &id,
                 std::unique_ptr<Expr> expression)
    : Stmt(Parse::StmtType::VARDECL) {
  stmtT = Parse::StmtType::VARDECL;
  type = std::move(t);
  name = id;
  expr = std::move(expression);
}

std::string &VarDecl::getName() { return name; }
Expr &VarDecl::getExpr() { return *expr; }

returnNode::returnNode(std::unique_ptr<Expr> exprNode)
    : Stmt(Parse::StmtType::RETURN) {
  expr = std::move(exprNode);
}

BinaryOp::BinaryOp(std::unique_ptr<Expr> left, std::unique_ptr<Expr> right,
                   Parse::BinaryOperators opcode)
    : Expr(Parse::ExprType::BINARY) {
  op = opcode;
  lhs = std::move(left);
  rhs = std::move(right);
}

UnaryOp::UnaryOp(std::unique_ptr<Expr> inp, Parse::UnaryOperators opc)
    : Expr(Parse::ExprType::UNARY) {
  op = opc;
  input = std::move(inp);
}

leafNode::leafNode(const Lexer::LexerToken &tok)
    : Expr(Parse::ExprType::PRIMARY) {
  this->tok = tok;
}

std::string_view leafNode::getLexeme() { return tok.lexeme; }

fnCallNode::fnCallNode(const std::string &id,
                       std::unique_ptr<callArgList> arguments)
    : Expr(Parse::ExprType::FNCALL) {
  name = id;
  args = std::move(arguments);
}

callArgList::callArgList(std::vector<std::unique_ptr<Expr>> &a) { args = a; }
