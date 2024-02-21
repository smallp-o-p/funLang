#pragma once
#include "../Lex/Lex.hpp"
#include <map>
#include <memory>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
/*
 * TODO: find a way to untangle this with CMake */
namespace Parse {
enum DataTypes {
  VOID = 10000,
  CHAR,
  BOOL,
  i32,
  i64,
  f32,
  f64,
  STRING,
  USERDEFINED,
  INVALID
};

enum BinaryOperators {
  EQEQ = -9999,
  NE,
  GTE,
  LTE,
  GT,
  LT,
  MULT,
  DIV,
  SUBTRACT,
  ADDASSIGN,
  SUBASSIGN,
  ADD,
  ASSIGN,
  NONE,
  UNDEFINED
};

enum UnaryOperators {
  PREINCREMENT,
  PREDECREMENT,
  POSTINCREMENT,
  POSTDECREMENT,
  BANG,
  NEGATE,
  NOP
};

enum ExprType { PRIMARY, FNCALL, UNARY, BINARY };
enum StmtType { RETURN, VARDECL, EXPR };
} // namespace Parse

class ProgramNode;
class FunctionsNode;
class FunctionNode;
class PrototypeNode;
class ArgumentsNode;
class ArgNode;
class CompoundStmt;
class SimpleListNode;
class Stmt;
class DeclNode;
class Expr;
class ReturnNode;
class TypeNode;
class fnCall;
class callArgList;

class ProgramNode {
private:
  std::unique_ptr<FunctionsNode> funcs;
  std::unique_ptr<std::unordered_map<std::string, int>> globalSymbols;

public:
  auto &getFuncs();
  auto &getGlobs();
  ProgramNode(std::unique_ptr<FunctionsNode> funcs);
  ProgramNode(std::unique_ptr<FunctionsNode> funcs,
              std::unique_ptr<std::unordered_map<std::string, int>> globs);
};

class TypeNode {
private:
  Parse::DataTypes type;
  std::string user_defined;

public:
  TypeNode(Lexer::LexerToken &tok);
};

class FunctionsNode {
private:
  std::vector<std::unique_ptr<FunctionNode>> fns;

public:
  FunctionsNode(std::vector<std::unique_ptr<FunctionNode>> &fnList);
};

class FunctionNode {
private:
  std::unique_ptr<PrototypeNode> proto;
  std::unique_ptr<CompoundStmt> compound;

public:
  FunctionNode(std::unique_ptr<PrototypeNode> pro,
               std::unique_ptr<CompoundStmt> compoundStmt);
};

class PrototypeNode {
private:
  std::unique_ptr<TypeNode> type;
  std::string name;
  std::unique_ptr<ArgumentsNode> args;

public:
  PrototypeNode(std::unique_ptr<TypeNode> fnType, std::string &name,
                std::unique_ptr<ArgumentsNode> argsList);
};

class ArgumentsNode {
private:
  std::vector<std::unique_ptr<ArgNode>> argList;

public:
  ArgumentsNode(std::vector<std::unique_ptr<ArgNode>> &args);
};

class ArgNode {
private:
  std::unique_ptr<TypeNode> type;
  std::string argName;

public:
  ArgNode(std::unique_ptr<TypeNode> type, const std::string &id);
};

class CompoundStmt {
private:
  std::vector<std::unique_ptr<Stmt>> stmts;

public:
  CompoundStmt(std::vector<std::unique_ptr<Stmt>> &simples);
};

class Stmt {
protected:
  Parse::StmtType stmtT;

public:
  Stmt(Parse::StmtType t) { stmtT = t; }
  Stmt() = delete;
  virtual Parse::StmtType getStmtT() { return stmtT; };
};

class VarDecl : public Stmt {
private:
  std::unique_ptr<TypeNode> type;
  std::string name;
  std::unique_ptr<Expr> expr;

public:
  VarDecl(std::unique_ptr<TypeNode> t, std::string &id,
          std::unique_ptr<Expr> expression);
  Parse::DataTypes getDeclType();
  std::string &getName();
  Expr &getExpr();
};

class returnNode : public Stmt {
private:
  std::unique_ptr<Expr> expr;

public:
  returnNode(std::unique_ptr<Expr> exprNode);
};

class Expr : public Stmt {
protected:
  Parse::ExprType exprT;

public:
  Parse::ExprType getExprType();
  Expr() = delete;
  Expr(Parse::ExprType t) : Stmt(Parse::StmtType::EXPR) { exprT = t; }
};

class BinaryOp : public Expr {
private:
  Parse::BinaryOperators op;
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;

public:
  BinaryOp(std::unique_ptr<Expr> left, std::unique_ptr<Expr> right,
           Parse::BinaryOperators opcode);
};

class UnaryOp : public Expr {
private:
  Parse::UnaryOperators op;
  std::unique_ptr<Expr> input;

public:
  UnaryOp();
  void addOp(Parse::UnaryOperators opc);
  void addInput();
  UnaryOp(std::unique_ptr<Expr> inp, Parse::UnaryOperators opc);
};

class leafNode : public Expr {
protected:
  Lexer::LexerToken tok;

public:
  std::string_view getLexeme();
  Lexer::Tag getTag();

  leafNode(const Lexer::LexerToken &tok);
};

class fnCallNode : public Expr {
private:
  std::string name;
  std::unique_ptr<callArgList> args;

public:
  fnCallNode(const std::string &id, std::unique_ptr<callArgList> arguments);
};

class callArgList {
private:
  std::vector<std::unique_ptr<Expr>> args;

public:
  callArgList(std::vector<std::unique_ptr<Expr>> &a);
};
