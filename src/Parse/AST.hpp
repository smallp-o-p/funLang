#pragma once
#include "Lex.hpp"
#include "OperationKinds.hpp"
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
class TypeProperties;
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
class NodeVisitor;

class Node {
public:
  virtual ~Node() {}
  virtual void accept(NodeVisitor &visitor) = 0;
};
class ProgramNode : public Node {
private:
  std::unique_ptr<FunctionsNode> funcs;
  std::unique_ptr<std::unordered_map<std::string, int>> globalSymbols;

public:
  auto &getFuncs();
  auto &getGlobs();
  ProgramNode(std::unique_ptr<FunctionsNode> fncs)
      : funcs(std::move(fncs)), globalSymbols(nullptr) {};
  ProgramNode(std::unique_ptr<FunctionsNode> fncs,
              std::unique_ptr<std::unordered_map<std::string, int>> globs)
      : funcs(std::move(fncs)), globalSymbols(std::move(globs)) {}

  virtual void accept(NodeVisitor &visitor) override {}
};

class TypeNode : public Node {
public:
  enum DataTypes {
    VOID = 10000,
    CHAR,
    BOOL,
    i32,
    i64,
    f32,
    f64,
    STRING,
    IDENT,
    INVALID
  };

private:
  DataTypes type;
  std::string user_defined;

public:
  TypeNode(Token tok);
  DataTypes getType() { return type; }
  virtual void accept(NodeVisitor &v) override {}
};

class FunctionsNode : public Node {
private:
  std::vector<std::unique_ptr<FunctionNode>> fns;

public:
  FunctionsNode(std::vector<std::unique_ptr<FunctionNode>> fnList)
      : fns(std::move(fnList)) {}
  virtual void accept(NodeVisitor &v) override {}
};

class FunctionNode : public Node {
private:
  std::unique_ptr<PrototypeNode> proto;
  std::unique_ptr<CompoundStmt> compound;

public:
  FunctionNode(std::unique_ptr<PrototypeNode> pro,
               std::unique_ptr<CompoundStmt> compoundStmt)
      : proto(std::move(pro)), compound(std::move(compoundStmt)) {};
  virtual void accept(NodeVisitor &v) override {}
};

class PrototypeNode : public Node {
private:
  std::unique_ptr<TypeNode> type;
  std::string name;
  std::unique_ptr<ArgumentsNode> args;

public:
  PrototypeNode(std::unique_ptr<TypeNode> fnType, std::string_view n,
                std::unique_ptr<ArgumentsNode> argsList)
      : type(std::move(fnType)), name(n), args(std::move(argsList)) {}
  virtual void accept(NodeVisitor &v) override {}
};

class ArgumentsNode : public Node {
private:
  std::vector<std::unique_ptr<ArgNode>> argList;

public:
  ArgumentsNode(std::vector<std::unique_ptr<ArgNode>> &args)
      : argList(std::move(args)) {}
  virtual void accept(NodeVisitor &v) override {}
};

class ArgNode : public Node {
private:
  std::unique_ptr<TypeNode> type;
  std::string argName;

public:
  ArgNode(std::unique_ptr<TypeNode> type, const std::string_view &id)
      : type(std::move(type)), argName(id) {}
  virtual void accept(NodeVisitor &v) override {}
};

class CompoundStmt : public Node {
private:
  std::vector<std::unique_ptr<Stmt>> stmts;

public:
  CompoundStmt(std::vector<std::unique_ptr<Stmt>> simples)
      : stmts(std::move(simples)) {}
  virtual void accept(NodeVisitor &v) override {}
};

class Stmt : public Node {
public:
  Stmt() {}
};

class VarDecl : public Stmt {
private:
  std::unique_ptr<TypeNode> type;
  std::string name;
  std::unique_ptr<Expr> expr;

public:
  VarDecl(std::unique_ptr<TypeNode> t, std::string_view id,
          std::unique_ptr<Expr> expression)
      : type(std::move(t)), name(id), expr(std::move(expression)) {}
  TypeNode::DataTypes getDeclType();
  std::string &getName();
  Expr &getExpr();
  virtual void accept(NodeVisitor &v) override {}
};

class returnNode : public Stmt {
private:
  std::unique_ptr<Expr> expr;

public:
  returnNode(std::unique_ptr<Expr> exprNode) : expr(std::move(exprNode)) {}
  virtual void accept(NodeVisitor &v) override {}
};

class Expr : public Stmt {
public:
  enum ExprKind { BINARY, UNARY, PRIMARY, FNCALL };

public:
  ExprKind kind;

public:
  Expr(ExprKind k) : kind(k) {};
  Expr() {};
};

class BinaryOp : public Expr {
public:
private:
  Basic::BinaryOperations op;
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;

public:
  BinaryOp(std::unique_ptr<Expr> left, std::unique_ptr<Expr> right,
           Basic::BinaryOperations opcode)
      : lhs(std::move(left)), rhs(std::move(right)), op(opcode),
        Expr(ExprKind::BINARY) {}
  virtual void accept(NodeVisitor &v) override {}
};

class UnaryOp : public Expr {
public:
private:
  Basic::UnaryOperations op;
  std::unique_ptr<Expr> input;

public:
  void addOp(Basic::UnaryOperations opc);
  void addInput();
  UnaryOp(std::unique_ptr<Expr> inp, Basic::UnaryOperations opc)
      : input(std::move(inp)), op(opc), Expr(ExprKind::UNARY) {}
  virtual void accept(NodeVisitor &v) override {}
};

class leafNode : public Expr {
protected:
  Token tok;

public:
  const std::string &getLexeme();
  Basic::tok::Tag getTag();

  leafNode(const Token &token) : tok(token), Expr(ExprKind::PRIMARY) {}

  virtual void accept(NodeVisitor &v) override {}
};

class fnCallNode : public Expr {
private:
  std::string name;
  std::unique_ptr<callArgList> args;

public:
  fnCallNode(const std::string_view id, std::unique_ptr<callArgList> arguments)
      : name(id), args(std::move(arguments)), Expr(ExprKind::FNCALL) {}
  virtual void accept(NodeVisitor &v) override {}
};

class callArgList : public Node {
private:
  std::vector<std::unique_ptr<Expr>> args;

public:
  callArgList(std::vector<std::unique_ptr<Expr>> a) : args(std::move(a)) {}
  virtual void accept(NodeVisitor &v) override {};
};
