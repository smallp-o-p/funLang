#pragma once
#include "Lex.hpp"
#include "OperationKinds.hpp"
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>

class TypeProperties;
class CompilationUnit;
class FunctionsNode;
class FunctionNode;
class PrototypeNode;
class ArgumentsNode;
class ArgNode;
class CompoundStmt;
class Stmt;
class Decl;
class VarDeclStmt;
class Expr;
class returnNode;
class TypeNode;
class fnCallNode;
class callArgList;
class NodeVisitor;

class Node {
public:
  virtual ~Node() = default;
  virtual void accept(NodeVisitor &visitor) = 0;
};

class CompilationUnit : public Node {
private:
  std::unique_ptr<FunctionsNode> funcs;
  std::unique_ptr<std::unordered_map<std::string, int>> globalSymbols;
public:
  std::unordered_map<std::string, std::shared_ptr<FunctionNode>> &getFuncs();
  auto &getGlobs();
  explicit CompilationUnit(std::unique_ptr<FunctionsNode> fncs)
	  : funcs(std::move(fncs)), globalSymbols(nullptr) {};
  CompilationUnit(std::unique_ptr<FunctionsNode> fncs,
				  std::unique_ptr<std::unordered_map<std::string, int>> globs)
	  : funcs(std::move(fncs)), globalSymbols(std::move(globs)) {}

  void accept(NodeVisitor &visitor) override {}
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
  Token &identifier;

public:
  explicit TypeNode(Token &tok);
  DataTypes getType() { return type; }
  void accept(NodeVisitor &v) override {}
};

class Decl : public Node {
public:
  enum DeclKind {
	DK_FN,
	DK_VAR,
	DK_STRUCT
  };
private:
  DeclKind kind;

public:
  explicit Decl(DeclKind kind) : kind(kind) {}
  DeclKind getKind() const { return kind; }
  void accept(NodeVisitor &v) override {};
};

class FunctionsNode : public Node {
private:
  std::unordered_map<std::string, std::shared_ptr<FunctionNode>> fnMap;

public:
  explicit FunctionsNode(std::unordered_map<std::string, std::shared_ptr<FunctionNode>> fnMap)
	  : fnMap(std::move(fnMap)) {}
  void accept(NodeVisitor &v) override {}

  std::unordered_map<std::string, std::shared_ptr<FunctionNode>> &getFnMap();

  static bool classof(const Decl &decl) {
	return decl.getKind();
  }
};

class PrototypeNode : public Node {
private:
  std::unique_ptr<TypeNode> type;
  Token &name;
  std::unique_ptr<ArgumentsNode> args;

public:
  PrototypeNode(std::unique_ptr<TypeNode> fnType, Token &name,
				std::unique_ptr<ArgumentsNode> argsList)
	  : type(std::move(fnType)), name(name), args(std::move(argsList)) {}
  void accept(NodeVisitor &v) override {}

  const std::unique_ptr<TypeNode> &getTypeNode() const {
	return type;
  }
  llvm::StringRef getName() const {
	return name.getLexeme();
  }
  const std::unique_ptr<ArgumentsNode> &getArgs() const {
	return args;
  }

  size_t getNumArgs();

};

class FunctionNode : public Decl {
private:
  std::unique_ptr<PrototypeNode> proto;
  std::unique_ptr<CompoundStmt> compound;

public:
  FunctionNode(std::unique_ptr<PrototypeNode> pro,
			   std::unique_ptr<CompoundStmt> compoundStmt)
	  : proto(std::move(pro)), compound(std::move(compoundStmt)), Decl(DK_FN) {};
  void accept(NodeVisitor &v) override {}
  const std::unique_ptr<PrototypeNode> &getProto() const {
	return proto;
  }

  llvm::StringRef getName() {
	return proto->getName();
  }
  const std::unique_ptr<CompoundStmt> &getCompound() const {
	return compound;
  }

  static bool classof(const Decl *D) {
	return D->getKind()==DK_FN;
  }
};

class ArgumentsNode : public Node {
private:
  std::vector<std::unique_ptr<ArgNode>> argList;

public:
  ArgumentsNode(std::vector<std::unique_ptr<ArgNode>> &args)
	  : argList(std::move(args)) {}
  void accept(NodeVisitor &v) override {}
  const std::vector<std::unique_ptr<ArgNode>> &getArgList() {
	return argList;
  }
};

class ArgNode : public Node {
private:
  std::unique_ptr<TypeNode> type;
  Token &argName;

public:
  ArgNode(std::unique_ptr<TypeNode> type, Token &id)
	  : type(std::move(type)), argName(id) {}
  void accept(NodeVisitor &v) override {}
};

class CompoundStmt : public Node {
private:
  std::vector<std::unique_ptr<Stmt>> stmts;

public:
  CompoundStmt(std::vector<std::unique_ptr<Stmt>> simples)
	  : stmts(std::move(simples)) {}
  void accept(NodeVisitor &v) override {}
  std::vector<std::unique_ptr<Stmt>> &getStmts();
};

class Stmt : public Node {
public:
  enum StmtKind {
	SK_VARDECL,
	SK_EXPR,
	SK_RETURN
  };

private:
  StmtKind kind;
public:
  explicit Stmt(StmtKind k) : kind(k) {}
};

class VarDecl : public Decl {
  TypeNode &type;
  Token &name;
  Expr &expr;

public :
  VarDecl(TypeNode &t, Token &n, Expr &expr) : type(t), name(n), expr(expr), Decl(DK_VAR) {}
  TypeNode &getType() const {
	return type;
  }
  Token &getName() const {
	return name;
  }
  Expr &getExpr() const {
	return expr;
  }

  static bool classof(Decl *D) {
	return D->getKind()==DK_VAR;
  }
};

class VarDeclStmt : public Stmt {
private:
  std::unique_ptr<TypeNode> type;
  Token &name;
  std::unique_ptr<Expr> expr;

public:
  VarDeclStmt(std::unique_ptr<TypeNode> t, Token &id,
			  std::unique_ptr<Expr> expression)
	  : type(std::move(t)), name(id), expr(std::move(expression)), Stmt(SK_VARDECL) {}
  TypeNode::DataTypes getDeclType();
  llvm::StringRef getName();
  Token &getTok();
  Expr &getExpr();
  void accept(NodeVisitor &v) override {}
  std::shared_ptr<VarDecl> toDecl() { return std::make_shared<VarDecl>(*type, name, *expr); };
};

class returnNode : public Stmt {
private:
  std::unique_ptr<Expr> expr;

public:
  returnNode(std::unique_ptr<Expr> exprNode) : expr(std::move(exprNode)), Stmt(SK_RETURN) {}
  void accept(NodeVisitor &v) override {}
};

class Expr : public Stmt {
public:
  enum ExprKind { EXPR_BINARY, EXPR_UNARY, EXPR_PRIMARY, EXPR_FNCALL };

public:
  ExprKind kind;

public:
  explicit Expr(ExprKind k) : kind(k), Stmt(SK_EXPR) {};
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
		Expr(ExprKind::EXPR_BINARY) {}
  void accept(NodeVisitor &v) override {}
};

class UnaryOp : public Expr {
public:
private:
  Basic::UnaryOperations op;
  std::unique_ptr<Expr> input;

public:
  UnaryOp(std::unique_ptr<Expr> inp, Basic::UnaryOperations opc)
	  : input(std::move(inp)), op(opc), Expr(ExprKind::EXPR_UNARY) {}
  void accept(NodeVisitor &v) override {}
};

class leafNode : public Expr {
protected:
  Token &tok;

public:
  llvm::StringRef getLexeme();
  Basic::tok::Tag getTag();

  leafNode(Token &token) : tok(token), Expr(ExprKind::EXPR_PRIMARY) {}

  void accept(NodeVisitor &v) override {}
};

class fnCallNode : public Expr {
private:
  Token &name;
public:
  Token &getName() const {
	return name;
  }
  const std::unique_ptr<callArgList> &getArgs() const {
	return args;
  }
private:
  std::unique_ptr<callArgList> args;

public:
  fnCallNode(Token &id, std::unique_ptr<callArgList> arguments)
	  : name(id), args(std::move(arguments)), Expr(ExprKind::EXPR_FNCALL) {}
  void accept(NodeVisitor &v) override {}
};

class callArgList : public Node {
private:
  std::vector<std::unique_ptr<Expr>> args;

public:
  explicit callArgList(std::vector<std::unique_ptr<Expr>> a) : args(std::move(a)) {}
  size_t getSize() {
	return args.size();
  }

  std::vector<std::unique_ptr<Expr>> &getArgsVec() {
	return args;
  };
  void accept(NodeVisitor &v) override {};
};
