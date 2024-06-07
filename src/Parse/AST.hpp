#pragma once
#include "Lex.hpp"
#include "Basic.hpp"
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/APFloat.h"

class TypeProperties;
class CompilationUnit;
class TopLevelDecls;
class FunctionNode;
class ArgsList;
class ArgDecl;
class CompoundStmt;
class Stmt;
class matchStmt;
class matchArm;
class forStmt;
class whileStmt;
class loopStmt;
class Decl;
class TypeDecl;
class TypeUse;
class VarDeclStmt;
class Expr;
class ReturnStmt;
class FunctionCall;
class CallArgList;
class BinaryOp;
class UnaryOp;
class NameUsage;
class FloatingLiteral;
class IntegerLiteral;
class BooleanLiteral;
class StringLiteral;
class CharLiteral;

namespace funLang {

class SemaAnalyzer;

}

class Node {
protected:
  llvm::SMLoc locationInSrc;
public:
  Node() : locationInSrc(llvm::SMLoc()) {}
  explicit Node(llvm::SMLoc loc) : locationInSrc(loc) {}

  llvm::SMLoc getLoc() { return locationInSrc; }
  virtual void accept(funLang::SemaAnalyzer &visitor) {}
};

class CompilationUnit : public Node {
private:
  std::unique_ptr<TopLevelDecls> funcs;
  std::unique_ptr<std::unordered_map<std::string, int>> globalSymbols;
public:
  explicit CompilationUnit(std::unique_ptr<TopLevelDecls> fncs)
	  : funcs(std::move(fncs)), globalSymbols(nullptr) {};
  CompilationUnit(std::unique_ptr<TopLevelDecls> fncs,
				  std::unique_ptr<std::unordered_map<std::string, int>> globs)
	  : funcs(std::move(fncs)), globalSymbols(std::move(globs)) {}
  auto &getGlobs();
  void accept(funLang::SemaAnalyzer &visitor) override {}
};

class Decl : public Node {
public:
  enum DeclKind {
	DK_FN,
	DK_VAR,
	DK_TYPE,
	DK_ARG,
  };
private:
  DeclKind kind;
  llvm::StringRef name;

public:
  explicit Decl(DeclKind kind, llvm::StringRef name) : kind(kind), name(name), Node(llvm::SMLoc()) {}
  Decl(DeclKind kind, llvm::StringRef name, llvm::SMLoc loc) : kind(kind), name(name), Node(loc) {}

  DeclKind getKind() const { return kind; }
  llvm::StringRef getName() { return name; }
  void accept(funLang::SemaAnalyzer &v) override {};
};

class TypeDecl : public Decl {
private:
  std::unique_ptr<TypeProperties> properties;
public:
  TypeDecl(llvm::StringRef name, std::unique_ptr<TypeProperties> properties, llvm::SMLoc loc)
	  : Decl(DK_TYPE, name, loc), properties(std::move(properties)) {}
  explicit TypeDecl(llvm::StringRef name) : Decl(DK_TYPE, name), properties(nullptr) {}

  TypeProperties &getProperties() {
	return *properties;
  }

  static bool classof(const Decl *d) {
	return d->getKind() == DK_TYPE;
  }

  bool eq(TypeDecl &other) {
	return other.getName() == this->getName();
  }

  static bool areTypesEqual(TypeDecl &Lhs, TypeDecl &Rhs) {
	return Lhs.getName() == Rhs.getName();
  }
};

class TypeUse : public Node {
private:
  TypeDecl *type;

public:
  explicit TypeUse(llvm::SMLoc loc) : type(nullptr) {}
  TypeUse(TypeDecl *type, llvm::SMLoc loc) : type(type), Node(loc) {}

  TypeDecl &getType() { return *type; }
  void accept(funLang::SemaAnalyzer &v) override {}
};

class TopLevelDecls : public Node {
private:
  std::unordered_map<std::string, std::unique_ptr<Decl>> fnMap;
public:
  explicit TopLevelDecls(std::unordered_map<std::string, std::unique_ptr<Decl>> fnMap)
	  : fnMap(std::move(fnMap)) {}

  void accept(funLang::SemaAnalyzer &v) override {}
  std::unordered_map<std::string, std::unique_ptr<Decl>> &getTopLevelMap();
};

class ArgsList : public Node {
private:
  std::vector<std::unique_ptr<ArgDecl>> argList;
public:
  explicit ArgsList(std::vector<std::unique_ptr<ArgDecl>> &args)
	  : argList(std::move(args)) {}

  void accept(funLang::SemaAnalyzer &v) override {}
  const std::vector<std::unique_ptr<ArgDecl>> &getArgList() { return argList; }
};

class ArgDecl : public Decl {
private:
  std::unique_ptr<TypeUse> type;
public:
  ArgDecl(std::unique_ptr<TypeUse> type, llvm::StringRef name, llvm::SMLoc loc)
	  : type(std::move(type)), Decl(DK_ARG, name, loc) {}

  void accept(funLang::SemaAnalyzer &v) override {}
};

class FunctionNode : public Decl {
private:
  std::unique_ptr<TypeUse> retType;
  std::unique_ptr<ArgsList> argDecls;
  std::unique_ptr<CompoundStmt> compound;
public:
  FunctionNode(
	  std::unique_ptr<TypeUse> retType,
	  llvm::StringRef name,
	  std::unique_ptr<ArgsList> argDecls,
	  std::unique_ptr<CompoundStmt> compound,
	  llvm::SMLoc loc)
	  : retType(std::move(retType)), argDecls(std::move(argDecls)), compound(std::move(compound)),
		Decl(DK_FN, name, loc) {}

  void accept(funLang::SemaAnalyzer &v) override {}
  const std::vector<std::unique_ptr<ArgDecl>> &getArgDecls() { return argDecls->getArgList(); };
  CompoundStmt &getCompound() const { return *compound; }
  static bool classof(const Decl *D) { return D->getKind() == DK_FN; }
};

class Stmt : public Node {
public:
  enum StmtKind {
	SK_VARDECL,
	SK_EXPR,
	SK_EXPR_BINARY,
	SK_EXPR_UNARY,
	SK_EXPR_FNCALL,
	SK_EXPR_LEAF,
	SK_EXPR_INT,
	SK_EXPR_FLOAT,
	SK_EXPR_BOOL,
	SK_EXPR_STRING,
	SK_EXPR_NAME,
	SK_RETURN,
	SK_COMPOUND,
	SK_IF,
	SK_FOR,
	SK_WHILE,
	SK_LOOP,
	SK_MATCH,
	SK_MATCHARM
  };
protected:
  StmtKind kind;
public:
  explicit Stmt(StmtKind k) : kind(k) {}
  Stmt(StmtKind k, llvm::SMLoc loc) : kind(k), Node(loc) {}

  StmtKind getKind() const { return kind; }
};

class CompoundStmt : public Stmt {
private:
  std::vector<std::unique_ptr<Stmt>> stmts;
public:
  explicit CompoundStmt(std::vector<std::unique_ptr<Stmt>> simples)
	  : stmts(std::move(simples)), Stmt(SK_COMPOUND) {}

  void accept(funLang::SemaAnalyzer &sema) override {};
  std::vector<std::unique_ptr<Stmt>> &getStmts();

  static bool classof(const Stmt *S) { return S->getKind() == SK_COMPOUND; }
};
class elifStmt : public Stmt {
private:
  std::unique_ptr<Expr> cond;
  std::unique_ptr<CompoundStmt> block;
  std::unique_ptr<elifStmt> elif;
public:
  elifStmt(StmtKind K,
		   llvm::SMLoc Loc,
		   std::unique_ptr<Expr> cond,
		   std::unique_ptr<CompoundStmt> block,
		   std::unique_ptr<elifStmt> elif)
	  : Stmt(K, Loc), cond(std::move(cond)), block(std::move(block)), elif(std::move(elif)) {}
};

class ifStmt : public Stmt {
private:
  std::unique_ptr<Expr> cond;
  std::unique_ptr<CompoundStmt> block1;
  std::unique_ptr<elifStmt> elif;
  std::unique_ptr<CompoundStmt> block2;
public:
  ifStmt(std::unique_ptr<Expr> cond,
		 std::unique_ptr<CompoundStmt> block1,
		 std::unique_ptr<elifStmt> elif,
		 std::unique_ptr<CompoundStmt> block2)
	  : cond(std::move(cond)), block1(std::move(block1)), elif(std::move(elif)), block2(std::move(block2)),
		Stmt(SK_IF) {}
};

class forStmt : public Stmt {
private:
  std::unique_ptr<Expr> var;
  std::unique_ptr<Expr> range;
  std::unique_ptr<Expr> iterator;
  std::unique_ptr<CompoundStmt> compound;
public:
  forStmt(
	  std::unique_ptr<Expr> var,
	  std::unique_ptr<Expr> range,
	  std::unique_ptr<Expr> iter,
	  std::unique_ptr<CompoundStmt> compound,
	  llvm::SMLoc loc)
	  : Stmt(SK_FOR, loc), var(std::move(var)), range(std::move(range)), iterator(std::move(iter)),
		compound(std::move(compound)) {}
};

class whileStmt : public Stmt {
private:
  std::unique_ptr<Expr> condition;
  std::unique_ptr<CompoundStmt> compound;
public:
  whileStmt(std::unique_ptr<Expr> condition, std::unique_ptr<CompoundStmt> compound, llvm::SMLoc loc)
	  : compound(std::move(compound)), condition(std::move(condition)), Stmt(SK_WHILE, loc) {}
};

class loopStmt : public Stmt {
private:
  std::unique_ptr<CompoundStmt> compound;
public:
  loopStmt(std::unique_ptr<CompoundStmt> compound, llvm::SMLoc loc)
	  : compound(std::move(compound)), Stmt(SK_LOOP, loc) {}
};
class matchArm : public Stmt {
private:
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<CompoundStmt> rhs;
public:
  matchArm(std::unique_ptr<Expr> lhs, std::unique_ptr<CompoundStmt> rhs, llvm::SMLoc begin)
	  : lhs(std::move(lhs)), rhs(std::move(rhs)), Stmt(SK_MATCHARM, begin) {}
};

class matchStmt : public Stmt {
private:
  std::vector<std::unique_ptr<matchArm>> arms;
public:
  matchStmt(std::vector<std::unique_ptr<matchArm>> arms, llvm::SMLoc loc)
	  : arms(std::move(arms)), Stmt(SK_MATCH, loc) {}
};

class VarDecl : public Decl {
private:
  TypeUse &type;
  Expr *expr;
public :
  VarDecl(TypeUse &t, llvm::StringRef name, Expr *expr)
	  : type(t), expr(expr), Decl(DK_VAR, name, llvm::SMLoc().getFromPointer(name.data())) {}

  TypeUse &getType() const { return type; }
  Expr *getExpr() const { return expr; }
  static bool classof(const Decl *D) { return D->getKind() == DK_VAR; }
};

class VarDeclStmt : public Stmt {
private:
  std::unique_ptr<TypeUse> type;
  llvm::StringRef name;
  std::unique_ptr<Expr> expr;
public:
  VarDeclStmt(std::unique_ptr<TypeUse> t, llvm::StringRef id, std::unique_ptr<Expr> expression, llvm::SMLoc loc)
	  : type(std::move(t)), name(id), expr(std::move(expression)), Stmt(SK_VARDECL, loc) {}
  VarDeclStmt(std::unique_ptr<TypeUse> t, llvm::StringRef id, llvm::SMLoc loc)
	  : type(std::move(t)), name(id), Stmt(SK_VARDECL, loc) {}

  llvm::StringRef getName() { return name; };
  Expr *getExpr();
  TypeUse &getTypeUse() { return *type; }
  VarDecl *toDecl() { return new VarDecl(*type, name, expr == nullptr ? nullptr : expr.get()); }
  void accept(funLang::SemaAnalyzer &v) override {}
  static bool classof(const Stmt *S) {
	return S->getKind() == StmtKind::SK_VARDECL;
  }
};

class TypeProperties {
private:
  std::vector<std::unique_ptr<VarDeclStmt>> decls;
public:
  explicit TypeProperties(std::vector<std::unique_ptr<VarDeclStmt>> decls)
	  : decls(std::move(decls)) {}
  std::vector<std::unique_ptr<VarDeclStmt>> &getDecls() {
	return decls;
  }
};

class Expr : public Stmt {
public:
  enum ExprKind {
	EXPR_BINARY,
	EXPR_UNARY,
	EXPR_FNCALL,
	EXPR_LEAF,
	EXPR_INT,
	EXPR_FLOAT,
	EXPR_BOOL,
	EXPR_STRING,
  };
  ExprKind kind;
protected:
  TypeDecl *resultType;
  bool IsLiteral;
public:
  explicit Expr(ExprKind k, StmtKind K) : kind(k), Stmt(K) {};
  Expr(ExprKind Kind, StmtKind StmtK, llvm::SMLoc Loc) : kind(Kind), Stmt(StmtK, Loc) {};

  void accept(funLang::SemaAnalyzer &v);
  void setType(TypeDecl *toSet);
  TypeDecl *getType() { return resultType; };
  ExprKind getExprKind() const { return kind; }
  static bool classof(StmtKind S) {
	return S == SK_EXPR;
  }
};

class NameUsage : public Expr {
protected:
  llvm::StringRef name;
public:
  explicit NameUsage(llvm::StringRef name)
	  : name(name), Expr(ExprKind::EXPR_LEAF, SK_EXPR_NAME, llvm::SMLoc().getFromPointer(name.data())) {}

  llvm::StringRef getLexeme() { return name; };
  void accept(funLang::SemaAnalyzer &v) override {}
};

class IntegerLiteral : public Expr {
private:
  llvm::APInt val;
public:
  explicit IntegerLiteral(llvm::APInt value, llvm::SMLoc litLoc)
	  : val(std::move(value)), Expr(EXPR_INT, SK_EXPR_INT, litLoc) {}
};

class FloatingLiteral : public Expr {
private:
  llvm::APFloat val;
public:
  explicit FloatingLiteral(llvm::APFloat value, llvm::SMLoc litLoc)
	  : val(std::move(value)), Expr(EXPR_FLOAT, SK_EXPR_FLOAT) {}
};

class BooleanLiteral : public Expr {
private:
  bool val;
public:
  explicit BooleanLiteral(bool value, llvm::SMLoc loc) : Expr(EXPR_BOOL, SK_EXPR_BOOL, loc), val(value) {}
};

class StringLiteral : public Expr {
private:
  llvm::StringRef str;
  uint32_t len;
public:
  explicit StringLiteral(uint32_t len, llvm::StringRef str, llvm::SMLoc loc) : Expr(EXPR_STRING, SK_EXPR_STRING, loc) {}
};

class FunctionCall : public Expr {
private:
  llvm::StringRef name;
  std::unique_ptr<CallArgList> args;

public:
  FunctionCall(llvm::StringRef name, std::unique_ptr<CallArgList> arguments, llvm::SMLoc loc)
	  : name(name), args(std::move(arguments)),
		Expr(ExprKind::EXPR_FNCALL, SK_EXPR_FNCALL, loc) {}

  void accept(funLang::SemaAnalyzer &v) override {}
  llvm::StringRef getName() const { return name; }
  const std::unique_ptr<CallArgList> &getArgs() const { return args; }

  static bool classof(StmtKind K) {
	return K == SK_EXPR_FNCALL;
  }
};

class UnaryOp : public Expr {
private:
  Basic::Op::Unary op;
  std::unique_ptr<Expr> input;
public:
  UnaryOp(std::unique_ptr<Expr> inp, Basic::Op::Unary opc)
	  : input(std::move(inp)), op(opc), Expr(ExprKind::EXPR_UNARY, SK_EXPR_UNARY, inp->getLoc()) {}
  void accept(funLang::SemaAnalyzer &v) override {}
  Basic::Op::Unary getOp() { return op; }
};

class BinaryOp : public Expr {
private:
  Basic::Op::Binary op;
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
public:
  BinaryOp(std::unique_ptr<Expr> left, std::unique_ptr<Expr> right,
		   Basic::Op::Binary opcode)
	  : lhs(std::move(left)), rhs(std::move(right)), op(opcode),
		Expr(ExprKind::EXPR_BINARY, StmtKind::SK_EXPR_BINARY) {}

  void accept(funLang::SemaAnalyzer &v) override {}
  Expr &getLhs() { return *lhs; }
  Expr &getRhs() { return *rhs; }
  TypeDecl *getLHSType() { return lhs->getType(); }
  TypeDecl *getRHSType() { return rhs->getType(); }
  llvm::SMRange getRange() { return {lhs->getLoc(), rhs->getLoc()}; };
  static bool classof(StmtKind K) { return K == StmtKind::SK_EXPR_BINARY; }
};

class CallArgList : public Node {
private:
  std::vector<std::unique_ptr<Expr>> args;
public:
  explicit CallArgList(std::vector<std::unique_ptr<Expr>> a) : args(std::move(a)) {}

  size_t getSize() { return args.size(); }
  std::vector<std::unique_ptr<Expr>> &getArgsVec() { return args; };
  void accept(funLang::SemaAnalyzer &v) override {};
};

class ReturnStmt : public Stmt {
private:
  std::unique_ptr<Expr> expr;
public:
  explicit ReturnStmt(std::unique_ptr<Expr> exprNode)
	  : expr(std::move(exprNode)), Stmt(SK_RETURN) {}

  void accept(funLang::SemaAnalyzer &v) override {}

  static bool classof(StmtKind S) {
	return S == SK_RETURN;
  }
};
