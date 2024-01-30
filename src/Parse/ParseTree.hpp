#pragma once
#include "../Lex/Lex.hpp"
#include <map>
#include <memory>
#include <stdexcept>
#include <string>
#include <unordered_map>

/*
 * TODO: find a way to untangle this with CMake */

enum DataTypes {
  VOID = 10000,
  CHAR,
  BOOL,
  i32,
  i64,
  f32,
  f64,
  STRING,
  INVALID
};

enum Operator {
  EQEQ = -9999,
  NE,
  GTE,
  LTE,
  GT,
  LT,
  MULT,
  DIV,
  INCREMENT,
  DECREMENT,
  BANG,
  SUBTRACT,
  ADD,
  NONE
};

enum stmtType { declStmt, retStmt, exprStmt };

class functionsNode;
class funcNode;
class protoNode;
class argsNode;
class argNode;
class compoundStmtNode;
class simpleListNode;
class simpleStmtNode;
class declareNode;
class exprNode;
class returnNode;
class assignExprNode;
class eqExprNode;
class cmpExprNode;
class addExprNode;
class multdivNode;
class unaryNode;
class primaryNode;
class fnCallNode;
class callArgsNode;
class typeNode;
class programNode;

class ParseTreeNode {
public:
  virtual ~ParseTreeNode(){};
};
class BinaryOpNode : ParseTreeNode {
protected:
  std::unique_ptr<BinaryOpNode> lhs;
  std::unique_ptr<BinaryOpNode> rhs;
  Operator op;

public:
  bool isFallthrough() { return rhs == nullptr; }
  const std::unique_ptr<BinaryOpNode> &getLHS() { return lhs; };
  const std::unique_ptr<BinaryOpNode> &getRHS() { return rhs; };
  virtual ~BinaryOpNode(){};
};

/*
 * program --> functions EOF
 *
 * */

class programNode : public ParseTreeNode {
private:
  std::unique_ptr<functionsNode> funcs;

public:
  programNode(std::unique_ptr<functionsNode> fns);
  ~programNode() = default;
};
/*
 * functions -> func functions
 *            | func
 *
 * */

class functionsNode : public ParseTreeNode {
private:
  std::vector<std::unique_ptr<funcNode>> funcs;

public:
  functionsNode();
  void addFunc(std::unique_ptr<funcNode> func);
  std::vector<std::unique_ptr<funcNode>> &getFuncs();
  ~functionsNode() = default;
};

/*
 * func --> type proto compoundstmt
 *
 * */

class funcNode : public ParseTreeNode {
private:
  std::unique_ptr<protoNode> proto;
  std::unique_ptr<compoundStmtNode> compoundStmt;

public:
  funcNode(std::unique_ptr<protoNode> prototype,
           std::unique_ptr<compoundStmtNode> compound);
  ~funcNode() = default;
  std::unique_ptr<protoNode> &getProto();
  std::unique_ptr<compoundStmtNode> &getCompound();
};

// proto --> identifier '(' args ')'
class protoNode : public ParseTreeNode {
private:
  std::unique_ptr<typeNode> returnType;
  std::unique_ptr<argsNode> argList;
  std::string id;

public:
  protoNode(std::unique_ptr<typeNode> ret, std::string name,
            std::unique_ptr<argsNode> args);
  ~protoNode() = default;
  std::unique_ptr<argsNode> &getArgsNode();
  DataTypes getTypeVal();
  std::string &getId();
};

/*
 * args --> arg ',' args
 *       |  arg
 * */
class argsNode : public ParseTreeNode {
private:
  std::vector<std::unique_ptr<argNode>> args;

public:
  argsNode();
  ~argsNode() = default;
  void addArg(std::unique_ptr<argNode> arg);
  std::vector<std::unique_ptr<argNode>> &getArgs();
  bool hasArgs();
};

/*
 * arg --> type identifier
 *
 * */
class argNode : public ParseTreeNode {
private:
  std::unique_ptr<typeNode> type;
  std::string id;

public:
  argNode(std::unique_ptr<typeNode> nodeType, std::string identifier);
  ~argNode() = default;
  std::string &getId();
  DataTypes getTypeVal();
};

/**
  compoundStmt --> '{' simpleList '}'
*/
class compoundStmtNode : public ParseTreeNode {
private:
  std::unique_ptr<simpleListNode> simpleList;

public:
  compoundStmtNode(std::unique_ptr<simpleListNode> simples);
  ~compoundStmtNode() = default;

  std::unique_ptr<simpleListNode> &getList() { return simpleList; }
};

/*
 * simpleList --> simpleStmt simpleList
 *             |  simpleStmt
 *
 * */

class simpleListNode : public ParseTreeNode {
private:
  std::unique_ptr<simpleStmtNode> simpleStmt;
  std::unique_ptr<simpleListNode> moreStmts;
  std::vector<std::unique_ptr<simpleStmtNode>> simpleStmts;

public:
  simpleListNode();
  void addStmt(std::unique_ptr<simpleStmtNode> stmt);
  ~simpleListNode() = default;
  std::vector<std::unique_ptr<simpleStmtNode>> &getStmts();
};

/*
 * simpleStmt --> declare
 *              | expr
 *              | return
 * */

class simpleStmtNode : public ParseTreeNode {
private:
  std::unique_ptr<ParseTreeNode> stmt;
  stmtType stmtType;

public:
  simpleStmtNode(std::unique_ptr<ParseTreeNode> decl, enum stmtType stmtT);
  enum stmtType getStmtT();
  std::unique_ptr<ParseTreeNode> &getChild();
};

class declareNode : public ParseTreeNode {
private:
  std::unique_ptr<typeNode> type;
  std::string id;
  std::unique_ptr<exprNode> expr;

public:
  declareNode(std::unique_ptr<typeNode> t, std::string i,
              std::unique_ptr<exprNode> exp);
  std::string &getId();
};

/*
 * return --> 'return' expr ';'
 *
 *
 */
class returnNode : public ParseTreeNode {
private:
  std::unique_ptr<exprNode> expr;

public:
  returnNode(std::unique_ptr<exprNode> exp);
};

class typeNode : public ParseTreeNode {
private:
  DataTypes type;

public:
  typeNode(Tok::Token tok);
  ~typeNode() = default;
  bool invalid();
  DataTypes getType();
};

class exprNode : public ParseTreeNode {
private:
  std::unique_ptr<assignExprNode> assign;

public:
  exprNode(std::unique_ptr<assignExprNode> ass);
  ~exprNode() = default;
};

/*
 * assign --> eqExpr
 *          | ID '=' expr
 *
 * */
class assignExprNode : public ParseTreeNode {
private:
  std::unique_ptr<BinaryOpNode> eq;
  std::string id;
  std::unique_ptr<exprNode> expr;

public:
  assignExprNode(std::unique_ptr<eqExprNode> eq_expr);
  assignExprNode(std::string identifier, std::unique_ptr<exprNode> expr_ptr);
  ~assignExprNode() = default;
  std::string &getId();
};

/*
 * cmpExpr --> addExpr ('<' | '<=' | '>' | '>=') addExpr
 *          |  addExpr
 *
 * */

class cmpExprNode : public BinaryOpNode {
private:
public:
  cmpExprNode(std::unique_ptr<addExprNode> left,
              std::unique_ptr<addExprNode> right, Tok::Token opr);
  cmpExprNode(std::unique_ptr<addExprNode> left);

  std::unique_ptr<addExprNode> &getLHS();
  Operator getOp();
};

/*
 * eqExpr --> cmpExpr ('==' | '!=') cmpExpr
 *          | cmpExpr
 *
 * */
class eqExprNode : public BinaryOpNode {
private:
public:
  eqExprNode(std::unique_ptr<cmpExprNode> left,
             std::unique_ptr<cmpExprNode> right, Tok::Token opr);
  eqExprNode(std::unique_ptr<cmpExprNode> left);
  Operator getOp();
};
/*
 *  addExpr --> multdiv ('*'| '/') multdiv
 *           |  multdiv
 *
 * */

class addExprNode : public BinaryOpNode {
public:
  addExprNode(std::unique_ptr<multdivNode> left, Tok::Token opr,
              std::unique_ptr<multdivNode> right);
  addExprNode(std::unique_ptr<multdivNode> left);
  Operator getOp();
};

/*
 * multdiv --> unary ('*'| '/') unary
 *          |  unary
 *
 * */
class multdivNode : public BinaryOpNode {
protected:
  std::unique_ptr<unaryNode> lhs;
  std::unique_ptr<unaryNode> rhs;
  Operator op;

public:
  multdivNode(std::unique_ptr<unaryNode> left, Tok::Token opr,
              std::unique_ptr<unaryNode> right);
  multdivNode(std::unique_ptr<unaryNode> left);
  Operator getOp();
};

class unaryNode : public ParseTreeNode {
private:
  std::unique_ptr<primaryNode> operand;
  Operator op;

public:
  unaryNode(Tok::Token opr, std::unique_ptr<primaryNode> rhs);
  unaryNode(std::unique_ptr<primaryNode> rhs);
};

/*
 * primary --> ID
 *          | NUM
 *          | STRINGLIT
 *          | 'call' fnCall
 *          | '(' expr ')'
 * */
class primaryNode : public ParseTreeNode {
public:
  enum primaryType { EXPR, SYMBOL, LITERAL, FUNCCALL };
  primaryNode(std::string identifier);
  primaryNode(std::unique_ptr<exprNode> expr);
  primaryNode(std::unique_ptr<fnCallNode> fncall);
  primaryNode(std::string litVal, Tok::Token typ);
  DataTypes getType();

private:
  std::string id_name;
  std::string lexed_lit_val;
  std::unique_ptr<ParseTreeNode> non_terminal_ptr;
  std::unique_ptr<fnCallNode> fnc;
  DataTypes type;
  primaryType primType;
};

/**
  fnCall -> identifier '(' callArgs ')'

  */
class fnCallNode : public ParseTreeNode {
private:
  std::string fnName;
  std::unique_ptr<callArgsNode> args;

public:
  fnCallNode(std::string &name, std::unique_ptr<callArgsNode> calls);
};

class callArgsNode : public ParseTreeNode {
private:
  std::vector<std::unique_ptr<primaryNode>> callArgs;

public:
  callArgsNode(std::vector<std::unique_ptr<primaryNode>> primaries);
};
