#pragma once
#include "Lex.hpp"
#include <map>
#include <memory>
#include <stdexcept>
#include <string>

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

class ASTNode {
public:
  virtual ~ASTNode(){};
};
class BinaryOpNode : ASTNode {
private:
  std::unique_ptr<BinaryOpNode> lhs;
  std::unique_ptr<BinaryOpNode> rhs;
  Operator op;

public:
  virtual ~BinaryOpNode(){};
};

/*
 * program --> functions EOF
 *
 * */

class programNode : public ASTNode {
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

class functionsNode : public ASTNode {
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

class funcNode : public ASTNode {
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
class protoNode : public ASTNode {
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
class argsNode : public ASTNode {
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
class argNode : public ASTNode {
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
class compoundStmtNode : public ASTNode {
private:
  std::unique_ptr<simpleListNode> simpleList;

public:
  compoundStmtNode(std::unique_ptr<simpleListNode> simples);
  ~compoundStmtNode() = default;
};

/*
 * simpleList --> simpleStmt simpleList
 *             |  simpleStmt
 *
 * */

class simpleListNode : public ASTNode {
private:
  std::unique_ptr<simpleStmtNode> simpleStmt;
  std::unique_ptr<simpleListNode> moreStmts;
  std::vector<std::unique_ptr<simpleStmtNode>> simpleStmts;

public:
  simpleListNode();
  void addStmt(std::unique_ptr<simpleStmtNode> stmt);
  ~simpleListNode() = default;
};

/*
 * simpleStmt --> declare
 *              | expr
 *              | return
 * */

class simpleStmtNode : public ASTNode {
private:
  std::unique_ptr<ASTNode> stmt;

public:
  simpleStmtNode(std::unique_ptr<ASTNode> decl);
};

class declareNode : public ASTNode {
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
class returnNode : public ASTNode {
private:
  std::unique_ptr<exprNode> expr;

public:
  returnNode(std::unique_ptr<exprNode> exp);
};

class typeNode : public ASTNode {
private:
  DataTypes type;

public:
  typeNode(Tok::Token tok);
  ~typeNode() = default;
  bool invalid();
  DataTypes getType();
};

class exprNode : public ASTNode {
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
class assignExprNode : public ASTNode {
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
  std::unique_ptr<addExprNode> lhs;
  std::unique_ptr<addExprNode> rhs;
  Operator op;

public:
  cmpExprNode(std::unique_ptr<addExprNode> left,
              std::unique_ptr<addExprNode> right, Tok::Token opr);
  cmpExprNode(std::unique_ptr<addExprNode> left);
  const std::unique_ptr<addExprNode> &getLHS();
  const std::unique_ptr<addExprNode> &getRHS();
  Operator getOp();
};

/*
 * eqExpr --> cmpExpr ('==' | '!=') cmpExpr
 *          | cmpExpr
 *
 * */
class eqExprNode : public BinaryOpNode {
private:
  std::unique_ptr<cmpExprNode> lhs;
  std::unique_ptr<cmpExprNode> rhs;
  Operator op;

public:
  eqExprNode(std::unique_ptr<cmpExprNode> left,
             std::unique_ptr<cmpExprNode> right, Tok::Token opr);
  eqExprNode(std::unique_ptr<cmpExprNode> left);
  const std::unique_ptr<cmpExprNode> &getLHS();
  const std::unique_ptr<cmpExprNode> &getRHS();
  Operator getOp();
};
/*
 *  addExpr --> multdiv ('*'| '/') multdiv
 *           |  multdiv
 *
 * */

class addExprNode : public BinaryOpNode {
private:
  std::unique_ptr<multdivNode> lhs;
  std::unique_ptr<multdivNode> rhs;
  Operator op;

public:
  addExprNode(std::unique_ptr<multdivNode> left, Tok::Token opr,
              std::unique_ptr<multdivNode> right);
  addExprNode(std::unique_ptr<multdivNode> left);
  const std::unique_ptr<multdivNode> &getLHS();
  const std::unique_ptr<multdivNode> &getRHS();
  Operator getOp();
};

/*
 * multdiv --> unary ('*'| '/') unary
 *          |  unary
 *
 * */
class multdivNode : public BinaryOpNode {
private:
  std::unique_ptr<unaryNode> lhs;
  std::unique_ptr<unaryNode> rhs;
  Operator op;

public:
  multdivNode(std::unique_ptr<unaryNode> left, Tok::Token opr,
              std::unique_ptr<unaryNode> right);
  multdivNode(std::unique_ptr<unaryNode> left);
  const std::unique_ptr<unaryNode> &getLHS();
  const std::unique_ptr<unaryNode> &getRHS();
  Operator getOp();
};

class unaryNode : public ASTNode {
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
class primaryNode : public ASTNode {
private:
  std::string id_name;
  std::string lexed_lit_val;
  std::unique_ptr<ASTNode> non_terminal_ptr;
  DataTypes type;

public:
  primaryNode(std::string identifier);
  primaryNode(std::unique_ptr<ASTNode> expr);
  primaryNode(std::string litVal, Tok::Token typ);
  DataTypes getType();
};

/**
  fnCall -> identifier '(' callArgs ')'

  */
class fnCallNode : public ASTNode {
private:
  std::string name;
  std::unique_ptr<callArgsNode> args;

public:
  fnCallNode(std::string id, std::unique_ptr<callArgsNode> calls);
};

class callArgsNode : public ASTNode {
private:
  std::vector<std::unique_ptr<primaryNode>> callArgs;

public:
  callArgsNode(std::vector<std::unique_ptr<primaryNode>> primaries);
};
