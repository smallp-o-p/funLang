#pragma once
#include "Lex.hpp"
#include <llvm-14/llvm/IR/Constant.h>
#include <llvm-14/llvm/IR/Value.h>
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
class ASTNode {
public:
  virtual ~ASTNode(){};
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

/*
 * functions -> func functions
 *            | func
 *
 * */

class functionsNode : public ASTNode {
private:
  std::unique_ptr<funcNode> func;
  std::unique_ptr<functionsNode> moreFuncs;

public:
  functionsNode(std::unique_ptr<funcNode> fn,
                std::unique_ptr<functionsNode> otherFuncs) {
    func = std::move(fn);
    moreFuncs = std::move(otherFuncs);
  }
  functionsNode(std::unique_ptr<funcNode> onlyFunc) {
    moreFuncs = nullptr;
    func = std::move(onlyFunc);
  };
  functionsNode() {
    func = nullptr;
    moreFuncs = nullptr;
  };

  void setMoreFuncs(std::unique_ptr<functionsNode> funcs) {
    moreFuncs = std::move(funcs);
  }

  ~functionsNode();
  llvm::Value *codegen();
};

/*
 * func --> type proto compoundstmt
 *
 * */

class funcNode : public ASTNode {
private:
  std::unique_ptr<typeNode> returnType;
  std::unique_ptr<protoNode> proto;
  std::unique_ptr<compoundStmtNode> compoundStmt;

public:
  funcNode(std::unique_ptr<typeNode> type, std::unique_ptr<protoNode> prototype,
           std::unique_ptr<compoundStmtNode> compound) {
    returnType = std::move(type);
    proto = std::move(prototype);
    compoundStmt = std::move(compound);
  }
  ~funcNode();
  llvm::Value *codegen();
};

// proto --> identifier '(' args ')'
class protoNode : public ASTNode {
private:
  std::unique_ptr<argsNode> argList;
  std::string id;

public:
  protoNode(std::string name, std::unique_ptr<argsNode> args) {
    argList = std::move(args);
    id = name;
  }
  ~protoNode() = default;

  llvm::Value *codegen();
};

class typeNode : public ASTNode {
private:
  DataTypes type;

public:
  typeNode(Tok::Token tok);
  ~typeNode();
  bool invalid() { return type == INVALID; }
  DataTypes getType() { return type; }

  llvm::Value *codegen();
};

class exprNode : public ASTNode {
private:
  std::unique_ptr<assignExprNode> assign;

public:
  exprNode(std::unique_ptr<assignExprNode> ass) { assign = std::move(ass); }
};

class fnCallNode : public ASTNode {
private:
  std::string name;
  std::unique_ptr<callArgsNode> args;

public:
  fnCallNode(std::string id, std::unique_ptr<callArgsNode> calls) {
    name = id;
    args = std::move(calls);
  }
};

/*
 * primary --> ID
 *          | NUM
 *          | STRINGLIT
 *          | fnCall
 *          | '(' expr ')'
 * */
class primaryNode : public ASTNode {
private:
  std::string id_name;
  std::string lexed_lit_val;
  std::unique_ptr<ASTNode> non_terminal_ptr;
  DataTypes type;

public:
  primaryNode(std::string identifier) {
    id_name = identifier;
    non_terminal_ptr = nullptr;
    lexed_lit_val = nullptr;
  }
  primaryNode(std::unique_ptr<ASTNode> expr) {
    non_terminal_ptr = std::move(expr);
    id_name = nullptr;
    lexed_lit_val = nullptr;
  }
  primaryNode(std::string litVal, Tok::Token typ) {
    switch (typ) {
    case (Tok::STRINGLIT):
      if (litVal.length() == 1) {
        type = CHAR;
      } else {
        type = STRING;
      }
      break;
    case (Tok::NUM):
      try {
        std::stoi(litVal);
        type = i32;
      } catch (std::out_of_range) {
        type = i64;
      }
      break;
    case (Tok::POINTNUM):
      try {
        std::stof(litVal);
        type = f32;
      } catch (std::out_of_range) {
        type = f64;
      }
      break;
    case (Tok::FALSE):
    case (Tok::TRUE):
      type = BOOL;
      break;
    default:
      type = INVALID;
    }
    lexed_lit_val = litVal;
  }

  DataTypes getType() { return type; }
};

class callArgsNode : public ASTNode {
private:
  std::unique_ptr<primaryNode> arg;
  std::unique_ptr<callArgsNode> moreArgs;

public:
  callArgsNode(std::unique_ptr<primaryNode> prim,
               std::unique_ptr<callArgsNode> more) {
    arg = std::move(prim);
    moreArgs = std::move(more);
  }
};

class unaryNode : public ASTNode {
private:
  std::unique_ptr<primaryNode> operand;
  Operator op;

public:
  unaryNode(Tok::Token opr, std::unique_ptr<primaryNode> rhs) {
    operand = std::move(rhs);
    switch (opr) {
    case (Tok::PLUSPLUS):
      op = INCREMENT;
      break;
    case (Tok::MINUSMINUS):
      op = DECREMENT;
      break;
    case (Tok::BANG):
      op = BANG;
      break;
    case (Tok::MINUS):
      op = SUBTRACT;
      break;
    default:
      op = NONE;
    }
  }

  unaryNode(std::unique_ptr<primaryNode> rhs) { operand = std::move(rhs); }
};
/*
 * multdiv --> unary ('*'| '/') unary
 *          |  unary
 *
 * */
class multdivNode : public ASTNode {
private:
  std::unique_ptr<unaryNode> lhs;
  std::unique_ptr<unaryNode> rhs;
  Operator op;

public:
  multdivNode(std::unique_ptr<unaryNode> left, Tok::Token opr,
              std::unique_ptr<unaryNode> right) {
    lhs = std::move(left);
    rhs = std::move(right);
    switch (opr) {
    case (Tok::MULT):
      op = MULT;
      break;
    case (Tok::DIV):
      op = DIV;
      break;
    default:
      op = NONE;
    }
  }

  multdivNode(std::unique_ptr<unaryNode> left) {
    lhs = std::move(left);
    rhs = nullptr;
    op = NONE;
  }
};

/*
 *  addExpr --> multdiv ('*'| '/') multdiv
 *           |  multdiv
 *
 * */

class addExprNode : public ASTNode {
private:
  std::unique_ptr<multdivNode> lhs;
  std::unique_ptr<multdivNode> rhs;
  Operator op;

public:
  addExprNode(std::unique_ptr<multdivNode> left, Tok::Token opr,
              std::unique_ptr<multdivNode> right) {
    lhs = std::move(left);
    rhs = std::move(right);
    switch (opr) {
    case (Tok::PLUS):
      op = ADD;
      break;
    case (Tok::MINUS):
      op = SUBTRACT;
      break;
    default:
      op = NONE;
    }
  }
  addExprNode(std::unique_ptr<multdivNode> left) {
    lhs = std::move(left);
    rhs = nullptr;
    op = NONE;
  }
};

/*
 * cmpExpr --> addExpr ('<' | '<=' | '>' | '>=') addExpr
 *          |  addExpr
 *
 * */

class cmpExprNode : public ASTNode {
private:
  std::unique_ptr<addExprNode> lhs;
  std::unique_ptr<addExprNode> rhs;
  Operator op;

public:
  cmpExprNode(std::unique_ptr<addExprNode> left,
              std::unique_ptr<addExprNode> right, Tok::Token opr) {
    lhs = std::move(left);
    rhs = std::move(right);
    switch (opr) {
    case (Tok::LTCMP):
      op = LT;
      break;
    case (Tok::LTECMP):
      op = LTE;
      break;
    case (Tok::GTCMP):
      op = GT;
      break;
    case (Tok::GTECMP):
      op = GTE;
      break;
    default:
      op = NONE;
    }
  }
  cmpExprNode(std::unique_ptr<addExprNode> left) {
    lhs = std::move(left);
    rhs = nullptr;
    op = NONE;
  }
};
/*
 * eqExpr --> cmpExpr ('==' | '!=') cmpExpr
 *          | cmpExpr
 *
 * */
class eqExprNode : public ASTNode {
private:
  std::unique_ptr<cmpExprNode> lhs;
  std::unique_ptr<cmpExprNode> rhs;
  Operator op;

public:
  eqExprNode(std::unique_ptr<cmpExprNode> left,
             std::unique_ptr<cmpExprNode> right, Tok::Token opr) {
    lhs = std::move(left);
    rhs = std::move(right);
    if (opr == Tok::NECMP) {
      op = NE;
    } else if (opr == Tok::EQCMP) {
      op = EQEQ;
    } else {
      op = NONE;
    }
  }
  eqExprNode(std::unique_ptr<cmpExprNode> left) {
    lhs = std::move(left);
    rhs = nullptr;
    op = NONE;
  }
};
/*
 * assign --> eqExpr
 *          | ID '=' expr
 *
 * */
class assignExprNode : public ASTNode {
private:
  std::unique_ptr<ASTNode> eq;
  std::string id;
  std::unique_ptr<exprNode> expr;

public:
  assignExprNode(std::unique_ptr<eqExprNode> eq_expr) {
    eq = std::move(eq_expr);
    id = "";
    expr = nullptr;
  }
  assignExprNode(std::string identifier, std::unique_ptr<exprNode> expr_ptr) {
    eq = nullptr;
    id = identifier;
    expr = std::move(expr_ptr);
  }
};

class declareNode : public ASTNode {
private:
  std::unique_ptr<typeNode> type;
  std::string id;
  std::unique_ptr<exprNode> expr;

public:
  declareNode(std::unique_ptr<typeNode> t, std::string i,
              std::unique_ptr<exprNode> exp) {
    type = std::move(t);
    id = i;
    expr = std::move(exp);
  }
};

class returnNode : public ASTNode {
private:
  std::unique_ptr<exprNode> expr;

public:
  returnNode(std::unique_ptr<exprNode> exp) { expr = std::move(exp); }
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
  simpleStmtNode(std::unique_ptr<ASTNode> decl) { stmt = std::move(decl); };

  llvm::Value *codegen();
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

public:
  simpleListNode(std::unique_ptr<simpleStmtNode> simple,
                 std::unique_ptr<simpleListNode> more) {
    simpleStmt = std::move(simple);
    moreStmts = std::move(more);
  }

  ~simpleListNode() = default;

  llvm::Value *codegen();
};

class compoundStmtNode : public ASTNode {
private:
  std::unique_ptr<simpleListNode> simpleList;

public:
  compoundStmtNode(std::unique_ptr<simpleListNode> simples) {
    simpleList = std::move(simples);
  }

  ~compoundStmtNode() = default;

  llvm::Value *codegen();
};

// nodes for functions

// ------------ args --------------- //

/*
 * arg --> type identifier
 *
 * */

class argNode : public ASTNode {
private:
  std::unique_ptr<typeNode> type;
  std::string id;

public:
  argNode(std::unique_ptr<typeNode> nodeType, std::string identifier) {
    type = std::move(nodeType);
    id = identifier;
  }
  argNode() { // no args
    type = nullptr;
  }
  ~argNode() = default;

  llvm::Value *codegen();
};

/*
 * args --> arg ',' args
 *       |  arg
 * */

class argsNode : public ASTNode {

private:
  std::unique_ptr<argNode> argu;
  std::unique_ptr<argsNode> moreArgs;

public:
  argsNode(std::unique_ptr<argNode> arg, std::unique_ptr<argsNode> moreArgus) {
    argu = std::move(arg);
    moreArgs = std::move(moreArgus);
  };
  argsNode(std::unique_ptr<argNode> arg) {
    argu = std::move(arg);
    moreArgs = nullptr;
  };
  ~argsNode() = default;
  llvm::Value *codegen();
};
// ------------- end args ----------- //

// ---------- func/functions -------- //

/*
 * program --> functions EOF
 *
 * */

class programNode : public ASTNode {
private:
  std::unique_ptr<functionsNode> funcs;

public:
  void setFuncs(std::unique_ptr<functionsNode> fns) { funcs = std::move(fns); }

  ~programNode();
  llvm::Value *codegen();
};
// ---------- end func/functions -------- //
