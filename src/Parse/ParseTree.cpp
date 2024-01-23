#include "ParseTree.hpp"
#include <cstdarg>
#include <cstdint>
#include <map>
#include <memory>
#include <string>

programNode::programNode(std::unique_ptr<functionsNode> fns) {
  funcs = std::move(fns);
}

functionsNode::functionsNode() {
  funcs = std::vector<std::unique_ptr<funcNode>>();
}
void functionsNode::addFunc(std::unique_ptr<funcNode> func) {
  funcs.push_back(std::move(func));
}
std::vector<std::unique_ptr<funcNode>> &functionsNode::getFuncs() {
  return funcs;
}

funcNode::funcNode(std::unique_ptr<protoNode> prototype,
                   std::unique_ptr<compoundStmtNode> compound) {
  proto = std::move(prototype);
  compoundStmt = std::move(compound);
}
std::unique_ptr<compoundStmtNode> &funcNode::getCompound() {
  return compoundStmt;
}

protoNode::protoNode(std::unique_ptr<typeNode> ret, std::string name,
                     std::unique_ptr<argsNode> args) {
  returnType = std::move(ret);
  argList = std::move(args);
  id = name;
}
DataTypes protoNode::getTypeVal() { return returnType->getType(); }
std::string &protoNode::getId() { return id; }
std::unique_ptr<argsNode> &protoNode::getArgsNode() { return argList; }
std::unique_ptr<protoNode> &funcNode::getProto() { return proto; }

argsNode::argsNode() { args = std::vector<std::unique_ptr<argNode>>(); }
void argsNode::addArg(std::unique_ptr<argNode> arg) {
  args.push_back(std::move(arg));
}
std::vector<std::unique_ptr<argNode>> &argsNode::getArgs() { return args; }
bool argsNode::hasArgs() { return args.empty(); }

argNode::argNode(std::unique_ptr<typeNode> nodeType, std::string identifier) {
  type = std::move(nodeType);
  id = identifier;
}
std::string &argNode::getId() { return id; }
DataTypes argNode::getTypeVal() { return type->getType(); }

// seriously debating the purpose of this node since we
// just keep reaching right through it...
typeNode::typeNode(Tok::Token tok) {
  switch (tok) {
  case (Tok::VOID):
    type = VOID;
    break;
  case (Tok::I32):
    type = i32;
    break;
  case (Tok::I64):
    type = i64;
    break;
  case (Tok::F32):
    type = f32;
    break;
  case (Tok::F64):
    type = f64;
    break;
  case (Tok::CHAR):
    type = CHAR;
    break;
  case (Tok::STRING):
    type = STRING;
    break;
  case (Tok::BOOL):
    type = BOOL;
    break;
  default:
    type = INVALID;
    break;
  }
};
bool typeNode::invalid() { return type == INVALID; }
DataTypes typeNode::getType() { return type; }

returnNode::returnNode(std::unique_ptr<exprNode> exp) { expr = std::move(exp); }

compoundStmtNode::compoundStmtNode(std::unique_ptr<simpleListNode> simples) {
  simpleList = std::move(simples);
}

simpleListNode::simpleListNode() {
  simpleStmts = std::vector<std::unique_ptr<simpleStmtNode>>();
}
void simpleListNode::addStmt(std::unique_ptr<simpleStmtNode> stmt) {
  simpleStmts.push_back(std::move(stmt));
}
std::vector<std::unique_ptr<simpleStmtNode>> &simpleListNode::getStmts() {
  return simpleStmts;
}

simpleStmtNode::simpleStmtNode(std::unique_ptr<ParseTreeNode> decl,
                               enum stmtType stmtT) {
  stmt = std::move(decl);
  stmtType = stmtT;
};

std::unique_ptr<ParseTreeNode> &simpleStmtNode::getChild() { return stmt; };
stmtType simpleStmtNode::getStmtT() { return stmtType; }

declareNode::declareNode(std::unique_ptr<typeNode> t, std::string i,
                         std::unique_ptr<exprNode> exp) {
  type = std::move(t);
  id = i;
  expr = std::move(exp);
}
std::string &declareNode::getId() { return id; }

exprNode::exprNode(std::unique_ptr<assignExprNode> ass) {
  assign = std::move(ass);
}

assignExprNode::assignExprNode(std::unique_ptr<eqExprNode> eq_expr) {
  eq = std::move(eq_expr);
  id = "";
  expr = nullptr;
}
assignExprNode::assignExprNode(std::string identifier,
                               std::unique_ptr<exprNode> expr_ptr) {
  eq = nullptr;
  id = identifier;
  expr = std::move(expr_ptr);
}
std::string &assignExprNode::getId() { return id; }

eqExprNode::eqExprNode(std::unique_ptr<cmpExprNode> left,
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
eqExprNode::eqExprNode(std::unique_ptr<cmpExprNode> left) {
  lhs = std::move(left);
  rhs = nullptr;
  op = NONE;
}
const std::unique_ptr<cmpExprNode> &eqExprNode::getLHS() { return lhs; }
const std::unique_ptr<cmpExprNode> &eqExprNode::getRHS() { return rhs; }
Operator eqExprNode::getOp() { return op; }

cmpExprNode::cmpExprNode(std::unique_ptr<addExprNode> left,
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
cmpExprNode::cmpExprNode(std::unique_ptr<addExprNode> left) {
  lhs = std::move(left);
  rhs = nullptr;
  op = NONE;
}
const std::unique_ptr<addExprNode> &cmpExprNode::getLHS() { return lhs; }
const std::unique_ptr<addExprNode> &cmpExprNode::getRHS() { return rhs; }
Operator cmpExprNode::getOp() { return op; }

addExprNode::addExprNode(std::unique_ptr<multdivNode> left, Tok::Token opr,
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
addExprNode::addExprNode(std::unique_ptr<multdivNode> left) {
  lhs = std::move(left);
  rhs = nullptr;
  op = NONE;
}
const std::unique_ptr<multdivNode> &addExprNode::getLHS() { return lhs; }
const std::unique_ptr<multdivNode> &addExprNode::getRHS() { return rhs; }
Operator addExprNode::getOp() { return op; }

multdivNode::multdivNode(std::unique_ptr<unaryNode> left, Tok::Token opr,
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
multdivNode::multdivNode(std::unique_ptr<unaryNode> left) {
  lhs = std::move(left);
  rhs = nullptr;
  op = NONE;
}
const std::unique_ptr<unaryNode> &multdivNode::getLHS() { return lhs; }
const std::unique_ptr<unaryNode> &multdivNode::getRHS() { return rhs; }
Operator multdivNode::getOp() { return op; }

unaryNode::unaryNode(Tok::Token opr, std::unique_ptr<primaryNode> rhs) {
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
unaryNode::unaryNode(std::unique_ptr<primaryNode> rhs) {
  operand = std::move(rhs);
}

primaryNode::primaryNode(std::string identifier) { id_name = identifier; }
primaryNode::primaryNode(std::unique_ptr<ParseTreeNode> expr) {
  non_terminal_ptr = std::move(expr);
}
primaryNode::primaryNode(std::string litVal, Tok::Token typ) {
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
DataTypes primaryNode::getType() { return type; }

fnCallNode::fnCallNode(std::string id, std::unique_ptr<callArgsNode> calls) {
  name = id;
  args = std::move(calls);
}
callArgsNode::callArgsNode(
    std::vector<std::unique_ptr<primaryNode>> primaries) {
  callArgs = std::move(primaries);
}
