#pragma once
#include "Lex.hpp"
#include "llvm/IR/BasicBlock.h"
#include <llvm-14/llvm/IR/Constant.h>
#include <llvm-14/llvm/IR/Value.h>
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
// base node class and leaf classes
extern llvm::Module globModule;
class ASTNode {

public:
  virtual llvm::Value *codegen() = 0;

  virtual ~ASTNode(){};
};

class simpleStmtNode : public ASTNode {

public:
  ~simpleStmtNode() = default;
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

class identifierNode : public ASTNode {

private:
  std::string name;

  llvm::Value *codegen();
};

class typeNode : public ASTNode {
private:
  DataTypes type;

public:
  typeNode(Tok::Token tok);
  ~typeNode();
  bool invalid() { return type == INVALID; }

  llvm::Value *codegen();
};

// nodes for functions

// ------------ args --------------- //

/*
 * arg --> type identifier
 *
 * */

class ArgNode : public ASTNode {
private:
  std::unique_ptr<typeNode> type;
  std::unique_ptr<identifierNode> id;

public:
  ArgNode(std::unique_ptr<typeNode> nodeType,
          std::unique_ptr<identifierNode> identifier) {
    type = std::move(nodeType);
    id = std::move(identifier);
  }
  ~ArgNode() = default;

  llvm::Value *codegen();
};

/*
 * args --> arg ',' args
 *       |  arg
 * */

class ArgsNode : public ASTNode {

public:
  ~ArgsNode() = default;
  llvm::Value *codegen();
};
// ------------- end args ----------- //

// proto --> identifier '(' args ')'
class protoNode : public ASTNode {
private:
  std::unique_ptr<ArgsNode> argList;
  std::unique_ptr<identifierNode> name;

public:
  protoNode(std::unique_ptr<identifierNode> id,
            std::unique_ptr<ArgsNode> args) {
    argList = std::move(args);
    name = std::move(id);
  }
  ~protoNode() = default;

  llvm::Value *codegen();
};

// ---------- func/functions -------- //

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
