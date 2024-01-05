#include "AST.hpp"
#include <llvm-14/llvm/IR/Value.h>

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
  default:
    type = INVALID;
    break;
  }
};

typeNode::~typeNode() {}

funcNode::~funcNode() {}

functionsNode::~functionsNode() {}

programNode::~programNode() {}
