//
// Created by will on 5/6/24.
//

#include "Basic.hpp"
#include <cassert>

using namespace Basic;
static const char *const tokNames[] = {
#define TOK(ID) #ID,
#define KEYWORD(ID, FLAG) #ID,
#include "TokenTags.def"
	nullptr};

const char *tok::getTokenName(Tag tag) { return tokNames[tag]; }
const char *tok::getPunctuatorSpelling(Tag tag) {
  switch (tag) {
#define PUNCTUATOR(ID, SP)                                                     \
  case ID:                                                                     \
    return SP;
#include "TokenTags.def"
  default:break;
  }
  return nullptr;
}
const char *tok::getKeywordSpelling(Tag tag) {
  switch (tag) {
#define KEYWORD(ID, FLAG)                                                      \
  case ##ID:  return #ID;
	#include "TokenTags.def"
  default:break;
  }
  return nullptr;
}

const char *Op::getBinaryOpSpelling(Basic::Op::Binary binop) {
  switch (binop) {
#define BINARY_OPERATION(ID, SP)                                               \
  case ##ID: return #SP;
	#include "OperationKinds.def"
  default:break;
  }
  return nullptr;
}

const char *Op::getUnaryOpSpelling(Basic::Op::Unary unop) {
  switch (unop) {
	#define UNARY_OPERATION(ID, SP) \
    case ##ID: return #SP;
	#include "OperationKinds.def"
  default: break;
  }
}

const char *Data::getBasicTypeSpelling(Data::Type t) {
  assert(t > Type::ident);
  switch (t) {
	#define DATA(ID, SP) \
    case ##ID: return #SP;
	#include "BasicDataTypes.def"
  default:break;
  }
  return nullptr;
}
