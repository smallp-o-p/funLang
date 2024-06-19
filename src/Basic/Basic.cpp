//
// Created by will on 5/6/24.
//

#include "Basic/Basic.hpp"
#include <cassert>

using namespace Basic;
static const char *const tokNames[] = {
#define TOK(ID) #ID,
#define KEYWORD(ID, FLAG) #ID,
#include "TokenTags.def"
	nullptr};

static const char *const BuiltInTypes[]{
#define DATA(ID, SP) SP,
#include "Basic/defs/BuiltInTypes.def"
	nullptr
};

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
  return nullptr;
}

const char *Data::getBasicTypeSpelling(Data::Type T) {
  return BuiltInTypes[T];
}
