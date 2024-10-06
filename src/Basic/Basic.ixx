//
// Created by will on 10/5/24.
//
module;
#include <memory>
export module Basic;

namespace funLang {
static const char *const tokNames[] = {
#define TOK(ID) #ID,
#define KEYWORD(ID, FLAG) #ID,
#include "TokenTags.def"
    nullptr};

static const char *const BuiltInTypes[]{
#define DATA(ID, SP) SP,
#include "BuiltInTypes.def"
    nullptr};

export template<typename T>
using u_ptr = std::unique_ptr<T>;

export namespace Basic::tok {

enum Tag : unsigned short {
#define TOK(ID) ID,
#include "TokenTags.def"
  NUM_TOKENS
};

const char *getTokenName(Tag tag) { return tokNames[tag]; }

const char *getPunctuatorSpelling(Tag tag) {
  switch (tag) {
#define PUNCTUATOR(ID, SP) \
  case ID:                 \
    return SP;
#include "TokenTags.def"
  default: break;
  }
  return nullptr;
}

const char *getKeywordSpelling(Tag tag);
const char *getKwPunctuatorSpelling(Tag tag);
}// namespace Basic::tok

export namespace Basic::Data {
enum Type : unsigned short {
#define DATA(ID, SP) ID,
#include "BuiltInTypes.def"
  NUM_DATA_TYPES
};

const char *getBasicTypeSpelling(Data::Type T) {
  return BuiltInTypes[T];
}

};// namespace Basic::Data

export namespace Basic::Op {

enum Binary : unsigned short {
#define BINARY_OPERATION(ID, SP) ID,
#include "OperationKinds.def"
  NUM_BINARY
};

const char *getBinaryOpSpelling(Basic::Op::Binary binop) {
  switch (binop) {
#define BINARY_OPERATION(ID, SP) \
  case##ID: return #SP;
#include "OperationKinds.def"
  default: break;
  }
  return nullptr;
}

enum Unary : unsigned short {
#define UNARY_OPERATION(ID, SP) ID,
#include "OperationKinds.def"
  NUM_UNARY
};
const char *getUnaryOpSpelling(Basic::Op::Unary unop) {
  switch (unop) {
#define UNARY_OPERATION(ID, SP) \
  case##ID: return #SP;
#include "OperationKinds.def"
  default: break;
  }
  return nullptr;
}

}// namespace Basic::Op

}// namespace funLang
