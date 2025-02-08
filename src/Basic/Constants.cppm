//
// Created by will on 12/23/24.
//
export module Basic:Constants;
import std_modules;

namespace funLang {
export template<typename T> using u_ptr = std::unique_ptr<T>;

static constexpr char const *BuiltInTypes[] = {
#define DATA(ID, SP) SP,
#include "BuiltInTypes.def"

    nullptr};

static constexpr char const *tokNames[] = {
#define TOK(ID) #ID,
#define KEYWORD(ID, FLAG) #ID,
#include "TokenTags.def"

    nullptr};

export namespace Basic::tok {
enum Tag : unsigned short {
#define TOK(ID) ID,
#include "TokenTags.def"

  NUM_TOKENS
};

const char *getTokenName(const Tag tag) { return tokNames[tag]; }

const char *getPunctuatorSpelling(const Tag tag) {
  switch (tag) {
#define PUNCTUATOR(ID, SP)                                                     \
  case ID: return SP;
#include "TokenTags.def"

  default: break;
  }
  return nullptr;
}
}// namespace Basic::tok

export namespace Basic::Data {
enum Type : unsigned short {
#define DATA(ID, SP) ID,
#include "BuiltInTypes.def"

  NUM_DATA_TYPES
};

const char *getBasicTypeSpelling(const Type T) { return BuiltInTypes[T]; }
};// namespace Basic::Data

export namespace Basic::Op {
enum Binary : unsigned short {
#define BINARY_OPERATION(ID, SP) ID,
#include "OperationKinds.def"

  NUM_BINARY
};

[[maybe_unused]] const char *getBinaryOpSpelling(const Binary BinOp) {
  switch (BinOp) {
#define BINARY_OPERATION(ID, SP)                                               \
  case ID: return SP;
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
[[maybe_unused]] const char *getUnaryOpSpelling(const Unary UnaryOp) {
  switch (UnaryOp) {
#define UNARY_OPERATION(ID, SP)                                                \
  case ID: return SP;
#include "OperationKinds.def"

  default: break;
  }
  return nullptr;
}
}// namespace Basic::Op
}// namespace funLang
