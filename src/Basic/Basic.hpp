//
// Created by will on 5/6/24.
//

#ifndef FUNLANG_SRC_BASIC_BASIC_HPP
#define FUNLANG_SRC_BASIC_BASIC_HPP
#pragma once

namespace Basic::tok {
enum Tag : unsigned short {
#define TOK(ID) ID,
#include "TokenTags.def"
  NUM_TOKENS
};

const char *getTokenName(Tag tag);
const char *getPunctuatorSpelling(Tag tag);
const char *getKeywordSpelling(Tag tag);
const char *getKwPunctuatorSpelling(Tag tag);
} // namespace Basic::tok

namespace Basic::Data {
enum Type : unsigned short {
  #define DATA(ID, SP) ID,
  #include "BasicDataTypes.def"
  NUM_DATA_TYPES
};
const char *getBasicTypeSpelling(Type t);

};

namespace Basic::Op {

enum Binary : unsigned short {
#define BINARY_OPERATION(ID, SP) ID,
#include "OperationKinds.def"
  NUM_BINARY
};
const char *getBinaryOpSpelling(Binary binop);

enum Unary : unsigned short {
#define UNARY_OPERATION(ID, SP) ID,
#include "OperationKinds.def"
  NUM_UNARY
};
const char *getUnaryOpSpelling(Unary unop);

} // namespace Basic


#endif //FUNLANG_SRC_BASIC_BASIC_HPP
