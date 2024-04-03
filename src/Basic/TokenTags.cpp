#include "TokenTags.hpp"
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
  case kw_##ID:                                                                \
    return #ID;
#include "TokenTags.def"
  default:break;
  }
  return nullptr;
}
