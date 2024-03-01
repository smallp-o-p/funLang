#pragma once

namespace Basic {

namespace tok {
enum Tag : unsigned short {
#define TOK(ID) ID,
#include "TokenTags.def"
  NUM_TOKENS
};

const char *getTokenName(Tag tag);
const char *getPunctuatorSpelling(Tag tag);
const char *getKeywordSpelling(Tag tag);

} // namespace tok

} // namespace Basic
