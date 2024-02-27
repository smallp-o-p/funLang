#pragma once
#include <string>
namespace tok {
enum Tag : unsigned short {
#define TOK(ID) ID,
#include "TokenTags.def"
  NUM_TOKENS
};

const std::string getTokenName(Tag tag);
const std::string getPunctuatorSpelling(Tag tag);
const std::string getKeywordSpelling(Tag tag);
} // namespace tok
