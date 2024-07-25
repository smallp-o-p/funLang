//
// Created by will on 7/22/24.
// This class will hold all lexed identifiers, to avoid multiple instances of the same string.
// Decls will hold pointers to the entries in this table.
//

#ifndef FUNLANG_INCLUDE_BASIC_IDENTIFIERTABLE_HPP
#define FUNLANG_INCLUDE_BASIC_IDENTIFIERTABLE_HPP
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Allocator.h"

namespace funLang {
class Lexer;
class IdentifierTable;

class IdentifierTable {
private:
  llvm::StringSet<llvm::BumpPtrAllocator> SeenIdentifiers;
  IdentifierTable() : SeenIdentifiers(llvm::StringSet<llvm::BumpPtrAllocator>()) {}
public:
  friend class Lexer;
  llvm::StringMapEntry<std::nullopt_t> *insert(llvm::StringRef Key) {
	auto Inserted = SeenIdentifiers.insert(Key);
	return &*Inserted.first;
  }
};

}

#endif //FUNLANG_INCLUDE_BASIC_IDENTIFIERTABLE_HPP
