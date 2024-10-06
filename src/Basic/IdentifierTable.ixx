//
// Created by will on 10/5/24.
//
module;

#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Allocator.h"

export module Basic.IdentifierTable;

namespace funLang {
export {
  typedef llvm::StringMapEntry<std::nullopt_t> IDTableEntry;
  class IdentifierTable;

  class IdentifierTable {
  private:
    llvm::StringSet<llvm::BumpPtrAllocator> SeenIdentifiers;

  public:
    IdentifierTable() : SeenIdentifiers(llvm::StringSet<llvm::BumpPtrAllocator>()) {}
    llvm::StringMapEntry<std::nullopt_t> *insert(llvm::StringRef Key) {
      auto Inserted = SeenIdentifiers.insert(Key);
      return &*Inserted.first;
    }
  };
}

}// namespace funLang