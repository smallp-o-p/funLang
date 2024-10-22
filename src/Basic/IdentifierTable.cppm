//
// Created by will on 10/5/24.
//
module;
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Allocator.h"
export module Basic:IdentifierTable;

namespace funLang {
  export
  {
    using IDTableEntry = llvm::StringMapEntry<std::nullopt_t>;
    class [[maybe_unused]] IdentifierTable {
      llvm::StringSet<llvm::BumpPtrAllocator> SeenIdentifiers{};

    public:
      IdentifierTable() = default;
      llvm::StringMapEntry<std::nullopt_t> *insert(const llvm::StringRef Key) {
        auto [First, Second] = SeenIdentifiers.insert(Key);
        return &*First;
      }
    };
  }
} // namespace funLang
