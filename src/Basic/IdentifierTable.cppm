//
// Created by will on 10/5/24.
//
export module Basic:IdentifierTable;
import llvm;
import std_modules;

namespace funLang {
export {
  using IDTableEntry = llvm::StringMapEntry<std::nullopt_t>;
  class IdentifierTable {
    llvm::StringSet<llvm::BumpPtrAllocator> SeenIdentifiers{};

  public:
    IdentifierTable() = default;
    decltype(auto) insert(const llvm::StringRef Key) {
      auto [First, Second] = SeenIdentifiers.insert(Key);
      return &*First;
    }
  };
}
}// namespace funLang
