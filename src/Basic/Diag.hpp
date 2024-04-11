#include <utility>

#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"

namespace diag {
using namespace llvm;
enum {
  #define DIAG(ID, Level, Msg) ID,
  #include "Diags.def"
};

}

class DiagEngine {
  static const char *getDiagText(unsigned diagID);
  static llvm::SourceMgr::DiagKind getDiagKind(unsigned diagID);

  std::shared_ptr<llvm::SourceMgr> srcMgr;
  uint32_t numErrors;

public:
  explicit DiagEngine(std::shared_ptr<llvm::SourceMgr> srcMgr)
	  : srcMgr(std::move(srcMgr)), numErrors(0) {}

  uint32_t getNumErrors() const { return numErrors; }

  template<typename... args>
  void reportErr(llvm::SMLoc loc, uint32_t diagID, args &&...arguments);
  template<typename... args>
  void addNote(llvm::SMLoc loc, uint32_t diagID, args &&...arguments);
};
