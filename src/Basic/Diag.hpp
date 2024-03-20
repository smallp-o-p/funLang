#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"

class Diag {
  enum Problem {
#define DIAG(ID, Level, Msg) ID,
#include "Diags.def"
  };
};

class DiagEngine {
  static const std::string getDiagText(unsigned diagID);
  static llvm::SourceMgr::DiagKind getDiagKind(unsigned diagID);

  std::shared_ptr<llvm::SourceMgr> srcMgr;
  uint32_t numErrors;

public:
  DiagEngine(std::shared_ptr<llvm::SourceMgr> srcMgr)
      : srcMgr(srcMgr), numErrors(0) {}

  uint32_t getNumErrors() { return numErrors; }

  template <typename... args>
  void reportErr(llvm::SMLoc loc, uint32_t diagID, args &&...arguments) {
    std::string msg =
        llvm::formatv(getDiagText(diagID), std::forward<args>(arguments)...)
            .str();
    llvm::SourceMgr::DiagKind kind = getDiagKind(diagID);
    srcMgr->PrintMessage(loc, kind, msg);
    numErrors += (kind == llvm::SourceMgr::DK_Error);
  }
};
