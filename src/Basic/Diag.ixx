//
// Created by will on 10/5/24.
//
module;
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"
#include <utility>
export module Basic.Diag;

static const char *diagFmts[] = {
    #define DIAG(ID, Level, Msg) Msg,
    #include "Diags.def"
};

llvm::SourceMgr::DiagKind diagKind[] = {
    #define DIAG(ID, Level, Msg) llvm::SourceMgr::DK_##Level,
    #include "Diags.def"
};

export namespace Diag {
  using namespace llvm;
  enum {
    #define DIAG(ID, Level, Msg) ID,
    #include "Diags.def"
  };
}// namespace Diag

export class DiagEngine {
  static const char *getDiagText(unsigned int diagID) {
    return diagFmts[diagID];
  }
  static llvm::SourceMgr::DiagKind getDiagKind(unsigned int diagID) {
    return diagKind[diagID];
  }

  std::shared_ptr<llvm::SourceMgr> srcMgr;
  uint32_t numErrors;

public:
  explicit DiagEngine(std::shared_ptr<llvm::SourceMgr> srcMgr)
      : srcMgr(std::move(srcMgr)), numErrors(0) {}

  uint32_t getNumErrors() const { return numErrors; }

  template<typename... args>
  void emitDiagMsg(llvm::SMLoc loc, uint32_t diagID, args &&...arguments) {
    assert(loc.isValid() && "SMLoc returned invalid.");
    std::string msg =
        llvm::formatv(getDiagText(diagID), std::forward<args>(arguments)...)
            .str();
    llvm::SourceMgr::DiagKind kind = getDiagKind(diagID);
    srcMgr->PrintMessage(loc, kind, msg);

    numErrors += (kind == llvm::SourceMgr::DK_Error);
  }

  template<typename... args>
  void emitDiagMsgRange(llvm::SMLoc left, llvm::SMLoc right, uint32_t diagID,
                        args &&...arguments) {
    assert(left.isValid() && right.isValid() && "SMLoc left or right returned invalid.");
    std::string msg =
        llvm::formatv(getDiagText(diagID), std::forward<args>(arguments)...)
            .str();
    llvm::SourceMgr::DiagKind kind = getDiagKind(diagID);
    srcMgr->PrintMessage(left, kind, msg, {llvm::SMRange(left, right)});

    numErrors += (kind == llvm::SourceMgr::DK_Error);
  }
};
