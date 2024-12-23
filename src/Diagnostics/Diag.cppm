//
// Created by will on 10/5/24.
//
module;
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"
#include <utility>
export module Diag;

namespace funLang {
static constexpr const char *diagFmts[] = {
    #define DIAG(ID, Level, Msg) Msg,
    #include "Diags.def"
};

static constexpr llvm::SourceMgr::DiagKind diagKind[] = {
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
  static const char *getDiagText(const unsigned DiagID) {
    return diagFmts[DiagID];
  }
  static llvm::SourceMgr::DiagKind getDiagKind(const unsigned DiagID) {
    return diagKind[DiagID];
  }

  llvm::SourceMgr& srcMgr;
  size_t numErrors;

public:
  explicit DiagEngine(llvm::SourceMgr& srcMgr)
      : srcMgr(srcMgr), numErrors(0) {}

  [[nodiscard]] size_t getNumErrors() const { return numErrors; }

  template<typename... args>
  void emitDiagMsg(const llvm::SMLoc Loc, const unsigned DiagID, args &&...arguments) {
    assert(Loc.isValid() && "SMLoc returned invalid.");
    const std::string Msg =
        llvm::formatv(getDiagText(DiagID), std::forward<args>(arguments)...)
            .str();
    const llvm::SourceMgr::DiagKind Kind = getDiagKind(DiagID);
    srcMgr.PrintMessage(Loc, Kind, Msg);

    numErrors += (Kind == llvm::SourceMgr::DK_Error);
  }

  template<typename... args>
  void emitDiagMsgRange(const llvm::SMLoc Left, const llvm::SMLoc Right, const unsigned DiagID,
                        args &&...arguments) {
    assert(Left.isValid() && Right.isValid() && "SMLoc left or right returned invalid.");
    const std::string Msg =
        llvm::formatv(getDiagText(DiagID), std::forward<args>(arguments)...)
            .str();
    const llvm::SourceMgr::DiagKind Kind = getDiagKind(DiagID);
    srcMgr.PrintMessage(Left, Kind, Msg, {llvm::SMRange(Left, Right)});

    numErrors += Kind == llvm::SourceMgr::DK_Error;
  }
};
}
