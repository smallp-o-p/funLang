#pragma once
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
  void emitDiagMsg(llvm::SMLoc loc, uint32_t diagID, args &&...arguments) {
	assert(loc.isValid() && "SMLoc returned invalid.");
	std::string msg =
		llvm::formatv(getDiagText(diagID), std::forward<args>(arguments)...)
			.str();
	llvm::SourceMgr::DiagKind kind = getDiagKind(diagID);
	srcMgr->PrintMessage(loc, kind, msg);

	numErrors += (kind==llvm::SourceMgr::DK_Error);
  }

  template<typename... args>
  void emitDiagMsgRange(llvm::SMLoc left, llvm::SMLoc right, uint32_t diagID, args &&...arguments) {
	assert(left.isValid() && right.isValid() && "SMLoc left or right returned invalid.");
	std::string msg = llvm::formatv(getDiagText(diagID), std::forward<args>(arguments)...)
		.str();
	llvm::SourceMgr::DiagKind kind = getDiagKind(diagID);
	srcMgr->PrintMessage(left, kind, msg, {llvm::SMRange(left, right)});

	numErrors += (kind==llvm::SourceMgr::DK_Error);
  }
};
