#include "Basic/Diag.hpp"

static const char *diagFmts[] = {
	#define DIAG(ID, Level, Msg) Msg,
	#include "Basic/defs/Diags.def"
};

llvm::SourceMgr::DiagKind diagKind[] = {
	#define DIAG(ID, Level, Msg) llvm::SourceMgr::DK_##Level,
	#include "Basic/defs/Diags.def"
};
const char *DiagEngine::getDiagText(unsigned int diagID) {
  return diagFmts[diagID];
}

llvm::SourceMgr::DiagKind DiagEngine::getDiagKind(unsigned int diagID) {
  return diagKind[diagID];
}

