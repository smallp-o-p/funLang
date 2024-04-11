#include "Diag.hpp"
static const char *diagFmts[] = {
	#define DIAG(ID, Level, Msg) Msg,
	#include "Diags.def"
};

llvm::SourceMgr::DiagKind diagKind[] = {
	#define DIAG(ID, Level, Msg) llvm::SourceMgr::DK_##Level,
	#include "Diags.def"
};
const char *DiagEngine::getDiagText(unsigned int diagID) {
  return diagFmts[diagID];
}

llvm::SourceMgr::DiagKind DiagEngine::getDiagKind(unsigned int diagID) {
  return diagKind[diagID];
}

template<typename... args>
void DiagEngine::reportErr(llvm::SMLoc loc, uint32_t diagID, args &&... arguments) {
  assert(loc.isValid() && "SMLoc returned invalid.");
  std::string msg =
	  llvm::formatv(getDiagText(diagID), std::forward<args>(arguments)...)
		  .str();
  llvm::SourceMgr::DiagKind kind = getDiagKind(diagID);
  srcMgr->PrintMessage(loc, kind, msg);
  numErrors += (kind==llvm::SourceMgr::DK_Error);
}

template<typename... args>
void DiagEngine::addNote(llvm::SMLoc loc, uint32_t diagID, args &&... arguments) {
  assert(loc.isValid() && "SMLoc returned invalid.");
  std::string msg =
	  llvm::formatv(getDiagText(diagID), std::forward<args>(arguments)...)
		  .str();
  llvm::SourceMgr::DiagKind kind = getDiagKind(diagID);
  srcMgr->PrintMessage(loc, kind, msg);
}
