#include "AST.hpp"
#include "Lex.hpp"
#include "Basic.hpp"
#include "Sema.hpp"
std::unordered_map<std::string,
				   std::shared_ptr<FunctionNode>> &CompilationUnit::getFuncs() { return funcs->getFnMap(); }
auto &CompilationUnit::getGlobs() { return globalSymbols; }

std::unordered_map<std::string, std::shared_ptr<FunctionNode>> &TopLevelDecls::getFnMap() {
  return fnMap;
}

Expr &VarDeclStmt::getExpr() { return *expr; }

std::vector<std::unique_ptr<Stmt>> &CompoundStmt::getStmts() { return stmts; }

void Expr::accept(funLang::SemaAnalyzer &v) { ; }
void Expr::setType(TypeDecl *toSet) {
  resultType = toSet;
}