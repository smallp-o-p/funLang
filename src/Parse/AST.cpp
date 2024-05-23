#include "AST.hpp"
#include "Lex.hpp"
#include "Basic.hpp"
#include "Sema.hpp"
auto &CompilationUnit::getGlobs() { return globalSymbols; }

std::unordered_map<std::string, std::unique_ptr<Decl>> &TopLevelDecls::getTopLevelMap() {
  return fnMap;
}

Expr *VarDeclStmt::getExpr() { return expr.get(); }

std::vector<std::unique_ptr<Stmt>> &CompoundStmt::getStmts() { return stmts; }

void Expr::accept(funLang::SemaAnalyzer &v) { ; }
void Expr::setType(TypeDecl *toSet) {
  resultType = toSet;
}