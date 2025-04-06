//
// Created by will on 10/27/24.
//
module AST;
import Basic;

namespace funLang {
ifStmt::ifStmt(const llvm::SMLoc IfLoc, u_ptr<Expr> cond,
               u_ptr<CompoundStmt> block1, u_ptr<elifStmt> elif,
               u_ptr<CompoundStmt> Else)

    : Stmt(SK_IF, IfLoc), Condition(std::move(cond)), Block(std::move(block1)),
      elif (std::move(elif)), ElseBlock(std::move(Else)) {}

elifStmt::elifStmt(const llvm::SMLoc Loc, u_ptr<Stmt> cond,
                   u_ptr<CompoundStmt> block, u_ptr<elifStmt> elif)
    : Stmt(SK_ELIF, Loc), Cond(std::move(cond)), NextElif(std::move(elif)),
      Block(std::move(block)) {}
elifStmt::~elifStmt() = default;

DeclStmt::DeclStmt(VarDecl *NamedDecl, std::unique_ptr<Stmt> Expression)
    : Stmt(SK_DECL, NamedDecl->getStart(),
           Expression ? Expression->getEndLoc() : NamedDecl->getEnd()),
      NamedDecl(NamedDecl), Init(std::move(Expression)) {}

llvm::StringRef DeclStmt::getName() const { return NamedDecl->getName(); }

ReturnStmt::ReturnStmt(u_ptr<Expr> exprNode, llvm::SMLoc RetStart,
                       llvm::SMLoc RetEnd)
    : Stmt(SK_RETURN, RetStart, RetEnd), ReturnExpr(std::move(exprNode)) {}

ReturnStmt::ReturnStmtPtr ReturnStmt::Create(u_ptr<Expr> E, SourceLoc RetStart,
                                             SourceLoc End) {
  return ReturnStmtPtr(new ReturnStmt(std::move(E), RetStart, End));
}
}// namespace funLang
