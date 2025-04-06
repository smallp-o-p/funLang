//
// Created by will on 10/27/24.
//
import Basic;
module AST;
namespace funLang {
bool Expr::isError() const { return isa<ErrorExpr>(this); }
}// namespace funLang
