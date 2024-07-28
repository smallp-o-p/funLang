//
// Created by will on 7/24/24.
//

#include "AST/Decl.hpp"
#include "AST/Stmt.hpp"
using namespace funLang;

funLang::Type *funLang::FunctionDecl::getTypePtr() { return ReturnType->getTypePtr(); }

FunctionDecl::FunctionDecl(std::unique_ptr<TypeUse> retType,
						   llvm::StringMapEntry<std::nullopt_t> *Name,
						   std::unique_ptr<CompoundStmt> Stmts,
						   llvm::SMLoc TypeLoc,
						   llvm::SMLoc RLoc,
						   std::unique_ptr<llvm::SmallVector<std::unique_ptr<ParamDecl>>> AllParams,
						   DeclContext *Parent)
	: ReturnType(std::move(retType)),
	  Stmts(std::move(Stmts)), Params(std::move(AllParams)), Decl(DK_FN, Name, TypeLoc, RLoc),
	  DeclContext(Parent) {}

FunctionDecl::FunctionDecl(std::unique_ptr<TypeUse> retType,
						   IDTableEntry *Name,
						   llvm::SMLoc TypeLoc,
						   llvm::SMLoc RLoc,
						   std::unique_ptr<llvm::SmallVector<std::unique_ptr<ParamDecl>>> Parms,
						   DeclContext *Ctx)
	: Stmts(nullptr), ReturnType(std::move(retType)), Params(std::move(Parms)), Decl(DK_FN, Name, TypeLoc, RLoc),
	  DeclContext(Ctx) {}

ParamDecl::ParamDecl(IDTableEntry *Name,
					 std::unique_ptr<Expr> Init,
					 DeclContext *Container,
					 llvm::SMLoc Left,
					 llvm::SMLoc Right,
					 u_ptr<TypeUse> T)
	: Decl(DK_PARAM,
		   Name,
		   Left,
		   Right),
	  Init(std::move(Init)), Container(Container), T(std::move(T)) {}
