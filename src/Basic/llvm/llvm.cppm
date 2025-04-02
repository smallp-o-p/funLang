//
// Created by will on 2/26/25.
//
module;
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringMapEntry.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/StringSet.h>
#include <llvm/Support/Allocator.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/FormatVariadic.h>
#include <llvm/Support/SMLoc.h>
#include <llvm/Support/SourceMgr.h>
export module llvm;

export namespace llvm {
using llvm::APFloat;
using llvm::APInt;
using llvm::BumpPtrAllocator;
using llvm::cast;
using llvm::dyn_cast;
using llvm::ErrorOr;
using llvm::formatv;
using llvm::isa;
using llvm::MemoryBuffer;
using llvm::SmallVector;
using llvm::SMLoc;
using llvm::SMRange;
using llvm::SourceMgr;
using llvm::StringMap;
using llvm::StringMapEntry;
using llvm::StringRef;
using llvm::StringSet;
using llvm::zip;
}// namespace llvm
