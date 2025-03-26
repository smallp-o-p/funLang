//
// Created by will on 2/21/25.
//
export module Basic:Casting;
import llvm;
import :Concepts;

export namespace funLang {
template<typename To, typename From>
  requires llvm_dyn_castable<To, From>
To *dyn_cast(From &f) {
  return llvm::dyn_cast<To>(f);
}

template<typename To, typename From>
  requires llvm_dyn_castable<To, From>
bool isa(From *F) {
  return llvm::isa<To>(F);
}

template<typename To, typename From>
  requires llvm_dyn_castable<To, From>
To *cast(From *F) {
  return llvm::cast<To>(F);
}

template<typename To, typename From>
  requires llvm_dyn_castable<To, From>
To *cast(From &F) {
  return llvm::cast<To>(F);
}
}// namespace funLang
