//
// Created by will on 2/21/25.
//
export module Basic:Concepts;
import std_modules;
namespace funLang {
export {
  template<typename To, typename From>
  concept llvm_dyn_castable =
      std::is_base_of_v<From, To> && requires(To, From *f) {
        { To::classof(f) } -> std::same_as<bool>;
      };
}
}// namespace funLang
