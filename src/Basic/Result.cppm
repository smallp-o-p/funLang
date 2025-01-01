//
// Created by will on 10/5/24.
//
module;
#include <memory>
#include <utility>
export module Basic:Result;
import :Constants;

namespace funLang {
export {
  template<class Ty> class ActionRes {
    u_ptr<Ty> Val;
    bool Invalid = false;

  public:
    explicit ActionRes(u_ptr<Ty> Val)
        : Val(std::move(Val)) {}// valid and present
    explicit ActionRes(const bool Invalid = false)
        : Val(nullptr), Invalid(Invalid) {}// valid and not present or invalid

    [[nodiscard]] bool isInvalid() const { return Invalid; }

    [[nodiscard]] bool isUnset() const { return !Invalid && !Val; }

    [[nodiscard]] std::unique_ptr<Ty> move() { return std::move(Val); }

    [[nodiscard]] static inline ActionRes InvalidRes() {
      return ActionRes(true);
    }

    [[nodiscard]] static inline ActionRes ValidEmptyRes() {
      return ActionRes();
    }

    bool operator!() const { return isInvalid(); }
  };
}
}// namespace funLang
