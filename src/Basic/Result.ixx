//
// Created by will on 10/5/24.
//
module;
#include <memory>

export module Basic.Result;

namespace funLang {
export {
  template<class Ty>
  class ActionRes {
    std::unique_ptr<Ty> Val;
    bool Invalid = false;

  public:
    explicit ActionRes(std::unique_ptr<Ty> Val) : Val(std::move(Val)), Invalid(false) {}
    explicit ActionRes(bool Invalid = false) : Val(nullptr), Invalid(Invalid) {}
    ActionRes() : Val(nullptr), Invalid(true) {}

    [[nodiscard]]
    bool isInvalid() const {
      return Invalid;
    }

    [[nodiscard]]
    bool isUnset() const {
      return !Invalid && !Val;
    }

    [[nodiscard]]
    bool isUsable() const {
      return !isInvalid() && !isUnset();
    }

    std::unique_ptr<Ty> move() {
      return std::move(Val);
    }

    bool operator!() {
      return isInvalid();
    }
  };
}
}// namespace funLang
