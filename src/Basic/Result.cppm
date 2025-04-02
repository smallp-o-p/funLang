//
// Created by will on 10/5/24.
//
export module Basic:Result;
import :Constants;
import std_modules;
import :Casting;

namespace funLang {
export {
  template<class Ty> class ActionRes {
    u_ptr<Ty> Val{};
    bool Invalid = false;

  public:
    ActionRes() : Invalid(true) {}
    explicit ActionRes(u_ptr<Ty> Val)
        : Val(std::move(Val)) {}// valid and present
    template<typename... Ts>
    explicit ActionRes(Ts... Args)// valid and present
        : Val(std::make_unique<Ty>(std::forward<Ts>(Args)...)) {}

    explicit ActionRes(const bool Invalid = false)
        : Val(nullptr), Invalid(Invalid) {}// valid and not present or invalid

    template<typename From>
    explicit ActionRes(ActionRes<From> &&F)
        : Val(F.template getAs<Ty>()), Invalid(F.Invalid) {}

    [[nodiscard]] bool isInvalid() const { return Invalid; }

    [[nodiscard]] bool isUnset() const { return !Invalid && !Val; }

    template<typename T> [[nodiscard]] std::unique_ptr<T> moveAs() {
      return std::unique_ptr<T>(dyn_cast<T>(Val.release()));
    }

    [[nodiscard]] std::unique_ptr<Ty> move() { return std::move(Val); }

    [[nodiscard]] Ty *get() { return Val.get(); }

    template<typename T> [[nodiscard]] T *getAs() {
      return dyn_cast<T>(Val.get());
    }

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
