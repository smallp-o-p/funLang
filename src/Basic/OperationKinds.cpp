#include "OperationKinds.hpp"

const char *getBinaryOpSpelling(Basic::BinaryOperations binop) {
  switch (binop) {
#define BINARY_OPERATION(ID, SP)                                               \
  case ID:                                                                     \
    return SP;
  default:
    break;
  }
  return nullptr;
}
