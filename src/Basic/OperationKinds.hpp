#include "TokenTags.hpp"

namespace Basic {
enum BinaryOperations : unsigned short {
#define BINARY_OPERATION(ID, SP) ID,
#include "OperationKinds.def"
  NUM_BINARY
};

enum UnaryOperations : unsigned short {
#define UNARY_OPERATION(ID, SP) ID,
#include "OperationKinds.def"
  NUM_UNARY
};

BinaryOperations Tag2Operator(Basic::tok::Tag tag);

const char *getBinaryOpSpelling();
const char *getUnaryOpSpelling();
} // namespace Basic
