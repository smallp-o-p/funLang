#pragma once
#include "ParseTree.hpp"
#include <memory>
#include <string>
#include <unordered_map>

class SymbolProperties {
protected:
  DataTypes type;
  std::string_view name;
};

class FunctionProperties : SymbolProperties {
private:
  std::unordered_map<std::string, SymbolProperties> arguments;

public:
  auto argIter() { return arguments.begin(); }
};

struct SymbolTable {
  std::unordered_map<std::string, SymbolProperties> table;
  std::unique_ptr<std::unordered_map<std::string, SymbolProperties>>
      outer_scope;
  std::unique_ptr<std::unordered_map<std::string, SymbolProperties>>
      inner_scope;
};

struct GlobalSymbolTable {
  std::unordered_map<std::string, SymbolProperties> globs;
};
