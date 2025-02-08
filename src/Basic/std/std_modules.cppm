//
// Created by will on 2/27/25.
//
module;
#include <cctype>
#include <concepts>
#include <cstddef>
#include <deque>
#include <iostream>
#include <memory>
#include <optional>
#include <type_traits>
#include <utility>
#include <vector>
export module std_modules;

export namespace std {
using std::cout;
using std::deque;
using std::is_base_of_v;
using std::isalnum;
using std::isalpha;
using std::isdigit;
using std::iswspace;
using std::make_pair;
using std::move;
using std::nullopt_t;
using std::ostream;
using std::same_as;
using std::size_t;
using std::string;
using std::string_view;
using std::unique_ptr;
using std::vector;
}// namespace std
