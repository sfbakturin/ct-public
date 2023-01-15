#pragma once

#include <cstddef>

template <typename>
struct in_place_type_t {
  explicit in_place_type_t() = default;
};

template <std::size_t>
struct in_place_index_t {
  explicit in_place_index_t() = default;
};

template <typename T>
inline constexpr in_place_type_t<T> in_place_type{};

template <std::size_t Index>
inline constexpr in_place_index_t<Index> in_place_index{};
