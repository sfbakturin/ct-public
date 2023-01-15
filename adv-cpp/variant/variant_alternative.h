#pragma once

#include <cstddef>

template <typename, typename...>
struct variant;

template <std::size_t, typename>
struct variant_alternative;

template <typename Head, typename... Tail>
struct variant_alternative<0, variant<Head, Tail...>> {
  using type = Head;
};

template <std::size_t Index, typename Head, typename... Tail>
struct variant_alternative<Index, variant<Head, Tail...>> {
  using type = details::helpers::get_type_t<Index, Head, Tail...>;
};

template <std::size_t Index, typename T>
struct variant_alternative<Index, const T> {
  using type = std::add_const_t<typename variant_alternative<Index, T>::type>;
};

template <std::size_t Index, typename T>
struct variant_alternative<Index, volatile T> {
  using type = std::add_volatile_t<typename variant_alternative<Index, T>::type>;
};

template <std::size_t Index, typename T>
struct variant_alternative<Index, const volatile T> {
  using type = std::add_cv_t<typename variant_alternative<Index, T>::type>;
};

template <size_t Index, typename T>
using variant_alternative_t = typename variant_alternative<Index, T>::type;
