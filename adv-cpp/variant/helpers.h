#pragma once

#include <cstddef>
#include <type_traits>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

namespace details::helpers {
template <std::size_t, typename...>
struct get_type;

template <typename Head, typename... Tail>
struct get_type<0, Head, Tail...> {
  using type = Head;
};

template <std::size_t Index, typename Head, typename... Tail>
struct get_type<Index, Head, Tail...> {
  using type = typename get_type<Index - 1, Tail...>::type;
};

template <std::size_t Index, typename... Types>
using get_type_t = typename get_type<Index, Types...>::type;

template <std::size_t Index, typename Head, typename... Tail>
struct get_index {
  static const std::size_t value = Index;
};

template <std::size_t Index, typename Head0, typename Head1, typename... Tail>
struct get_index<Index, Head0, Head1, Tail...> {
  static const std::size_t value = std::is_same_v<Head0, Head1> ? Index : get_index<Index + 1, Head0, Tail...>::value;
};

template <typename T, typename... Types>
constexpr inline std::size_t get_index_v = get_index<0, T, Types...>::value;
} // namespace details::helpers
