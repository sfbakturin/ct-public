#pragma once

#include "helpers.h"

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

namespace details::function {
using namespace details::helpers;

template <typename T, typename Array>
concept valid = requires(T && t) {
  new Array[1]{std::forward<T>(t)};
};

template <typename...>
struct function_builder;

template <typename Find, typename Head, typename... Tail>
struct function_builder<Find, Head, Tail...> : function_builder<Find, Tail...> {
  using function_builder<Find, Tail...>::F;

  static constexpr Head F(Head) requires(valid<Find, Head>);
};

template <typename Find, typename Head>
struct function_builder<Find, Head> {
  static constexpr Head F(Head) requires(valid<Find, Head>);
};

template <typename T, typename... Types>
using found_type_t = decltype(function_builder<T, Types...>::F(std::declval<T>()));

template <typename T, typename... Types>
constexpr inline std::size_t found_index_v = get_index_v<found_type_t<T, Types...>, Types...>;
} // namespace details::function
