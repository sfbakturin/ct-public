#pragma once

#include <cstddef>

template <typename, typename...>
struct variant;

template <typename>
struct variant_size;

template <typename... Types>
struct variant_size<variant<Types...>> : std::integral_constant<std::size_t, sizeof...(Types)> {};

template <typename T>
struct variant_size<const T> : variant_size<T> {};

template <typename T>
struct variant_size<volatile T> : variant_size<T> {};

template <typename T>
struct variant_size<const volatile T> : variant_size<T> {};

template <typename T>
inline constexpr std::size_t variant_size_v = variant_size<T>::value;
