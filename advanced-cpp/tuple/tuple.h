#pragma once

#include <memory>
#include <type_traits>
#include <utility>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

template <typename...>
struct tuple;

// tuple_size -- value equals number of elements (you can use
// std::integral_constant)
template <typename>
struct tuple_size;

template <typename... Types>
struct tuple_size<tuple<Types...>>
    : std::integral_constant<std::size_t, sizeof...(Types)> {};

template <typename T>
struct tuple_size<const T> : tuple_size<T> {};

template <typename T>
struct tuple_size<volatile T> : tuple_size<T> {};

template <typename T>
struct tuple_size<const volatile T> : tuple_size<T> {};

template <typename T>
inline constexpr std::size_t tuple_size_v = tuple_size<T>::value;

// NOTE: if not tuple passed to helpers -- must be compiler error
// tuple_element -- return type by it's number (type field)
template <std::size_t N, typename T>
struct tuple_element;

template <std::size_t N, typename Head, typename... Tail>
struct tuple_element<N, tuple<Head, Tail...>>
    : tuple_element<N - 1, tuple<Tail...>> {};

template <typename Head, typename... Tail>
struct tuple_element<0, tuple<Head, Tail...>> {
  using type = Head;
};

template <std::size_t N, typename Tuple>
using tuple_element_t = typename tuple_element<N, Tuple>::type;

template <typename T, typename... Types>
constexpr T& get(tuple<Types...>& t) noexcept;

template <typename T, typename... Types>
constexpr T&& get(tuple<Types...>&& t) noexcept;

template <typename T, typename... Types>
constexpr const T& get(const tuple<Types...>& t) noexcept;

template <typename T, typename... Types>
constexpr const T&& get(const tuple<Types...>&& t) noexcept;

namespace details {
template <typename T>
struct is_tuple {
  static constexpr bool value = false;
};

template <typename... Types>
struct is_tuple<tuple<Types...>> {
  static constexpr bool value = true;
};

template <typename...>
struct calc {
  static constexpr std::size_t value = 0;
};

template <typename Find, typename Head, typename... Tail>
struct calc<Find, Head, Tail...> {
  static constexpr std::size_t value =
      (std::is_same_v<Find, Head> ? 1 : 0) + calc<Find, Tail...>::value;
};

template <typename Find, typename... Types>
inline constexpr bool is_only_once = calc<Find, Types...>::value == 1;

template <typename... T>
concept IsNotATuple = !is_tuple<std::remove_cvref_t<T>...>::value;

template <typename...>
struct storage {};

template <typename... Types>
using storage_t = storage<Types...>;

template <typename Head, typename... Tail>
struct storage<Head, Tail...> {
  Head head{};
  storage_t<Tail...> tail{};

  constexpr storage() = default;

  template <typename H, typename... T>
  constexpr storage(storage_t<H, T...>&& other)
      : head(std::move(other.head)), tail(std::move(other.tail)) {}

  template <typename H, typename... T>
  constexpr storage(const storage_t<H, T...>& other)
      : head(other.head), tail(other.tail) {}

  template <typename H, typename... T>
  constexpr storage(H&& h, T&&... t)
      requires(IsNotATuple<H> && (IsNotATuple<T> && ...))
      : head(std::forward<H>(h)), tail(std::forward<T>(t)...) {}
};

template <std::size_t Index, typename Storage>
constexpr decltype(auto) storage_get(Storage&& storage) noexcept {
  if constexpr (Index == 0) {
    return (std::forward<Storage>(storage).head);
  } else {
    return (storage_get<Index - 1>(std::forward<Storage>(storage).tail));
  }
}

template <std::size_t Index, std::size_t Recursive, typename... TTypes,
          typename... UTypes>
constexpr bool storage_compare(bool equals, const tuple<TTypes...>& first,
                               const tuple<UTypes...>& second) noexcept {
  if constexpr (Index < Recursive) {
    if (equals) {
      return (get<Index>(first) == get<Index>(second)) &&
             storage_compare<Index + 1, Recursive>(equals, first, second);
    } else {
      return (get<Index>(first) < get<Index>(second)) ||
             storage_compare<Index + 1, Recursive>(equals, first, second);
    }
  } else {
    return true;
  }
}

template <typename Head, typename... Tail>
constexpr void storage_swap(storage_t<Head, Tail...>& to,
                            storage_t<Head, Tail...>& from) {
  using std::swap;
  swap(to.head, from.head);
  if constexpr (sizeof...(Tail) > 0) {
    storage_swap(to.tail, from.tail);
  }
}

template <std::size_t Index, typename Head, typename... Tail>
struct get_index {
  static const std::size_t value = Index;
};

template <std::size_t Index, typename Head0, typename Head1, typename... Tail>
struct get_index<Index, Head0, Head1, Tail...> {
  static const std::size_t value =
      std::is_same_v<Head0, Head1>
          ? Index
          : get_index<Index + 1, Head0, Tail...>::value;
};

template <typename T, typename... Types>
constexpr inline std::size_t get_index_v = get_index<0, T, Types...>::value;
} // namespace details

template <std::size_t Index, typename... Types>
constexpr tuple_element_t<Index, tuple<Types...>>&
get(tuple<Types...>& t) noexcept requires(Index < sizeof...(Types)) {
  return details::storage_get<Index>(t);
}

template <std::size_t Index, typename... Types>
constexpr tuple_element_t<Index, tuple<Types...>>&&
get(tuple<Types...>&& t) noexcept requires(Index < sizeof...(Types)) {
  return details::storage_get<Index>(std::move(t));
}

template <std::size_t Index, typename... Types>
constexpr tuple_element_t<Index, tuple<Types...>> const&
get(tuple<Types...> const& t) noexcept requires(Index < sizeof...(Types)) {
  return details::storage_get<Index>(t);
}

template <std::size_t Index, typename... Types>
constexpr tuple_element_t<Index, tuple<Types...>> const&&
get(tuple<Types...> const&& t) noexcept requires(Index < sizeof...(Types)) {
  return details::storage_get<Index>(std::move(t));
}

template <typename T, typename... Types>
constexpr T& get(tuple<Types...>& t) noexcept
    requires(details::is_only_once<T, Types...>) {
  return get<details::get_index_v<T, Types...>>(t);
}

template <typename T, typename... Types>
constexpr T&& get(tuple<Types...>&& t) noexcept
    requires(details::is_only_once<T, Types...>) {
  return get<details::get_index_v<T, Types...>>(std::move(t));
}

template <typename T, typename... Types>
constexpr const T& get(const tuple<Types...>& t) noexcept
    requires(details::is_only_once<T, Types...>) {
  return get<details::get_index_v<T, Types...>>(t);
}

template <typename T, typename... Types>
constexpr const T&& get(const tuple<Types...>&& t) noexcept
    requires(details::is_only_once<T, Types...>) {
  return get<details::get_index_v<T, Types...>>(std::move(t));
}

template <>
struct tuple<> {};

template <typename... Types>
struct tuple : details::storage_t<Types...> {
private:
  using base = details::storage_t<Types...>;

public:
  // same types constructors
  // note: think *when* you need to enable all of them
  constexpr tuple() requires(
      std::conjunction_v<std::is_default_constructible<Types>...>) = default;
  constexpr tuple(const Types&... args) : base(args...) {}

  tuple(const tuple&) = default;
  tuple(tuple&&) = default;

  tuple& operator=(const tuple&) = default;
  tuple& operator=(tuple&&) = default;

  // no need for conditional explicitness
  // convert types constructors
  template <typename... UTypes>
  constexpr tuple(UTypes&&... args)
      requires(sizeof...(Types) == sizeof...(UTypes) &&
               std::conjunction_v<std::is_constructible<Types, UTypes>...>)
      : base(std::forward<UTypes>(args)...) {}

  template <
      typename... UTypes, typename T0 = tuple_element_t<0, tuple<Types...>>,
      typename D0 = std::remove_cvref_t<tuple_element_t<0, tuple<UTypes...>>>>
  constexpr tuple(const tuple<UTypes...>& other)
      requires(sizeof...(Types) == sizeof...(UTypes) &&
               std::conjunction_v<std::is_constructible<Types, UTypes>...> &&
               (sizeof...(Types) == 1
                    ? details::IsNotATuple<D0>
                    : (sizeof...(Types) == 2 || sizeof...(Types) == 3
                           ? !std::is_same_v<D0, std::allocator_arg_t> ||
                                 std::is_same_v<T0, std::allocator_arg_t>
                           : true)))
      : base(other) {}
  template <typename... UTypes>
  constexpr tuple(tuple<UTypes...>&& other) requires(
      (sizeof...(Types) == sizeof...(UTypes) &&
       std::conjunction_v<std::is_constructible<
           Types,
           decltype(get<UTypes>(std::forward<decltype(other)>(
               other)))>...>)&&(sizeof...(Types) != 1 ||
                                (!std::conjunction_v<std::is_constructible<
                                     decltype(other), Types>...> &&
                                 !std::conjunction_v<std::is_constructible<
                                     Types, decltype(other)>...> &&
                                 !std::conjunction_v<
                                     std::is_same<Types, UTypes>...>)))
      : base(std::forward<decltype(other)>(other)) {}
};
// make_tuple -- constructor with auto deducing types
// note references are dereferenced

// see cppreference

template <typename... Types>
constexpr tuple<std::unwrap_ref_decay_t<Types>...> make_tuple(Types&&... args) {
  return tuple<std::unwrap_ref_decay_t<Types>...>(std::forward<Types>(args)...);
}

// swap specialization
template <typename... TTypes, typename... UTypes>
void swap(tuple<TTypes...>& first, tuple<UTypes...>& second) noexcept(
    (std::is_nothrow_swappable_v<TTypes> && ...) &&
    (std::is_nothrow_swappable_v<UTypes> && ...)) {
  details::storage_swap(first, second);
}

// compare operators
template <typename... TTypes, typename... UTypes>
constexpr bool operator==(const tuple<TTypes...>& first,
                          const tuple<UTypes...>& second)
    requires(sizeof...(TTypes) == sizeof...(UTypes)) {
  return details::storage_compare<0, sizeof...(TTypes)>(true, first, second);
}
template <typename... TTypes, typename... UTypes>
constexpr bool operator!=(const tuple<TTypes...>& first,
                          const tuple<UTypes...>& second)
    requires(sizeof...(TTypes) == sizeof...(UTypes)) {
  return !(first == second);
}
template <typename... TTypes, typename... UTypes>
constexpr bool operator<(const tuple<TTypes...>& first,
                         const tuple<UTypes...>& second)
    requires(sizeof...(TTypes) == sizeof...(UTypes)) {
  return details::storage_compare<0, sizeof...(TTypes)>(false, first, second);
}
template <typename... TTypes, typename... UTypes>
constexpr bool operator>(const tuple<TTypes...>& first,
                         const tuple<UTypes...>& second)
    requires(sizeof...(TTypes) == sizeof...(UTypes)) {
  return (second < first);
}
template <typename... TTypes, typename... UTypes>
constexpr bool operator<=(const tuple<TTypes...>& first,
                          const tuple<UTypes...>& second)
    requires(sizeof...(TTypes) == sizeof...(UTypes)) {
  return !(first > second);
}
template <typename... TTypes, typename... UTypes>
constexpr bool operator>=(const tuple<TTypes...>& first,
                          const tuple<UTypes...>& second)
    requires(sizeof...(TTypes) == sizeof...(UTypes)) {
  return !(first < second);
}
