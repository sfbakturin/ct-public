#pragma once

#include "constants.h"
#include "in_place.h"

namespace details::base {
template <bool, typename...>
struct storage {};

template <bool, typename...>
struct base {
  void destroy() noexcept {}
};

template <typename... Types>
using storage_t = storage<std::conjunction_v<std::is_trivially_destructible<Types>...>, Types...>;

template <typename... Types>
using base_t = base<std::conjunction_v<std::is_trivially_destructible<Types>...>, Types...>;

template <typename Head, typename... Tail>
struct storage<true, Head, Tail...> {
  union {
    Head head;
    storage_t<Tail...> tail;
  };

  constexpr storage() {}

  template <typename... Args>
  constexpr explicit storage(in_place_index_t<0>, Args&&... args) : head(std::forward<Args>(args)...) {}

  template <std::size_t Index, typename... Args>
  constexpr explicit storage(in_place_index_t<Index>, Args&&... args)
      : tail(in_place_index<Index - 1>, std::forward<Args>(args)...) {}
};

template <typename Head, typename... Tail>
struct storage<false, Head, Tail...> {
  union {
    Head head;
    storage_t<Tail...> tail;
  };

  constexpr storage() {}

  template <typename... Args>
  constexpr explicit storage(in_place_index_t<0>, Args&&... args) : head(std::forward<Args>(args)...) {}

  template <std::size_t Index, typename... Args>
  constexpr explicit storage(in_place_index_t<Index>, Args&&... args)
      : tail(in_place_index<Index - 1>, std::forward<Args>(args)...) {}

  ~storage() {}
};

template <std::size_t Index, typename Storage>
constexpr decltype(auto) storage_get(Storage&& storage) noexcept {
  if constexpr (Index == 0) {
    return (std::forward<Storage>(storage).head);
  } else {
    return (storage_get<Index - 1>(std::forward<Storage>(storage).tail));
  }
}

template <typename Head, typename... Tail, typename Storage>
void storage_construct(std::size_t index, storage_t<Head, Tail...>& to, Storage&& from) {
  if (index == 0) {
    std::construct_at(std::addressof(to.head), std::forward<Storage>(from).head);
  } else if constexpr (sizeof...(Tail) > 0) {
    storage_construct(index - 1, to.tail, std::forward<Storage>(from).tail);
  }
}

template <typename Head, typename... Tail, typename Storage>
void storage_assign(std::size_t index, storage_t<Head, Tail...>& to, Storage&& from) {
  if (index == 0) {
    to.head = std::forward<Storage>(from).head;
  } else if constexpr (sizeof...(Tail) > 0) {
    storage_assign(index - 1, to.tail, std::forward<Storage>(from).tail);
  }
}

template <typename Head, typename... Tail>
void storage_destroy(std::size_t index, storage_t<Head, Tail...>& to) {
  if (index == 0) {
    to.head.~Head();
  } else if constexpr (sizeof...(Tail) > 0) {
    storage_destroy(index - 1, to.tail);
  }
}

template <typename... Args>
struct parameter_pack {
  template <std::size_t Index, typename Head, typename... Tail>
  static void storage_construct(storage_t<Head, Tail...>& to, Args&&... args) {
    if constexpr (Index == 0) {
      std::construct_at(std::addressof(to.head), std::forward<Args>(args)...);
    } else if constexpr (sizeof...(Tail) > 0) {
      parameter_pack<Args...>::template storage_construct<Index - 1>(to.tail, std::forward<Args>(args)...);
    }
  }

  template <std::size_t Index, typename Arg, typename Head, typename... Tail>
  static void storage_assign(storage_t<Head, Tail...>& to, Arg&& arg) {
    if constexpr (Index == 0) {
      to.head = std::forward<Arg>(arg);
    } else if constexpr (sizeof...(Tail) > 0) {
      parameter_pack<>::template storage_assign<Index - 1>(to.tail, std::forward<Arg>(arg));
    }
  }
};

template <typename Head, typename... Tail>
struct base<true, Head, Tail...> : storage_t<Head, Tail...> {
  using data = storage_t<Head, Tail...>;
  std::size_t current_alt = 0;

  constexpr base() noexcept(std::conjunction_v<std::is_nothrow_default_constructible<Head>>)
      requires(std::is_default_constructible_v<Head>)
      : data(in_place_index<0>) {}
  constexpr base() noexcept requires(!std::is_default_constructible_v<Head>) = default;

  template <std::size_t Index, typename... Args>
  constexpr explicit base(in_place_index_t<Index>, Args&&... args)
      : data(in_place_index<Index>, std::forward<Args>(args)...), current_alt(Index) {}

  constexpr std::size_t index() const noexcept {
    return current_alt;
  }

  constexpr bool valueless_by_exception() const noexcept {
    return current_alt == variant_npos;
  }

  void destroy() noexcept {
    current_alt = variant_npos;
  }
};

template <typename Head, typename... Tail>
struct base<false, Head, Tail...> : storage_t<Head, Tail...> {
  using data = storage_t<Head, Tail...>;
  std::size_t current_alt = 0;

  constexpr base() noexcept(std::conjunction_v<std::is_nothrow_default_constructible<Head>>)
      requires(std::is_default_constructible_v<Head>)
      : data(in_place_index<0>) {}
  constexpr base() noexcept requires(!std::is_default_constructible_v<Head>) = default;

  template <std::size_t Index, typename... Args>
  constexpr explicit base(in_place_index_t<Index>, Args&&... args)
      : data(in_place_index<Index>, std::forward<Args>(args)...), current_alt(Index) {}

  constexpr std::size_t index() const noexcept {
    return current_alt;
  }

  constexpr bool valueless_by_exception() const noexcept {
    return current_alt == variant_npos;
  }

  void destroy() noexcept {
    if (!valueless_by_exception()) {
      storage_destroy(current_alt, *this);
    }
    current_alt = variant_npos;
  }

  ~base() {
    destroy();
  }
};
} // namespace details::base
