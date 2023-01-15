#pragma once

#include <array>

#include "helpers.h"

namespace details::visit {
using namespace helpers;

template <std::size_t... Indexes, typename Auditor, typename... Variants>
constexpr auto do_auditor_doing_things(Auditor&& auditor, Variants&&... variants) {
  return std::forward<Auditor>(auditor)(get<Indexes>(std::forward<Variants>(variants))...);
}

template <typename Pointer, std::size_t...>
requires std::is_pointer_v<Pointer> struct table {
  Pointer ptr;

  constexpr Pointer get_ptr() noexcept {
    return ptr;
  }
};

template <typename Pointer, std::size_t Head, std::size_t... Tail>
requires std::is_pointer_v<Pointer> struct table<Pointer, Head, Tail...> {
  std::array<table<Pointer, Tail...>, Head> ptr;

  template <typename... Numeric>
  requires std::conjunction_v<std::is_integral<Numeric>...> constexpr Pointer get_ptr(std::size_t head,
                                                                                      Numeric&&... tail) noexcept {
    return ptr[head].get_ptr(std::forward<Numeric>(tail)...);
  }
};

template <typename...>
struct wrapper_classes;

template <typename...>
struct filler;

template <typename Pointer, typename Auditor, std::size_t Size, typename... Variants, std::size_t Head,
          std::size_t... Tail, std::size_t... Indexes>
struct filler<Pointer, Auditor, std::index_sequence<Size>, wrapper_classes<Variants...>,
              std::index_sequence<Head, Tail...>, std::index_sequence<Indexes...>> {
private:
  template <std::size_t FirstIndex, std::size_t... RestIndexes>
  constexpr void fill_seq(table<Pointer, Head, Tail...>& tab,
                          std::index_sequence<FirstIndex, RestIndexes...>) noexcept {
    filler<Pointer, Auditor, std::index_sequence<Size>, wrapper_classes<Variants...>, std::index_sequence<Tail...>,
           std::index_sequence<Indexes..., FirstIndex>>
        filler;
    filler.fill(tab.ptr[FirstIndex]);
    if constexpr (sizeof...(RestIndexes) > 0) {
      fill_seq(tab, std::index_sequence<RestIndexes...>());
    }
  }

public:
  constexpr void fill(table<Pointer, Head, Tail...>& tab) noexcept {
    fill_seq(tab, std::make_index_sequence<Size>());
  }
};

template <typename Pointer, typename Auditor, std::size_t Size, typename... Variants, std::size_t... Indexes>
struct filler<Pointer, Auditor, std::index_sequence<Size>, wrapper_classes<Variants...>, std::index_sequence<>,
              std::index_sequence<Indexes...>> {
  constexpr void fill(table<Pointer>& tab) noexcept {
    tab.ptr = &do_auditor_doing_things<Indexes...>;
  }
};

template <typename Auditor, typename... Variants>
constexpr auto visit_many(Auditor&& auditor, Variants&&... variants) {
  using Ret = decltype(std::forward<Auditor>(auditor)(get<0>(std::forward<Variants>(variants))...));
  table<Ret (*)(Auditor&&, Variants && ...), variant_size_v<std::remove_cvref_t<Variants>>...> funcs{};
  filler<Ret (*)(Auditor&&, Variants && ...), Auditor,
         std::index_sequence<variant_size_v<get_type_t<0, std::remove_cvref_t<Variants>...>>>,
         wrapper_classes<Variants...>, std::index_sequence<variant_size_v<std::remove_cvref_t<Variants>>...>,
         std::index_sequence<>>
      filler;
  filler.fill(funcs);
  auto result = funcs.get_ptr((std::forward<Variants>(variants).index())...);
  return (*result)(std::forward<Auditor>(auditor), std::forward<Variants>(variants)...);
}
} // namespace details::visit
