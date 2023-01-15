#pragma once

#include "base.h"
#include "concepts.h"
#include "constants.h"
#include "function_builder.h"
#include "helpers.h"
#include "in_place.h"
#include "variant_alternative.h"
#include "variant_size.h"
#include "visit.h"

template <typename, typename...>
struct variant;

template <typename T, typename... Types>
requires std::disjunction_v<std::is_same<T, Types>...> constexpr bool
holds_alternative(variant<Types...> const& v) noexcept {
  return details::helpers::get_index_v<T, Types...> == v.index();
}

template <std::size_t Index, typename Variant>
constexpr decltype(auto) get(Variant&& v) {
  if (Index != v.index() || v.valueless_by_exception()) {
    throw bad_variant_access();
  }
  return (storage_get<Index>(std::forward<Variant>(v)));
}

template <typename T, typename... Types>
constexpr T& get(variant<Types...>& v) {
  return get<details::helpers::get_index_v<T, Types...>>(v);
}

template <typename T, typename... Types>
constexpr T&& get(variant<Types...>&& v) {
  return get<details::helpers::get_index_v<T, Types...>>(v);
}

template <typename T, typename... Types>
constexpr T const& get(variant<Types...> const& v) {
  return get<details::helpers::get_index_v<T, Types...>>(v);
}

template <typename T, typename... Types>
constexpr T const&& get(variant<Types...> const&& v) {
  return get<details::helpers::get_index_v<T, Types...>>(v);
}

template <std::size_t Index, typename... Types>
constexpr std::add_pointer_t<variant_alternative_t<Index, variant<Types...>>> get_if(variant<Types...>* pv) noexcept {
  if (!pv || pv->index() != Index) {
    return nullptr;
  }
  return std::addressof(get<Index>(*pv));
}

template <std::size_t Index, typename... Types>
constexpr std::add_pointer_t<const variant_alternative_t<Index, variant<Types...>>>
get_if(const variant<Types...>* pv) noexcept {
  return const_cast<variant_alternative_t<Index, variant<Types...>>>(const_cast<variant<Types...>*>(pv));
}

template <typename T, typename... Types>
constexpr std::add_pointer_t<T> get_if(variant<Types...>* pv) noexcept {
  return get_if<details::helpers::get_index_v<T, Types...>>(pv);
}

template <typename T, typename... Types>
constexpr std::add_pointer_t<const T> get_if(const variant<Types...>* pv) noexcept {
  return get_if<details::helpers::get_index_v<T, Types...>>(pv);
}

template <typename Auditor, typename... Variants>
constexpr auto visit(Auditor&& auditor, Variants&&... variants) {
  if ((std::forward<Variants>(variants).valueless_by_exception() || ...)) {
    throw bad_variant_access();
  }
  return details::visit::visit_many(std::forward<Auditor>(auditor), std::forward<Variants>(variants)...);
}

template <typename Head, typename... Tail>
struct variant : details::base::base_t<Head, Tail...> {
private:
  using base = details::base::base_t<Head, Tail...>;

  template <typename, typename... Types1>
  friend constexpr bool holds_alternative(variant<Types1...> const&) noexcept;

  constexpr void move(variant& to, variant&& from) {
    to = std::move(from);
    from.destroy();
  }

public:
  constexpr variant() requires(std::is_default_constructible_v<Head>) = default;

  constexpr variant(variant&&) requires(!details::concepts::MoveConstructible<Head, Tail...>) = delete;
  constexpr variant(variant&&) requires(details::concepts::TriviallyMoveConstructible<Head, Tail...>) = default;
  constexpr variant(variant&& other) noexcept(
      std::conjunction_v<std::is_nothrow_move_constructible<Head>, std::is_nothrow_move_constructible<Tail>...>)
      requires(!details::concepts::TriviallyMoveConstructible<Head, Tail...> &&
               details::concepts::MoveConstructible<Head, Tail...>) {
    if (!this->valueless_by_exception()) {
      storage_construct(other.current_alt, *this, std::move(other));
      this->current_alt = other.current_alt;
    }
  }

  constexpr variant(variant const&) requires(!details::concepts::CopyConstructible<Head, Tail...>) = delete;
  constexpr variant(variant const&) requires(details::concepts::TriviallyCopyConstructible<Head, Tail...>) = default;
  constexpr variant(variant const& other) requires(!details::concepts::TriviallyCopyConstructible<Head, Tail...> &&
                                                   details::concepts::CopyConstructible<Head, Tail...>) {
    if (!this->valueless_by_exception()) {
      storage_construct(other.current_alt, *this, other);
      this->current_alt = other.current_alt;
    }
  }

  variant& operator=(variant&&) requires(!details::concepts::MoveAssignable<Head, Tail...>) = delete;
  variant& operator=(variant&&) requires(details::concepts::TriviallyMoveAssignable<Head, Tail...>) = default;
  variant& operator=(variant&& other) noexcept(
      std::conjunction_v<std::is_nothrow_move_constructible<Head>, std::is_nothrow_move_constructible<Tail>...>&&
          std::conjunction_v<std::is_nothrow_move_assignable<Head>, std::is_nothrow_move_assignable<Tail>...>)
      requires(!details::concepts::TriviallyMoveAssignable<Head, Tail...> &&
               details::concepts::MoveAssignable<Head, Tail...>) {
    if (this == std::addressof(other)) {
      return *this;
    }
    if (other.valueless_by_exception()) {
      this->destroy();
      return *this;
    }
    if (this->index() == other.index()) {
      storage_assign(other.index(), *this, std::move(other));
      return *this;
    }
    this->destroy();
    storage_construct(other.index(), *this, std::move(other));
    this->current_alt = other.index();
    return *this;
  }

  variant& operator=(variant const&) requires(!details::concepts::CopyAssignable<Head, Tail...>) = delete;
  variant& operator=(variant const&) requires(details::concepts::TriviallyCopyAssignable<Head, Tail...>) = default;
  variant& operator=(variant const& other) requires(!details::concepts::TriviallyCopyAssignable<Head, Tail...> &&
                                                    details::concepts::CopyAssignable<Head, Tail...>) {
    if (this == std::addressof(other)) {
      return *this;
    }
    if (other.valueless_by_exception()) {
      this->destroy();
      return *this;
    }
    if (this->index() == other.index()) {
      storage_assign(other.index(), *this, other);
      return *this;
    }
    this->destroy();
    visit(
        [this, &other](auto&& rhs) {
          using Type = std::remove_cvref_t<decltype(rhs)>;
          if constexpr (std::is_nothrow_copy_constructible_v<Type> || !std::is_nothrow_move_constructible_v<Type>) {
            this->emplace<Type>(rhs);
          } else {
            this->operator=(variant(other));
          }
        },
        other);
    this->current_alt = other.index();
    return *this;
  }

  template <std::size_t Index, typename... Args>
  requires((Index < sizeof...(Tail) + 1) &&
           (std::is_constructible_v<details::helpers::get_type_t<Index, Head, Tail...>,
                                    Args...>)) constexpr explicit variant(in_place_index_t<Index>, Args&&... args)
      : base(in_place_index<Index>, std::forward<Args>(args)...) {}

  template <typename T, typename... Args>
  requires((std::disjunction_v<std::is_same<Head, T>, std::is_same<T, Tail>...>)&&(
      std::is_constructible_v<T, Args...>)) constexpr explicit variant(in_place_type_t<T>, Args&&... args)
      : base(in_place_index<details::helpers::get_index_v<T, Head, Tail...>>, std::forward<Args>(args)...) {}

  template <typename T, typename Found = details::function::found_type_t<T, Head, Tail...>,
            std::size_t Index = details::function::found_index_v<Found, Head, Tail...>>
  requires((!std::is_same_v<T, variant<Head, Tail...>>)&&(Index < sizeof...(Tail) + 1)) constexpr variant(
      T&& t) noexcept(std::is_nothrow_constructible_v<details::function::found_type_t<T, Head, Tail...>, T>)
      : base(in_place_index<Index>, std::forward<T>(t)) {}

  template <typename T, typename Found = details::function::found_type_t<T, Head, Tail...>,
            std::size_t Index = details::function::found_index_v<T, Head, Tail...>>
  requires((!std::is_same_v<T, variant<Head, Tail...>>)&&(Index < sizeof...(Tail) + 1)) constexpr variant&
  operator=(T&& t) noexcept(std::is_nothrow_constructible_v<details::function::found_type_t<T, Head, Tail...>, T>) {
    if (Index == this->index()) {
      details::base::parameter_pack<>::template storage_assign<Index>(*this, std::forward<T>(t));
    } else {
      if constexpr (std::is_nothrow_constructible_v<Found, T> || !std::is_nothrow_move_constructible_v<Found>) {
        this->emplace<Index>(std::forward<T>(t));
      } else {
        this->emplace<Index>(Found(std::forward<T>(t)));
      }
    }
    this->current_alt = Index;
    return *this;
  }

  template <typename T, typename... Args>
  requires((std::is_same_v<T, Head> || (std::is_same_v<T, Tail> || ...)) &&
           (std::is_constructible_v<T, Args...>)) constexpr T& emplace(Args&&... args) {
    return emplace<details::function::get_index_v<T, Head, Tail...>>(std::forward<Args>(args)...);
  }

  template <std::size_t Index, typename... Args>
  constexpr variant_alternative_t<Index, variant>& emplace(Args&&... args) {
    this->destroy();
    details::base::parameter_pack<Args...>::template storage_construct<Index>(*this, std::forward<Args>(args)...);
    this->current_alt = Index;
    return get<Index>(*this);
  }

  constexpr void swap(variant& other) noexcept(
      std::is_nothrow_move_constructible_v<Head>&& std::is_nothrow_swappable_v<Head> &&
      ((std::is_nothrow_move_constructible_v<Tail> && std::is_nothrow_swappable_v<Tail>)&&...)) {
    if (this->valueless_by_exception() && other.valueless_by_exception()) {
      return;
    }
    if (this->valueless_by_exception() && !other.valueless_by_exception()) {
      move(*this, std::move(other));
      return;
    }
    if (!this->valueless_by_exception() && other.valueless_by_exception()) {
      move(other, std::move(*this));
      return;
    }
    if (this->index() != other.index()) {
      variant temp(std::move(*this));
      *this = std::move(other);
      other = std::move(temp);
    } else {
      visit(
          [](auto&& a, auto&& b) {
            using A = decltype(a);
            using B = decltype(b);
            if constexpr (std::is_same_v<A, B>) {
              using std::swap;
              swap(a, b);
            }
          },
          *this, other);
    }
  }
};

template <typename... Types>
constexpr bool operator==(variant<Types...> const& v, variant<Types...> const& w) {
  if (v.index() != w.index()) {
    return false;
  }
  if (v.valueless_by_exception()) {
    return true;
  }
  return visit(
      [](auto&& a, auto&& b) {
        using A = decltype(a);
        using B = decltype(b);
        if constexpr (std::is_same_v<A, B>) {
          return a == b;
        } else {
          return false;
        }
      },
      v, w);
}

template <typename... Types>
constexpr bool operator!=(variant<Types...> const& v, variant<Types...> const& w) {
  if (v.index() != w.index()) {
    return true;
  }
  if (v.valueless_by_exception()) {
    return false;
  }
  return visit(
      [](auto&& a, auto&& b) {
        using A = decltype(a);
        using B = decltype(b);
        if constexpr (std::is_same_v<A, B>) {
          return a != b;
        } else {
          return false;
        }
      },
      v, w);
}

template <typename... Types>
constexpr bool operator<(variant<Types...> const& v, variant<Types...> const& w) {
  if (w.valueless_by_exception()) {
    return false;
  }
  if (v.valueless_by_exception()) {
    return true;
  }
  if (v.index() < w.index()) {
    return true;
  }
  if (v.index() > w.index()) {
    return false;
  }
  return visit(
      [](auto&& a, auto&& b) {
        using A = decltype(a);
        using B = decltype(b);
        if constexpr (std::is_same_v<A, B>) {
          return a < b;
        } else {
          return false;
        }
      },
      v, w);
}

template <typename... Types>
constexpr bool operator>(variant<Types...> const& v, variant<Types...> const& w) {
  if (v.valueless_by_exception()) {
    return false;
  }
  if (w.valueless_by_exception()) {
    return true;
  }
  if (v.index() > w.index()) {
    return true;
  }
  if (v.index() < w.index()) {
    return false;
  }
  return visit(
      [](auto&& a, auto&& b) {
        using A = decltype(a);
        using B = decltype(b);
        if constexpr (std::is_same_v<A, B>) {
          return a > b;
        } else {
          return false;
        }
      },
      v, w);
}

template <typename... Types>
constexpr bool operator<=(variant<Types...> const& v, variant<Types...> const& w) {
  if (v.valueless_by_exception()) {
    return true;
  }
  if (w.valueless_by_exception()) {
    return false;
  }
  if (v.index() < w.index()) {
    return true;
  }
  if (v.index() > w.index()) {
    return false;
  }
  return visit(
      [](auto&& a, auto&& b) {
        using A = decltype(a);
        using B = decltype(b);
        if constexpr (std::is_same_v<A, B>) {
          return a <= b;
        } else {
          return false;
        }
      },
      v, w);
}

template <typename... Types>
constexpr bool operator>=(variant<Types...> const& v, variant<Types...> const& w) {
  if (w.valueless_by_exception()) {
    return true;
  }
  if (v.valueless_by_exception()) {
    return false;
  }
  if (v.index() > w.index()) {
    return true;
  }
  if (v.index() < w.index()) {
    return false;
  }
  return visit(
      [](auto&& a, auto&& b) {
        using A = decltype(a);
        using B = decltype(b);
        if constexpr (std::is_same_v<A, B>) {
          return a >= b;
        } else {
          return false;
        }
      },
      v, w);
}
