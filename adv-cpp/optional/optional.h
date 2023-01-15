#pragma once

#include "optional_constants.h"
#include "optional_details.h"

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

template <typename T>
struct optional : details::base_trivial_move_assign<T>,
                  details::copy_construct<T>,
                  details::copy_assign<T>,
                  details::move_construct<T>,
                  details::move_assign<T> {
private:
  using details::base_trivial_move_assign<T>::base_trivial_move_assign;

  constexpr bool has_value() const noexcept {
    return this->is_present;
  }

  constexpr void swap(optional& other) noexcept {
    if (has_value() && other.has_value()) {
      using std::swap;
      swap(this->data, other.data);
    } else if (has_value() && !other.has_value()) {
      other = std::move(*this);
      reset();
    } else if (!has_value() && other.has_value()) {
      other.swap(*this);
    }
  }

public:
  using details::base_trivial_move_assign<T>::reset;

  constexpr optional() noexcept {}
  constexpr optional(nullopt_t) noexcept {}

  constexpr optional(optional const&) = default;
  constexpr optional(optional&&) = default;

  optional& operator=(optional const&) = default;
  optional& operator=(optional&&) = default;

  constexpr explicit optional(T value)
      : details::base_trivial_move_assign<T>(in_place, std::move(value)) {}

  optional& operator=(nullopt_t) noexcept {
    reset();
    return *this;
  }

  constexpr explicit operator bool() const noexcept {
    return has_value();
  }

  constexpr T& operator*() noexcept {
    return this->data;
  }
  constexpr T const& operator*() const noexcept {
    return this->data;
  }

  constexpr T* operator->() noexcept {
    return static_cast<T*>(&(this->data));
  }
  constexpr T const* operator->() const noexcept {
    return static_cast<T const*>(&(this->data));
  }

  template <typename... Args>
  constexpr void emplace(Args&&... args) {
    reset();
    ::new (&(this->data)) T(std::forward<Args>(args)...);
    this->is_present = true;
  }

  ~optional() = default;
};

template <typename T>
constexpr bool operator==(optional<T> const& a, optional<T> const& b) {
  if (bool(a) != bool(b)) {
    return false;
  } else if (!bool(a) || !bool(b)) {
    return true;
  } else {
    return *a == *b;
  }
}

template <typename T>
constexpr bool operator!=(optional<T> const& a, optional<T> const& b) {
  if (bool(a) != bool(b)) {
    return true;
  } else if (!bool(a) || !bool(b)) {
    return false;
  } else {
    return *a != *b;
  }
}

template <typename T>
constexpr bool operator<(optional<T> const& a, optional<T> const& b) {
  if (!bool(b)) {
    return false;
  } else if (!bool(a)) {
    return true;
  } else {
    return *a < *b;
  }
}

template <typename T>
constexpr bool operator<=(optional<T> const& a, optional<T> const& b) {
  if (!bool(a)) {
    return true;
  } else if (!bool(b)) {
    return false;
  } else {
    return *a <= *b;
  }
}

template <typename T>
constexpr bool operator>(optional<T> const& a, optional<T> const& b) {
  if (!bool(a)) {
    return false;
  } else if (!bool(b)) {
    return true;
  } else {
    return *a > *b;
  }
}

template <typename T>
constexpr bool operator>=(optional<T> const& a, optional<T> const& b) {
  if (!bool(b)) {
    return true;
  } else if (!bool(a)) {
    return false;
  } else {
    return *a >= *b;
  }
}
