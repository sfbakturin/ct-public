#pragma once

#include <algorithm>

#include "optional_constants.h"

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

namespace details {
template <typename T, bool trivial = std::is_trivially_destructible_v<T>>
struct base {
  constexpr base() : dummy{0} {}
  template <typename... Args>
  constexpr base(in_place_t, Args&&... args)
      : is_present{true}, data(std::forward<Args>(args)...) {}

  bool is_present{false};
  union {
    char dummy;
    T data;
  };

  constexpr void reset() {
    if (this->is_present) {
      this->data.~T();
      this->is_present = false;
    }
  }

  ~base() {
    reset();
  }
};

template <typename T>
struct base<T, true> {
  constexpr base() : dummy{0} {}
  template <typename... Args>
  constexpr base(in_place_t, Args&&... args)
      : is_present{true}, data(std::forward<Args>(args)...) {}

  bool is_present{false};
  union {
    char dummy;
    T data;
  };

  constexpr void reset() {
    this->is_present = false;
  }
};

template <typename T, bool enabled = std::is_trivially_copy_constructible_v<T>>
struct base_trivial_copy_construct : base<T> {
  using base<T>::base;

  constexpr base_trivial_copy_construct(
      base_trivial_copy_construct const& other) {
    if (other.is_present) {
      ::new (&(this->data)) T(other.data);
      this->is_present = true;
    }
  }
};

template <typename T>
struct base_trivial_copy_construct<T, true> : base<T> {
  using base<T>::base;
};

template <typename T, bool enabled = std::is_copy_constructible_v<T>>
struct copy_construct {
  copy_construct() = default;
  ~copy_construct() = default;

  constexpr copy_construct(copy_construct const&) = delete;
  constexpr copy_construct(copy_construct&&) = default;

  copy_construct& operator=(copy_construct const&) = default;
  copy_construct& operator=(copy_construct&&) = default;
};

template <typename T>
struct copy_construct<T, true> {};

template <typename T, bool enabled = std::is_trivially_copy_assignable_v<T>&&
                          std::is_trivially_copy_constructible_v<T>>
struct base_trivial_copy_assign : base_trivial_copy_construct<T> {
  using base_trivial_copy_construct<T>::base_trivial_copy_construct;

  base_trivial_copy_assign& operator=(base_trivial_copy_assign const& other) {
    if (this == &other) {
      return *this;
    }
    if (!this->is_present && !other.is_present) {
      return *this;
    }
    if (this->is_present && !other.is_present) {
      this->reset();
      return *this;
    }
    if (!this->is_present && other.is_present) {
      ::new (&(this->data)) T(other.data);
      this->is_present = true;
      return *this;
    }
    if (this->is_present && other.is_present) {
      this->data = other.data;
      return *this;
    }
  }

  constexpr base_trivial_copy_assign(base_trivial_copy_assign const&) = default;
};

template <typename T>
struct base_trivial_copy_assign<T, true> : base_trivial_copy_construct<T> {
  using base_trivial_copy_construct<T>::base_trivial_copy_construct;
};

template <typename T, bool enabled = std::is_copy_assignable_v<T>&&
                          std::is_copy_constructible_v<T>>
struct copy_assign {
  copy_assign() = default;
  ~copy_assign() = default;

  copy_assign& operator=(copy_assign const&) = delete;
  copy_assign& operator=(copy_assign&&) = default;

  constexpr copy_assign(copy_assign const&) = default;
  constexpr copy_assign(copy_assign&&) = default;
};

template <typename T>
struct copy_assign<T, true> {};

template <typename T, bool enabled = std::is_trivially_move_constructible_v<T>>
struct base_trivial_move_construct : base_trivial_copy_assign<T> {
  using base_trivial_copy_assign<T>::base_trivial_copy_assign;

  constexpr base_trivial_move_construct(base_trivial_move_construct const&) =
      default;
  constexpr base_trivial_move_construct(base_trivial_move_construct&& other) {
    if (other.is_present) {
      ::new (&(this->data)) T(std::move(other.data));
      this->is_present = true;
    }
  }

  base_trivial_move_construct&
  operator=(base_trivial_move_construct const&) = default;
};

template <typename T>
struct base_trivial_move_construct<T, true> : base_trivial_copy_assign<T> {
  using base_trivial_copy_assign<T>::base_trivial_copy_assign;

  constexpr base_trivial_move_construct(base_trivial_move_construct const&) =
      default;
  constexpr base_trivial_move_construct(base_trivial_move_construct&&) =
      default;

  base_trivial_move_construct&
  operator=(base_trivial_move_construct&&) = default;
  base_trivial_move_construct&
  operator=(base_trivial_move_construct const&) = default;
};

template <typename T, bool enabled = std::is_move_constructible_v<T>>
struct move_construct {
  move_construct() = default;
  ~move_construct() = default;

  constexpr move_construct(move_construct const&) = default;
  constexpr move_construct(move_construct&&) = delete;

  move_construct& operator=(move_construct const&) = default;
  move_construct& operator=(move_construct&&) = default;
};

template <typename T>
struct move_construct<T, true> {};

template <typename T, bool enabled = std::is_trivially_move_assignable_v<T>&&
                          std::is_trivially_move_constructible_v<T>>
struct base_trivial_move_assign : base_trivial_move_construct<T> {
  using base_trivial_move_construct<T>::base_trivial_move_construct;

  constexpr base_trivial_move_assign(base_trivial_move_assign&&) = default;
  constexpr base_trivial_move_assign(base_trivial_move_assign const&) = default;

  base_trivial_move_assign&
  operator=(base_trivial_move_assign const&) = default;
  base_trivial_move_assign& operator=(base_trivial_move_assign&& other) {
    if (this == &other) {
      return *this;
    }
    if (!this->is_present && !other.is_present) {
      return *this;
    }
    if (this->is_present && !other.is_present) {
      this->reset();
      return *this;
    }
    if (!this->is_present && other.is_present) {
      ::new (&(this->data)) T(std::move(other.data));
      this->is_present = true;
      return *this;
    }
    if (this->is_present && other.is_present) {
      this->data = std::move(other.data);
      return *this;
    }
  }
};

template <typename T>
struct base_trivial_move_assign<T, true> : base_trivial_move_construct<T> {
  using base_trivial_move_construct<T>::base_trivial_move_construct;
};

template <typename T, bool enabled = std::is_move_assignable_v<T>&&
                          std::is_move_constructible_v<T>>
struct move_assign {
  move_assign() = default;
  ~move_assign() = default;

  constexpr move_assign(move_assign&&) = default;
  constexpr move_assign(move_assign const&) = default;

  move_assign& operator=(move_assign const&) = default;
  move_assign& operator=(move_assign&&) = delete;
};

template <typename T>
struct move_assign<T, true> {};
} // namespace details
