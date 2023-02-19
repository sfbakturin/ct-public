#pragma once

#include <cstring>
#include <memory>
#include <utility>

/**
 * @author Saveliy Bakturin
 * <p>
 * Don't write off, if you don't wanna be banned!
 */

struct bad_function_call : std::exception {
  char const* what() {
    return "ERROR: Empty function-object call.\n";
  }
};

namespace details {
using storaged_t = void*;
using storage_t =
    std::aligned_storage_t<sizeof(storaged_t), alignof(storaged_t)>;

template <typename T>
T* get_static(storage_t& source) {
  return reinterpret_cast<T*>(&source);
}

template <typename T>
T const* get_static(storage_t const& source) {
  return reinterpret_cast<T const*>(&source);
}

template <typename T>
T* get_dynamic(storage_t const& source) {
  std::size_t ptr = 0;
  std::memcpy(&ptr, reinterpret_cast<unsigned char const*>(&source),
              sizeof(ptr));
  return reinterpret_cast<T*>(ptr);
}

template <typename T>
constexpr inline bool fits_small =
    sizeof(T) < sizeof(storage_t) && alignof(storage_t) % alignof(T) == 0 &&
    std::is_nothrow_move_constructible_v<T> &&
    std::is_nothrow_move_assignable_v<T>;

} // namespace details

template <typename R, typename... Args>
struct type_descriptor {
  void (*copy)(details::storage_t const& source,
               details::storage_t& destination);
  void (*move)(details::storage_t& source, details::storage_t& destination);
  void (*destroy)(details::storage_t& source);
  R (*invoke)(details::storage_t const& source, Args... arguments);

  static type_descriptor<R, Args...> const* get_empty_descriptor() noexcept {
    constexpr static type_descriptor<R, Args...> result = {
        [](details::storage_t const& source, details::storage_t& destination) {
        },
        [](details::storage_t& source, details::storage_t& destination) {},
        [](details::storage_t& source) {},
        [](details::storage_t const& source, Args... arguments) -> R {
          throw bad_function_call{};
        }};
    return &result;
  }

  template <typename T>
  static type_descriptor<R, Args...> const* get_descriptor() noexcept {
    constexpr static type_descriptor<R, Args...> result = {
        [](details::storage_t const& source, details::storage_t& destination) {
          if constexpr (details::fits_small<T>)
            new (&destination) T(*details::get_static<T>(source));
          else
            new (&destination) T*(new T(*details::get_dynamic<T>(source)));
        },
        [](details::storage_t& source, details::storage_t& destination) {
          if constexpr (details::fits_small<T>) {
            new (&destination) T(std::move(*details::get_static<T>(source)));
            details::get_static<T>(source)->~T();
          } else
            new (&destination) T*(details::get_dynamic<T>(source));
        },
        [](details::storage_t& source) {
          if constexpr (details::fits_small<T>)
            details::get_static<T>(source)->~T();
          else
            delete details::get_dynamic<T>(source);
        },
        [](details::storage_t const& source, Args... arguments) -> R {
          if constexpr (details::fits_small<T>)
            return (*details::get_static<T>(source))(
                std::forward<Args>(arguments)...);
          else
            return (*details::get_dynamic<T>(source))(
                std::forward<Args>(arguments)...);
        }};
    return &result;
  }
};

template <typename F>
struct function;

template <typename R, typename... Args>
struct function<R(Args...)> {
private:
  details::storage_t func;
  type_descriptor<R, Args...> const* desc;

public:
  function() noexcept
      : desc(type_descriptor<R, Args...>::get_empty_descriptor()) {}

  function(function const& other) : desc(other.desc) {
    other.desc->copy(other.func, func);
  }

  function(function&& other) noexcept : desc(other.desc) {
    other.desc->move(other.func, func);
    other.desc = type_descriptor<R, Args...>::get_empty_descriptor();
  }

  template <typename T>
  function(T val)
      : desc(type_descriptor<R, Args...>::template get_descriptor<T>()) {
    if constexpr (details::fits_small<T>)
      new (&func) T(std::move(val));
    else
      new (&func) T*(new T(std::move(val)));
  }

  function& operator=(function const& other) {
    if (this == &other)
      return *this;
    function(other).swap(*this);
    return *this;
  }

  function& operator=(function&& other) {
    if (this == &other)
      return *this;
    function(std::move(other)).swap(*this);
    return *this;
  }

  void swap(function& other) {
    details::storage_t transfer;
    desc->move(func, transfer);
    desc->move(other.func, func);
    desc->move(transfer, other.func);
    std::swap(desc, other.desc);
  }

  ~function() {
    desc->destroy(func);
  }

  explicit operator bool() const noexcept {
    return type_descriptor<R, Args...>::get_empty_descriptor() != desc;
  }

  R operator()(Args... args) const {
    return desc->invoke(func, std::forward<Args>(args)...);
  }

  template <typename T>
  T* target() noexcept {
    if (type_descriptor<R, Args...>::template get_descriptor<T>() != desc)
      return nullptr;

    if constexpr (details::fits_small<T>)
      return details::get_static<T>(func);
    else
      return details::get_dynamic<T>(func);
  }

  template <typename T>
  T const* target() const noexcept {
    if (type_descriptor<R, Args...>::template get_descriptor<T>() != desc)
      return nullptr;

    if constexpr (details::fits_small<T>)
      return details::get_static<T>(func);
    else
      return details::get_dynamic<T>(func);
  }
};
