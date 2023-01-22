#pragma once

#include <cassert>
#include <cstddef>
#include <exception>
#include <string>
#include <vector>

template <class... Ts>
struct overload : Ts... {
  using Ts::operator()...;
};
template <class... Ts>
overload(Ts...) -> overload<Ts...>;

struct trivial_t {};

struct no_default_t {
  no_default_t() = delete;
};

struct throwing_default_t {
  throwing_default_t() {
    throw std::exception();
  }
};

struct throwing_move_operator_t {
  static size_t swap_called; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
  throwing_move_operator_t() = default;
  throwing_move_operator_t(throwing_move_operator_t&&) noexcept(false) { // NOLINT(bugprone-exception-escape)
    throw std::exception();
  }
  throwing_move_operator_t& operator=(throwing_move_operator_t&&) = default;
};

void swap(throwing_move_operator_t&, throwing_move_operator_t&);

struct no_copy_t {
  no_copy_t(const no_copy_t&) = delete;
};

struct no_move_t {
  no_move_t(no_move_t&&) = delete;
};

struct non_trivial_copy_t {
  explicit non_trivial_copy_t(int x) noexcept : x{x} {}
  non_trivial_copy_t(const non_trivial_copy_t& other) noexcept : x{other.x + 1} {}

  int x;
};

struct non_trivial_copy_assignment_t {
  static constexpr int DELTA = 5;

  explicit non_trivial_copy_assignment_t(int x) noexcept : x{x} {}
  non_trivial_copy_assignment_t& operator=(const non_trivial_copy_assignment_t& other) {
    if (this != &other) {
      x = other.x + DELTA;
    }
    return *this;
  };

  int x;
};

struct non_trivial_int_wrapper_t {
  non_trivial_int_wrapper_t(int x) : x{x} {} // NOLINT(google-explicit-constructor)
  non_trivial_int_wrapper_t& operator=(int i) {
    x = i + 1;
    return *this;
  }
  friend constexpr bool operator==(non_trivial_int_wrapper_t const& lhs,
                                   non_trivial_int_wrapper_t const& rhs) noexcept {
    return lhs.x == rhs.x;
  }
  friend constexpr bool operator!=(non_trivial_int_wrapper_t const& lhs,
                                   non_trivial_int_wrapper_t const& rhs) noexcept {
    return lhs.x != rhs.x;
  }
  friend constexpr bool operator<(non_trivial_int_wrapper_t const& lhs, non_trivial_int_wrapper_t const& rhs) noexcept {
    return lhs.x < rhs.x;
  }
  friend constexpr bool operator<=(non_trivial_int_wrapper_t const& lhs,
                                   non_trivial_int_wrapper_t const& rhs) noexcept {
    return lhs.x <= rhs.x;
  }
  friend constexpr bool operator>(non_trivial_int_wrapper_t const& lhs, non_trivial_int_wrapper_t const& rhs) noexcept {
    return lhs.x > rhs.x;
  }
  friend constexpr bool operator>=(non_trivial_int_wrapper_t const& lhs,
                                   non_trivial_int_wrapper_t const& rhs) noexcept {
    return lhs.x >= rhs.x;
  }
  int x;
};

struct no_move_assignment_t {
  no_move_assignment_t& operator=(no_move_assignment_t&&) = delete;
};

struct no_copy_assignment_t {
  no_copy_assignment_t& operator=(const no_copy_assignment_t&) = delete;
};

struct throwing_move_assignment_t {
  throwing_move_assignment_t(throwing_move_assignment_t&&) = default;
  throwing_move_assignment_t&
  operator=(throwing_move_assignment_t&&) noexcept(false) { // NOLINT(bugprone-exception-escape)
    throw std::exception();
  }
};

struct only_movable {
  static size_t move_assignment_called; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

  constexpr only_movable() = default;

  constexpr only_movable(only_movable&& other) noexcept {
    assert(other.coin && "Move of moved value?");
    coin = true;
    other.coin = false;
  }

  constexpr only_movable& operator=(only_movable&& other) noexcept {
    if (this != &other) {
      assert(other.coin && "Move of moved value?");
      move_assignment_called += 1;
      coin = true;
      other.coin = false;
    }
    return *this;
  }

  [[nodiscard]] constexpr bool has_coin() const noexcept {
    return coin;
  }

  only_movable(only_movable const&) = delete;
  only_movable& operator=(only_movable const&) = delete;

private:
  bool coin{true};
};

struct yac_coin {
  constexpr operator int() noexcept { // NOLINT(google-explicit-constructor)
    return 42;                        // NOLINT(cppcoreguidelines-avoid-magic-numbers)
  }
};

struct coin_wrapper {
  constexpr coin_wrapper() noexcept = default;

  constexpr coin_wrapper(coin_wrapper&& other) noexcept {
    assert(other.coin && "Move of moved value?");
    coin = other.coin;
    other.coin = 0;
  }

  constexpr coin_wrapper& operator=(coin_wrapper&& other) noexcept {
    if (this != &other) {
      assert((other.coin != 0) && "Move of moved value?");
      coin = other.coin;
      other.coin = 0;
    }
    return *this;
  }

  [[nodiscard]] constexpr auto has_coins() const noexcept {
    return coin;
  }

  constexpr explicit coin_wrapper(yac_coin) noexcept : coin{17} {} // NOLINT(cppcoreguidelines-avoid-magic-numbers)

  constexpr coin_wrapper(coin_wrapper const& other) noexcept : coin(other.coin + 1) {}

  constexpr coin_wrapper& operator=(coin_wrapper const& other) noexcept {
    if (this != &other) {
      coin = other.coin + 1;
    }
    return *this;
  }

private:
  int coin{1};
};

struct sqr_sum_visitor {
  template <typename... Args>
  constexpr long operator()(Args... args) const noexcept {
    return ((args * args) + ...);
  }
};

struct strange_visitor {
  strange_visitor() = default;
  strange_visitor(strange_visitor const&) = delete;
  strange_visitor(strange_visitor&&) = default;

  constexpr int operator()(int value) & {
    return value;
  }
  constexpr int operator()(int value) && {
    return value + 1;
  }
  constexpr int operator()(int value) const& {
    return value + 2;
  }
  constexpr int operator()(int value) const&& {
    return value + 3;
  }
};

struct broken_address {
  broken_address* operator&() { // NOLINT(google-runtime-operator)
    return nullptr;
  }
  broken_address const* operator&() const { // NOLINT(google-runtime-operator)
    return nullptr;
  }
  std::vector<int> x;
};

struct empty_comparable {
  empty_comparable() = default;
  empty_comparable(empty_comparable&&) { // NOLINT(bugprone-exception-escape)
    throw std::exception();
  }
  empty_comparable& operator=(empty_comparable&&) { // NOLINT(bugprone-exception-escape)
    throw std::exception();
  }
  bool operator==(const empty_comparable&) const {
    throw std::exception();
  }
  bool operator!=(const empty_comparable&) const {
    throw std::exception();
  }
  bool operator<(const empty_comparable&) const {
    throw std::exception();
  }
  bool operator<=(const empty_comparable&) const {
    throw std::exception();
  }
  bool operator>(const empty_comparable&) const {
    throw std::exception();
  }
  bool operator>=(const empty_comparable&) const {
    throw std::exception();
  }
};
