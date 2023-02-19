#include "tuple.h"
#include "gtest/gtest.h"

struct moveable_helper {
  moveable_helper() = default;
  moveable_helper(moveable_helper&&) : foo(1) {}

  int operator()() const {
    return foo;
  }

private:
  int foo = 0;
};

struct only_moveable {
  explicit only_moveable(moveable_helper&&) : foo(42) {}

  int operator()() const {
    return foo;
  }

private:
  int foo = 0;
};

struct cross2;
struct cross1 {
  explicit cross1(int x) : foo(x) {}

  int operator()() const {
    return foo;
  }

private:
  int foo = 0;
};
struct cross2 {
  explicit operator cross1() {
    return cross1(32);
  }
};

struct copy_helper {
  copy_helper() = default;
  copy_helper(copy_helper const&) : foo(1) {}

  int operator()() const {
    return foo;
  }

private:
  int foo = 0;
};

const double BIG_EPS = 0.000001;

TEST(tuple_constructors, empty) {
  tuple<> x;
  static_assert(tuple_size_v<decltype(x)> == 0);
}

TEST(tuple_constructors, const_refered_constructor) {
  std::vector<int> const a(3, 11);
  std::vector<double> const b(10, 13);
  long long const c = 42;
  copy_helper const d;
  tuple<std::vector<int>, std::vector<double>, long long, copy_helper> x(a, b,
                                                                         c, d);
  EXPECT_EQ(3, get<0>(x).size());
  EXPECT_EQ(0, d());
  EXPECT_EQ(1, get<copy_helper>(x)());
}

TEST(tuple_constructors, cross_types) {
  tuple<float, float> x(1.1, 2);
  EXPECT_FLOAT_EQ(1.1f, get<0>(x));
  EXPECT_FLOAT_EQ(2.f, get<1>(x));

  cross2 a;
  tuple<cross1> b(a);
  EXPECT_EQ(32, get<0>(b)());
}

TEST(tuple_constructors, moves) {
  tuple<int, double, moveable_helper> x;
  get<1>(x) = 3.14;
  EXPECT_EQ(0, get<2>(x)());
  auto y(std::move(x));
  EXPECT_DOUBLE_EQ(3.14, get<1>(y));
  EXPECT_EQ(1, get<2>(y)());

  tuple<moveable_helper, int> z(std::move(get<2>(x)), 42);
  EXPECT_EQ(1, get<0>(z)());
  EXPECT_EQ(42, get<1>(z));

  // cross moves
  tuple<moveable_helper, float, float> a(moveable_helper(), 2., 3.14);
  tuple<only_moveable, float, double> b(std::move(a));
  EXPECT_FLOAT_EQ(2.f, get<1>(b));
  EXPECT_NEAR(3.14, get<2>(b), BIG_EPS);
  EXPECT_EQ(1, get<0>(a)());
  EXPECT_EQ(42, get<0>(b)());
}

TEST(tuple_constructors, copies) {
  tuple<int, double, float> x(1, 2., 3.14);
  auto y(x);
  EXPECT_EQ(get<0>(x), get<0>(y));
  EXPECT_EQ(get<2>(x), get<2>(y));

  tuple<int, float, double> z(x);
  EXPECT_EQ(get<0>(x), get<0>(z));
  EXPECT_NEAR(get<2>(x), get<2>(z), BIG_EPS);
}

TEST(tuple, simple) {
  tuple<int, int, int> x(1, 2, 3);
  EXPECT_EQ(1, get<0>(x));
  EXPECT_EQ(2, get<1>(x));
  EXPECT_EQ(3, get<2>(x));

  tuple<int, int, int> y;
  EXPECT_EQ(0, get<0>(y));
}

TEST(tuple, gets) {
  tuple<int, double, float, int> x;
  // must be "ambigious call" -- 2 ints in tuple!
  // EXPECT_EQ(get<int>(x), 0);

  EXPECT_FLOAT_EQ(0.f, get<float>(x));

  get<float>(x) = 4.2f;
  auto const& y = x;

  EXPECT_FLOAT_EQ(4.2f, get<2>(y));

  tuple<moveable_helper> z;
  EXPECT_EQ(0, get<0>(z)());
  moveable_helper a(get<0>(std::move(z)));
  EXPECT_EQ(1, a());

  tuple<moveable_helper> u;
  moveable_helper b(get<moveable_helper>(std::move(u)));
  EXPECT_EQ(1, b());
}

TEST(tuple, make_tuple_and_tuple_element) {
  int a = 3;
  int& b = a;

  double c = 12;
  auto x = make_tuple(b, c, moveable_helper());

  static_assert(std::is_same_v<tuple_element<0, decltype(x)>::type, int>);
  static_assert(std::is_same_v<tuple_element<1, decltype(x)>::type, double>);
  static_assert(
      std::is_same_v<tuple_element<2, decltype(x)>::type, moveable_helper>);

  EXPECT_EQ(1, get<2>(x)());
}

TEST(tuple, tuple_size) {
  tuple<int, double, int, int> x;
  static_assert(4 == tuple_size<decltype(x)>::value);
  static_assert(1 == tuple_size_v<tuple<int>>);
}

TEST(tuple, compares) {
  tuple<int, int, int> a(1, 2, 3);
  auto b(a);
  EXPECT_EQ(a, b);
  get<0>(b) = 32;
  EXPECT_NE(b, a);
  EXPECT_GT(b, a);
  get<0>(a) = 32;
  get<1>(a) = 3;
  EXPECT_NE(a, b);
  EXPECT_LT(b, a);
  swap(a, b);
  EXPECT_LT(a, b);
}

struct non_default_constructible_t {
  non_default_constructible_t(int x) {}
};

static_assert(std::is_default_constructible_v<tuple<int, double>>);
static_assert(
    !std::is_default_constructible_v<tuple<int, non_default_constructible_t>>);

// Some implicit conversion tests
static_assert(
    std::is_convertible_v<tuple<int, int>, tuple<long long, long long>>);
static_assert(
    std::is_convertible_v<tuple<long long, long long>, tuple<int, int>>);

struct copy_constructible_t {
  copy_constructible_t() {}
  copy_constructible_t(copy_constructible_t const&) {}
};

struct move_constructible_t {
  move_constructible_t(move_constructible_t&&) = default;
  move_constructible_t& operator=(move_constructible_t&&) = default;

private:
  move_constructible_t(move_constructible_t const&) = delete;
  move_constructible_t& operator=(move_constructible_t const&) = delete;
};

static_assert(std::is_copy_constructible_v<tuple<int, int>>);
static_assert(std::is_copy_constructible_v<tuple<copy_constructible_t>>);
static_assert(std::is_copy_assignable_v<tuple<copy_constructible_t>>);

static_assert(!std::is_copy_constructible_v<tuple<move_constructible_t>>);
static_assert(std::is_move_constructible_v<tuple<move_constructible_t>>);
static_assert(std::is_move_assignable_v<tuple<move_constructible_t>>);
