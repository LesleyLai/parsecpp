#include <catch2/catch.hpp>

#include "tuple.hpp"

TEST_CASE("Tuple Default Constructor")
{
  constexpr parsec::Tuple<int, int, int> t_const;
  STATIC_REQUIRE(t_const.get<0>() == 0);
  STATIC_REQUIRE(t_const.get<1>() == 0);
  STATIC_REQUIRE(t_const.get<2>() == 0);

  const parsec::Tuple<int, int, int> t;
  REQUIRE(t.get<0>() == 0);
  REQUIRE(t.get<1>() == 0);
  REQUIRE(t.get<2>() == 0);
}

struct S {
  int x;
  int y;
  double z;
  constexpr friend auto operator==(S, S) -> bool = default;
};

constexpr auto to_s(int x, int y, double z)
{
  return S{.x = x, .y = y, .z = z};
};

TEST_CASE("Tuple Functions")
{
  GIVEN("A tuple (1, 2, 3.5)")
  {
    constexpr parsec::Tuple t_const{1, 2, 3.5};
    const parsec::Tuple t{1, 2, 3.5};

    THEN("All of its elements has correct value")
    {
      STATIC_REQUIRE(t_const.get<0>() == 1);
      STATIC_REQUIRE(t_const.get<1>() == 2);
      STATIC_REQUIRE(t_const.get<2>() == 3.5);
      REQUIRE(t.get<0>() == 1);
      REQUIRE(t.get<1>() == 2);
      REQUIRE(t.get<2>() == 3.5);
    }

    THEN("We can apply its elements with function")
    {
      constexpr auto expected = S{1, 2, 3.5};
      constexpr S result_const = parsec::apply(to_s, t_const);
      STATIC_REQUIRE(result_const == expected);

      const S result = parsec::apply(to_s, t);
      REQUIRE(result == expected);
    }

    THEN("We can push back another element to the tuple")
    {
      constexpr auto result_const = parsec::tuple_push_back(t_const, 'c');
      STATIC_REQUIRE(parsec::get<0>(result_const) == 1);
      STATIC_REQUIRE(parsec::get<1>(result_const) == 2);
      STATIC_REQUIRE(parsec::get<2>(result_const) == 3.5);
      STATIC_REQUIRE(parsec::get<3>(result_const) == 'c');

      const auto result = parsec::tuple_push_back(t, 'c');
      CHECK(parsec::get<0>(result) == 1);
      CHECK(parsec::get<1>(result) == 2);
      CHECK(parsec::get<2>(result) == 3.5);
      CHECK(parsec::get<3>(result) == 'c');
    }
  }
}