#include <catch2/catch.hpp>

#include "tuple.hpp"

TEST_CASE("A default constructed tuple")
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

TEST_CASE("A value constructed tuple")
{
  constexpr parsec::Tuple<int, int, int> t_const{1, 2, 3};
  STATIC_REQUIRE(t_const.get<0>() == 1);
  STATIC_REQUIRE(t_const.get<1>() == 2);
  STATIC_REQUIRE(t_const.get<2>() == 3);

  const parsec::Tuple<int, int, int> t{1, 2, 3};
  REQUIRE(t.get<0>() == 1);
  REQUIRE(t.get<1>() == 2);
  REQUIRE(t.get<2>() == 3);
}