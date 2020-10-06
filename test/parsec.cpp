#include <catch2/catch.hpp>

#include "parsec.hpp"

TEST_CASE("Matches a character")
{
  THEN("Parsing an empty string gets no result")
  {
    STATIC_REQUIRE(parsec::character('t')("") == std::nullopt);
    REQUIRE(parsec::character('t')("") == std::nullopt);
  }

  WHEN("can parse a string with matching front character")
  {
    constexpr auto result_opt_const = parsec::character('H')("Hello, world!");
    const auto result_opt = parsec::character('H')("Hello, world!");

    THEN("Generates that character and the tail of the string")
    {
      STATIC_REQUIRE(result_opt_const);
      STATIC_REQUIRE(result_opt_const->first == 'H');
      STATIC_REQUIRE(result_opt_const->second == "ello, world!");

      REQUIRE(result_opt);
      CHECK(result_opt->first == 'H');
      CHECK(result_opt->second == "ello, world!");
    }
  }

  THEN("Parsing a string with non-matching front gets no result")
  {
    STATIC_REQUIRE(parsec::character('t')("Hello, world!") == std::nullopt);
    REQUIRE(parsec::character('t')("Hello, world!") == std::nullopt);
  }
}

TEST_CASE("Map")
{
  GIVEN("A mapped character parser")
  {
    constexpr auto parser =
        parsec::map([](char c) { return c + 1; }, parsec::character('t'));

    THEN("Gets no output with bad input")
    {
      STATIC_REQUIRE(!parser("Hello world"));
      REQUIRE(!parser("Hello world"));
    }

    THEN("Gets correct output with good input")
    {
      STATIC_REQUIRE(parser("tea"));
      STATIC_REQUIRE(parser("tea")->first == ('t' + 1));
      REQUIRE(parser("tea"));
      REQUIRE(parser("tea")->first == ('t' + 1));
    }
  }
}