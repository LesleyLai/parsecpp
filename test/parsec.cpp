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
    constexpr auto parser = parsec::character('H');

    THEN("Generates that character and the tail of the string")
    {
      constexpr auto result_const = parser("Hello, world!");
      STATIC_REQUIRE(result_const);
      STATIC_REQUIRE(result_const->output == 'H');
      STATIC_REQUIRE(result_const->remaining == "ello, world!");

      const auto result = parser("Hello, world!");
      REQUIRE(result);
      CHECK(result->output == 'H');
      CHECK(result->remaining == "ello, world!");
    }
  }

  THEN("Parsing a string with non-matching front gets no result")
  {
    constexpr auto parser = parsec::character('t');
    STATIC_REQUIRE(parser("Hello, world!") == std::nullopt);
    REQUIRE(parser("Hello, world!") == std::nullopt);
  }
}

TEST_CASE("Int")
{
  GIVEN("An parser")
  {
    constexpr auto parser = parsec::integer();

    THEN("Can parse 42")
    {
      STATIC_REQUIRE(parser("42"));
      STATIC_REQUIRE(parser("42")->output == 42);
      STATIC_REQUIRE(parser("42")->remaining == "");
      REQUIRE(parser("42"));
      REQUIRE(parser("42")->output == 42);
      REQUIRE(parser("42")->remaining == "");
    }

    THEN("Can parse 1234567890")
    {
      STATIC_REQUIRE(parser("1234567890"));
      STATIC_REQUIRE(parser("1234567890")->output == 1234567890);
      STATIC_REQUIRE(parser("1234567890")->remaining == "");
      REQUIRE(parser("1234567890"));
      REQUIRE(parser("1234567890")->output == 1234567890);
      REQUIRE(parser("1234567890")->remaining == "");
    }

    THEN("Negative numbers are not parsed")
    {
      STATIC_REQUIRE(!parser("-42"));
      REQUIRE(!parser("-42"));
    }

    THEN("Can parse 0")
    {
      STATIC_REQUIRE(parser("0"));
      STATIC_REQUIRE(parser("0")->output == 0);
      STATIC_REQUIRE(parser("0")->remaining == "");
      REQUIRE(parser("0"));
      REQUIRE(parser("0")->output == 0);
      REQUIRE(parser("0")->remaining == "");
    }

    THEN("Octal numbers are not parsed")
    {
      STATIC_REQUIRE(!parser("042"));
      REQUIRE(!parser("042"));
    }
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
      STATIC_REQUIRE(parser("tea")->output == ('t' + 1));
      STATIC_REQUIRE(parser("tea")->remaining == "ea");
      REQUIRE(parser("tea"));
      REQUIRE(parser("tea")->output == ('t' + 1));
      REQUIRE(parser("tea")->remaining == "ea");
    }
  }
}
