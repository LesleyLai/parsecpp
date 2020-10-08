#include <catch2/catch.hpp>

#include "parsec_test.hpp"

#define PARSE_AS(text, out, remain)                                            \
  constexpr auto result_const = parser(text);                                  \
  STATIC_REQUIRE(result_const);                                                \
  STATIC_REQUIRE(result_const->output == (out));                               \
  STATIC_REQUIRE(result_const->remaining == (remain));                         \
  const auto result = parser(text);                                            \
  REQUIRE(result);                                                             \
  CHECK(result->output == (out));                                              \
  CHECK(result->remaining == (remain));

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
      PARSE_AS("Hello, world!", 'H', "ello, world!");
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
    constexpr auto parser = parsec::integer;

    THEN("Can parse 42")
    {
      PARSE_AS("42", 42, "");
    }

    THEN("Can parse 1234567890AAA")
    {
      PARSE_AS("1234567890AAA", 1234567890, "AAA");
    }

    THEN("Negative numbers are not parsed")
    {
      STATIC_REQUIRE(!parser("-42"));
      REQUIRE(!parser("-42"));
    }

    THEN("Can parse 0")
    {
      PARSE_AS("0", 0, "");
    }

    THEN("Octal numbers are not parsed")
    {
      STATIC_REQUIRE(!parser("042"));
      REQUIRE(!parser("042"));
    }
  }
}

TEST_CASE("Map parser")
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
      PARSE_AS("tea", 't' + 1, "ea");
    }
  }
}

TEST_CASE("Or parser")
{
  GIVEN("A parser that accepts either character 't' or 'c'")
  {
    constexpr auto parser = parsec::character('t') | parsec::character('c');

    THEN("Gets no output with bad input")
    {
      STATIC_REQUIRE(!parser("Hello world"));
      REQUIRE(!parser("Hello world"));
    }

    THEN("Gets correct output for c")
    {
      PARSE_AS("cat", 'c', "at");
    }

    THEN("Gets correct output for t")
    {
      PARSE_AS("tea", 't', "ea");
    }
  }

  GIVEN("A parser that accepts either character 't' or map 't' to 'u'")
  {
    constexpr auto parser =
        parsec::character('t') |
        parsec::map([](char c) { return static_cast<char>(c + 1); },
                    parsec::character('t'));

    THEN("parse 'tea' get 't' instead of 'u'")
    {
      PARSE_AS("tea", 't', "ea");
    }
  }
}

TEST_CASE("One of parser")
{
  GIVEN("A parser that accepts character 'a', 'e', or 'g', 'h'")
  {
    constexpr auto parser =
        one_of(parsec::character('a'), parsec::character('e'),
               parsec::character('g'), parsec::character('h'));

    THEN("Gets correct output for a")
    {
      PARSE_AS("all", 'a', "ll");
    }

    THEN("Gets correct output for e")
    {
      PARSE_AS("egg", 'e', "gg");
    }

    THEN("Gets correct output for g")
    {
      PARSE_AS("google", 'g', "oogle");
    }

    THEN("Gets correct output for h")
    {
      PARSE_AS("hot", 'h', "ot");
    }
  }
}

TEST_CASE("Succeed parser")
{
  GIVEN("A succeed parser")
  {
    constexpr auto parser = parsec::succeed(10);
    THEN("It parses arbituary string into 10 and keeps the remaining parts")
    {
      PARSE_AS("dragonborn", 10, "dragonborn");
    }
  }
}

struct Point2 {
  int x;
  int y;

  [[nodiscard]] friend constexpr auto operator==(const Point2&,
                                                 const Point2&) noexcept
      -> bool = default;

  friend auto operator<<(std::ostream& os, Point2 p) -> std::ostream&
  {
    os << "{ " << p.x << ", " << p.y << " }" << '\n';
    return os;
  }
};

constexpr auto make_point(int y)
{
  return [y](int x) { return Point2{x, y}; };
}

TEST_CASE("Pipeline parser")
{
  GIVEN("A pipe parser")
  {
    using namespace parsec;

    // clang-format off
    constexpr auto parser =
         parsec::character('{') -=
         parsec::integer +=
         parsec::character(',') -=
         parsec::integer +=
         parsec::character('}') -=
         parsec::succeed(&make_point);
    // clang-format on

    THEN("It can parse a point")
    {
      PARSE_AS("{1234,5678}", (Point2{.x = 1234, .y = 5678}), "");
    }
  }
}