#ifndef PARSEC_HPP
#define PARSEC_HPP

#include <algorithm>
#include <concepts>
#include <optional>
#include <ranges>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>

namespace parsec {

constexpr struct Monostate {
  [[nodiscard]] friend constexpr auto operator==(Monostate lhs,
                                                 Monostate rhs) noexcept
      -> bool = default;
} monostate;

template <class T> struct ParseOutput {
  using Value = T;
  Value value;
  std::string_view remaining;
};

template <class T> using ParseResult = std::optional<ParseOutput<T>>;

// clang-format off
template <class T> concept IsParseOutput = requires(T parse_output)
{
  { ParseOutput(parse_output) } ->std::same_as<T>;
};
// clang-format on

template <class T>
concept IsParseResult = IsParseOutput<typename T::value_type>;

template <class Func>
concept Parser = std::regular_invocable<Func, std::string_view>&&
    IsParseResult<std::invoke_result_t<Func, std::string_view>>;

// The type that the Parser tries to parse
template <Parser P>
using ParseType =
    typename std::invoke_result_t<P, std::string_view>::value_type::Value;

struct Char {
  char c = 0;
  [[nodiscard]] constexpr auto operator()(std::string_view s) const
      -> ParseResult<char>
  {
    if (s.empty() || s.front() != c) {
      return std::nullopt;
    }
    s.remove_prefix(1);
    return ParseOutput<char>{.value = c, .remaining = s};
  }
};

/**
 * @brief Creates a parser that matches a single character
 * @param c The character to match
 */
[[nodiscard]] constexpr auto character(char c) noexcept -> Char
{
  return Char{c};
}

struct OneOfChar {
  std::string_view chars;
  [[nodiscard]] constexpr auto operator()(std::string_view s) const
      -> ParseResult<char>
  {
    if (s.empty()) {
      return std::nullopt;
    }
    const auto match = std::ranges::find(chars.begin(), chars.end(), s.front());
    if (match == chars.end()) {
      return std::nullopt;
    }
    s.remove_prefix(1);
    return ParseOutput<char>{.value = *match, .remaining = s};
  }
};

/**
 * @brief Creates a parser that matches one of the characters in chars
 * @param A string that contains the characters to match
 */
[[nodiscard]] constexpr auto one_of_char(std::string_view chars) noexcept
{
  return OneOfChar{.chars = chars};
}

namespace detail {

[[nodiscard]] constexpr auto is_digit(char c) noexcept -> bool
{
  return c >= '0' && c <= '9';
}

[[nodiscard]] constexpr auto to_digit(char c) noexcept -> int
{
  return c - '0';
}

[[nodiscard]] constexpr auto is_whitespace(char c) noexcept -> bool
{
  return c == ' ' || c == '\t' || c == '\n';
}

} // namespace detail

/**
 * @brief A parser that parses an integer
 */
template <std::integral T>
[[nodiscard]] constexpr auto integer(std::string_view s) -> ParseResult<T>
{
  // TODO: Handle out of range
  if (s.empty()) {
    return std::nullopt;
  }

  // Parses the first character
  int result = 0;
  bool first_zero = false;
  {
    const char c = s[0];
    if (!detail::is_digit(c)) {
      return std::nullopt;
    } else if (c == '0') {
      first_zero = true;
    }
    result = detail::to_digit(c);
  }

  // Parses the second character
  if (s.size() == 1 || !detail::is_digit(s[1])) {
    s.remove_prefix(1);
    return ParseOutput<int>{.value = result, .remaining = s};
  } else if (first_zero) { // Octal
    return std::nullopt;
  }
  result = detail::to_digit(s[1]) + result * 10;

  // Parse the remaining characters
  std::size_t i = 2;
  for (; i < s.size(); ++i) {
    const char c = s[i];
    if (!detail::is_digit(c)) {
      break;
    }
    result = detail::to_digit(c) + result * 10;
  }

  s.remove_prefix(i);
  return ParseOutput<int>{.value = result, .remaining = s};
}

template <class Func, Parser P> struct MappedParser {
  Func func;
  P parser;

  using Output = std::invoke_result_t<Func, ParseType<P>>;
  using Ret = ParseResult<Output>;
  static constexpr bool nothrow_invocable =
      std::is_nothrow_invocable_v<Func, ParseType<P>>;
  [[nodiscard]] constexpr auto operator()(std::string_view s) const
      noexcept(noexcept(parser(s)) && nothrow_invocable) -> Ret
  {
    const auto to_map = parser(s);
    if (!to_map) {
      return std::nullopt;
    }
    return Ret({func(to_map->value), to_map->remaining});
  }
};

/**
 * @brief Creates a parser that map another parser
 * @param func The function that maps the another parser's output
 * @param parser The parser to be transformed
 */
template <class Func, Parser P>
[[nodiscard]] constexpr auto map(Func&& func, P&& parser) noexcept
    -> MappedParser<Func, P>
{
  return MappedParser<Func, P>{.func = std::forward<Func>(func),
                               .parser = std::forward<P>(parser)};
}

/**
 * @brief Creates a parser that tries the first parser, and if it fails, does
 * the second
 *
 * @note p1 and p2 must have the same return types.
 */
template <Parser P1, Parser P2>
requires(std::same_as<ParseType<P1>, ParseType<P2>>)
    [[nodiscard]] constexpr auto
    operator|(P1&& p1, P2&& p2) noexcept
{
  return [p1 = std::forward<P1>(p1), p2 = std::forward<P2>(p2)](
             std::string_view s) noexcept(noexcept(p1(s))&& noexcept(p2(s))) {
    auto res = p1(s);
    return res ? res : p2(s);
  };
}

/**
 * @brief Creates a parser that pick `one_of` the parsers in sequence
 *
 * @note All the parsers must have the same return types.
 */
template <Parser... P>
[[nodiscard]] constexpr auto one_of(P&&... parser) noexcept
{
  // TODO: Try to enhance the error message of this
  return (... | std::forward<P>(parser));
}

template <Parser P> struct Pipe {
  P p;

  template <class Func>
  [[nodiscard]] constexpr auto map(Func&& func) const noexcept
  {
    return [p = std::forward<P>(p),
            func = std::forward<Func>(func)](std::string_view s)
               -> ParseResult<decltype(std::apply(func, (p(s)->value)))> {
      const auto res = p(s);
      if (!res) {
        return std::nullopt;
      }
      return ParseOutput<decltype(std::apply(func, res->value))>{
          .value = std::apply(func, res->value), .remaining = res->remaining};
    };
  }

  template <Parser P2> [[nodiscard]] constexpr auto keep(P2 p2) const noexcept
  {
    using Out1 = ParseType<P>;
    using Out2 = ParseType<P2>;
    using Out = decltype(
        std::tuple_cat(std::declval<Out1>(), std::tuple{std::declval<Out2>()}));

    const auto result_parser =
        [p1 = std::forward<P>(p),
         p2 = std::forward<P2>(p2)](std::string_view s) -> ParseResult<Out> {
      auto res1 = p1(s);
      if (!res1) {
        return std::nullopt;
      }
      auto res2 = p2(res1->remaining);
      if (!res2) {
        return std::nullopt;
      }
      return ParseOutput<Out>{
          .value = std::tuple_cat(res1->value, std::tuple{res2->value}),
          .remaining = res2->remaining};
    };

    return Pipe<decltype(result_parser)>{result_parser};
  }

  template <Parser P2> [[nodiscard]] constexpr auto ignore(P2 p2) const noexcept
  {
    const auto result_parser =
        [p1 = std::forward<P>(p), p2 = std::forward<P2>(p2)](
            std::string_view s) -> ParseResult<ParseType<P>> {
      auto res1 = p1(s);
      if (!res1) {
        return std::nullopt;
      }
      auto res2 = p2(res1->remaining);
      if (!res2) {
        return std::nullopt;
      }
      return ParseOutput<decltype(res1->value)>{.value = res1->value,
                                                .remaining = res2->remaining};
    };
    return Pipe<decltype(result_parser)>{result_parser};
  }
};

[[nodiscard]] constexpr auto pipe()
{
  constexpr auto empty_parser =
      [](std::string_view s) -> parsec::ParseResult<std::tuple<>> {
    return parsec::ParseOutput<std::tuple<>>{.value = std::tuple<>{},
                                             .remaining = s};
  };

  return parsec::Pipe<decltype(empty_parser)>{empty_parser};
};

/**
 * @brief Creates a parser that chomp zero or more characters if they pass the
 * test
 */
template <std::predicate<char> Pred>
[[nodiscard]] constexpr auto chomp_while(Pred&& pred)
{
  return [pred = std::forward<Pred>(pred)](
             const std::string_view s) -> ParseResult<Monostate> {
    auto remaining = s;
    for (const char c : s) {
      if (!pred(c)) {
        break;
      }
      remaining.remove_prefix(1);
    }

    return ParseOutput<Monostate>{.value = monostate, .remaining = remaining};
  };
}

/**
 * @brief Parse zero or more ' ', '\n', and '\r' characters.
 */
constexpr auto spaces = chomp_while(detail::is_whitespace);

} // namespace parsec

#endif // PARSEC_HPP
