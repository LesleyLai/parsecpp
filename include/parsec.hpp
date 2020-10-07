#ifndef PARSEC_HPP
#define PARSEC_HPP

#include <concepts>
#include <optional>
#include <string_view>
#include <type_traits>

namespace parsec {

template <class T> struct ParseOutput {
  using Output = T;

  Output output;
  std::string_view remaining;
};

template <class T> using MaybeParseResult = std::optional<ParseOutput<T>>;

// The type that the Parser tries to parse
template <class Parser>
using ParseType =
    typename std::invoke_result_t<Parser, std::string_view>::value_type::Output;

struct CharParser {
  char c = 0;
  [[nodiscard]] constexpr auto operator()(std::string_view s) const
      -> MaybeParseResult<char>
  {
    if (s.empty() || s.front() != c) {
      return std::nullopt;
    }
    s.remove_prefix(1);
    return ParseOutput<char>{.output = c, .remaining = s};
  }
};

/**
 * @brief Creates a parser that parses a single character
 * @param s The input string
 * @param c The character to match
 */
[[nodiscard]] constexpr auto character(char c) noexcept -> CharParser
{
  return CharParser{c};
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

} // namespace detail

struct IntParser {
  [[nodiscard]] constexpr auto operator()(std::string_view s) const
      -> MaybeParseResult<int>
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
      return ParseOutput<int>{.output = result, .remaining = s};
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
    return ParseOutput<int>{.output = result, .remaining = s};
  };
};

/**
 * @brief Creates a parser that parses an integer
 */
inline constexpr auto integer = IntParser{};

template <class Func, class Parser> struct MappedParser {
  Func func;
  Parser parser;

  using Output = std::invoke_result_t<Func, ParseType<Parser>>;
  using Ret = MaybeParseResult<Output>;
  static constexpr bool nothrow_invocable =
      std::is_nothrow_invocable_v<Func, ParseType<Parser>>;
  [[nodiscard]] constexpr auto operator()(std::string_view s) const
      noexcept(noexcept(parser(s)) && nothrow_invocable) -> Ret
  {
    const auto to_map = parser(s);
    if (!to_map) {
      return std::nullopt;
    }
    return Ret({func(to_map->output), to_map->remaining});
  }
};

/**
 * @brief Creates a parser that map another parser
 * @param func The function that maps the another parser's output
 * @param parser The parser to be transformed
 */
template <class Func, class Parser>
[[nodiscard]] constexpr auto map(Func&& func, Parser&& parser) noexcept
    -> MappedParser<Func, Parser>
{
  return MappedParser<Func, Parser>{.func = std::forward<Func>(func),
                                    .parser = std::forward<Parser>(parser)};
}

template <class Parser1, class Parser2>
requires(std::same_as<ParseType<Parser1>, ParseType<Parser2>>) struct OrParser {
  Parser1 p1;
  Parser2 p2;

  [[nodiscard]] constexpr auto operator()(std::string_view s) const
      noexcept(noexcept(p1(s)) && noexcept(p2(s)))
  {
    auto res = p1(s);
    return res ? res : p2(s);
  }
};

/**
 * @brief Creates a parser that tries the first parser, and if it fails, does
 * the second
 *
 * @note p1 and p2 must have the same return types.
 */
template <class Parser1, class Parser2>
requires(std::same_as<ParseType<Parser1>, ParseType<Parser2>>)
    [[nodiscard]] constexpr auto
    operator|(Parser1&& p1, Parser2&& p2) noexcept -> OrParser<Parser1, Parser2>
{
  return OrParser<Parser1, Parser2>{.p1 = std::forward<Parser1>(p1),
                                    .p2 = std::forward<Parser2>(p2)};
}

/**
 * @brief Creates a parser that pick `one_of` the parsers in sequence
 *
 * @note All the parsers must have the same return types.
 */
template <class... Parser>
[[nodiscard]] constexpr auto one_of(Parser&&... parser) noexcept
{
  // TODO: Try to enhance the error message of this
  return (... | std::forward<Parser>(parser));
}

} // namespace parsec

#endif // PARSEC_HPP
