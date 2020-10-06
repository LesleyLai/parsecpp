#ifndef PARSEC_HPP
#define PARSEC_HPP

#include <optional>
#include <string_view>
#include <utility>

namespace parsec {

template <class T>
using ParseResult = std::optional<std::pair<T, std::string_view>>;

// The type that the Parser tries to parse
template <class Parser>
using ParseType =
    typename std::invoke_result_t<Parser,
                                  std::string_view>::value_type::first_type;

struct CharParser {
  char c = 0;
  [[nodiscard]] constexpr auto operator()(std::string_view s) const
      -> ParseResult<char>
  {
    if (s.empty() || s.front() != c) {
      return std::nullopt;
    }
    s.remove_prefix(1);
    return std::make_pair(c, s);
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

template <class Func, class Parser> struct MappedParser {
  Func func;
  Parser parser;

  using Ret = ParseResult<std::invoke_result_t<Func, ParseType<Parser>>>;
  [[nodiscard]] constexpr auto operator()(std::string_view s) const -> Ret
  {
    const auto to_map = parser(s);
    if (!to_map) {
      return std::nullopt;
    }
    return Ret(std::in_place, func(to_map->first), to_map->second);
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

} // namespace parsec

#endif // PARSEC_HPP
