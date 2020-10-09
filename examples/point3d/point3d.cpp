#include "parsec.hpp"

struct Point {
  int x = 0;
  int y = 0;
  int z = 0;

  constexpr friend auto operator==(const Point&, const Point&)
      -> bool = default;
};

int main()
{
  constexpr auto point_parser = parsec::pipe()
                                    .ignore(parsec::spaces)
                                    .ignore(parsec::character('('))
                                    .ignore(parsec::spaces)
                                    .keep(parsec::integer) // x
                                    .ignore(parsec::spaces)
                                    .ignore(parsec::character(','))
                                    .ignore(parsec::spaces)
                                    .keep(parsec::integer) // y
                                    .ignore(parsec::spaces)
                                    .ignore(parsec::character(','))
                                    .ignore(parsec::spaces)
                                    .keep(parsec::integer) // z
                                    .ignore(parsec::spaces)
                                    .ignore(parsec::character(')'))
                                    .map([](int x, int y, int z) {
                                      return Point{x, y, z};
                                    });
  constexpr auto result = point_parser("(1,  \n 2, 3) ");
  static_assert(result);
  static_assert(result->output == Point{1, 2, 3});
}