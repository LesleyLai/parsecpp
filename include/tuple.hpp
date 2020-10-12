#ifndef PARSECPP_TUPLE_HPP
#define PARSECPP_TUPLE_HPP

#include <utility>

namespace parsec {

namespace detail {

template <class... T> class TupleStorage;

template <> struct TupleStorage<> {
};

template <class Head, class... Tail>
struct TupleStorage<Head, Tail...> : TupleStorage<Tail...> {
  Head value{};

  constexpr TupleStorage() = default;
  explicit constexpr TupleStorage(Head&& head, Tail&&... tail) noexcept(
      noexcept(TupleStorage<Tail...>(std::forward<Tail>(tail)...)) && noexcept(
          Head{std::forward<Head>(head)}))
      : TupleStorage<Tail...>(std::forward<Tail>(tail)...),
        value{std::forward<Head>(head)}
  {
  }

  template <std::size_t index>
  [[nodiscard]] constexpr auto get() const noexcept -> const auto&
  {
    if constexpr (index == 0) {
      return value;
    } else {
      return TupleStorage<Tail...>::template get<index - 1>();
    }
  }
};

} // namespace detail

template <class... T> struct Tuple : detail::TupleStorage<T...> {
  constexpr Tuple() noexcept = default;

  explicit constexpr Tuple(T&&... values) noexcept(
      noexcept(detail::TupleStorage<T...>(std::forward<T>(values)...)))
      : detail::TupleStorage<T...>(std::forward<T>(values)...)
  {
  }
};

template <std::size_t index, class... T>
[[nodiscard]] constexpr auto& get(const Tuple<T...>& t) noexcept
{
  return t.template get<index>();
}

} // namespace parsec

#endif // PARSECPP_TUPLE_HPP
