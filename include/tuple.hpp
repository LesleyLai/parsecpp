#ifndef PARSECPP_TUPLE_HPP
#define PARSECPP_TUPLE_HPP

#include <type_traits>
#include <utility>

#if _MSC_VER
#define PARSEC_FORCE_INLINE __forceinline
#else
#define PARSEC_FORCE_INLINE inline __attribute__((always_inline))
#endif

namespace parsec {

namespace detail {

template <class... T> struct TupleStorage;

template <> struct TupleStorage<> {
};

template <class Head, class... Tail>
struct TupleStorage<Head, Tail...> : public TupleStorage<Tail...> {
  Head value{};

  constexpr TupleStorage() = default;
  explicit PARSEC_FORCE_INLINE constexpr TupleStorage(Head&& head, Tail&&... tail) noexcept(
      noexcept(TupleStorage<Tail...>(std::forward<Tail>(tail)...)) && noexcept(
          Head{std::forward<Head>(head)}))
      : TupleStorage<Tail...>(std::forward<Tail>(tail)...),
        value{std::forward<Head>(head)}
  {
  }

  template <std::size_t index>
  [[nodiscard]] PARSEC_FORCE_INLINE constexpr auto get() const noexcept -> const
      auto&
  {
    if constexpr (index == 0) {
      return value;
    } else {
      return TupleStorage<Tail...>::template get<index - 1>();
    }
  }
};

} // namespace detail

template <class... T> struct Tuple : public detail::TupleStorage<T...> {
  constexpr Tuple() noexcept = default;

  explicit constexpr PARSEC_FORCE_INLINE Tuple(T&&... values) noexcept(
      noexcept(detail::TupleStorage<T...>(std::forward<T>(values)...)))
      : detail::TupleStorage<T...>(std::forward<T>(values)...)
  {
  }
};

template <class... T> Tuple(T&&...) -> Tuple<T...>;

template <std::size_t index, class... T>
[[nodiscard]] constexpr PARSEC_FORCE_INLINE auto
get(const Tuple<T...>& t) noexcept -> const auto&
{
  return t.template get<index>();
}

template <class Tuple> struct tuple_size;

template <class... T>
struct tuple_size<Tuple<T...>>
    : std::integral_constant<std::size_t, sizeof...(T)> {
};

template <class Tuple>
static constexpr std::size_t tuple_size_v = tuple_size<Tuple>::value;

template <class F, class Tuple>
constexpr auto PARSEC_FORCE_INLINE apply(F&& f, Tuple&& t) -> decltype(auto)
{
  return
      [ f = std::forward<F>(f), t = std::forward<Tuple>(t) ]<std::size_t... I>(
          std::index_sequence<I...>)
  {
    return std::invoke(f, parsec::get<I>(std::forward<Tuple>(t))...);
  }
  (std::make_index_sequence<tuple_size_v<std::remove_cvref_t<Tuple>>>{});
}

namespace detail {
template <class... Ts, class T>
constexpr auto tp_impl(const Tuple<Ts...>& tuple, T&& v)
{
  return [&tuple,
          v = std::forward<T>(v) ]<std::size_t... I>(std::index_sequence<I...>)
  {
    return Tuple{std::remove_reference_t<decltype(parsec::get<I>(tuple))>(
                     parsec::get<I>(tuple))...,
                 T{v}};
  }
  (std::make_index_sequence<sizeof...(Ts)>{});
}
} // namespace detail

template <class Tuple, class T>
constexpr auto PARSEC_FORCE_INLINE tuple_push_back(Tuple&& tuple, T&& value)
    -> auto
{
  return detail::tp_impl(std::forward<Tuple>(tuple), std::forward<T>(value));
}

} // namespace parsec

#endif // PARSECPP_TUPLE_HPP
