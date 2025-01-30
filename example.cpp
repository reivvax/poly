#include "poly.h"
#include <cstddef>
#include <array>
#include <type_traits>

namespace
{
  template <typename T, std::size_t N>
  constexpr bool operator==(const poly<T, N>& a, const poly<T, N>& b)
  {
    for (std::size_t i = 0; i < N; ++i)
      if (!(a[i] == b[i]))
        return false;
    return true;
  }

  void test_static()
  {
    constexpr auto p = poly(2, 1);
    static_assert(std::is_same_v<decltype(p), const poly<int, 2>>);
    static_assert(p[0] == 2);
    static_assert(p.at(1) == 3);
    static_assert(p.at(0.5) == 2.5);
    static_assert(p.at(p) == poly(4, 1));

    static_assert(std::is_same_v<std::common_type_t<double, poly<int, 2>>, poly<double, 2>>);
    static_assert(std::is_same_v<std::common_type_t<poly<int, 2>, poly<double, 2>>, poly<double, 2>>);

    constexpr auto q = poly(poly(1.0, 2.0), 3.0, 4.0);
    static_assert(std::is_same_v<decltype(q), const poly<poly<double, 2>, 3>>);
    static_assert(q[0][0] == 1.0);
    static_assert(q[0][1] == 2.0);
    static_assert(q[1][0] == 3.0);
    static_assert(q[1][1] == 0.0);
    static_assert(q[2][0] == 4.0);
    static_assert(q[2][1] == 0.0);
    static_assert(q.at() == q);
    static_assert(q.at(2.0, 3.0) == 29.0);
    static_assert(q.at(2.0, 3.0, 4.0) == 29.0);
    static_assert(q.at(2.0) == poly(23.0, 2.0));
    static_assert(q.at(2.0).at(3.0) == q.at(2.0, 3.0));
    static_assert(q.at(p) == poly(23.0, 21.0, 4.0, 0.0));
    static_assert(q.at(-1, p) == poly(6.0, 2.0));
    static_assert(q.at(std::array{2.0, 3.0}) == 29.0);

    static_assert(std::is_same_v<decltype(p + q), poly<poly<double, 2>, 3>>);
    static_assert(p + q == poly(poly(3.0, 2.0), 4.0, 4.0));
    static_assert(-q == poly(poly(-1.0, -2.0), -3.0, -4.0));
    static_assert(p - q == poly(poly(1.0, -2.0), -2.0, -4.0));
    static_assert(q - p == poly(poly(-1.0, 2.0), 2.0, 4.0));
    static_assert(std::is_same_v<decltype(p * q), poly<poly<double, 2>, 4>>);
    static_assert(p * q == poly(poly(2.0, 4.0), poly(7.0, 2.0), 11.0, 4.0));
    static_assert(cross(p, q) == poly(2.0 * q, q));
    static_assert(cross(q, p) == poly(poly(p, 2.0 * p), const_poly(3.0 * p), const_poly(4.0 * p)));

    constexpr auto pp(p);
    static_assert(p == pp);
    constexpr auto qq(q);
    static_assert(q == qq);
  }

  template <typename T>
  class weak_wrapper
  {
  private:
    T a;
  public:
    weak_wrapper() {}
    weak_wrapper(T a) : a(a) {}
    weak_wrapper& operator-=(weak_wrapper b) {a -= b.a; return *this;}
    weak_wrapper& operator+=(weak_wrapper b) {a += b.a; return *this;}
    weak_wrapper& operator*=(weak_wrapper b) {a *= b.a; return *this;}
    template <typename U>
    friend weak_wrapper<U> operator*(weak_wrapper<U> a, weak_wrapper<U> b) {return a.a * b.a;}
  };

  void test_weak()
  {
    using t = weak_wrapper<int>;
    poly(t(2)) * t(3);
    poly(t(2)) + poly(t(3));
  }
}

int main()
{
  test_static();
  test_weak();
}
