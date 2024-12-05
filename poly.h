#ifndef POLY_H
#define POLY_H

#include <cstddef>
#include <type_traits>
#include <array>
#include <algorithm>

template <typename T, std::size_t N>
class poly;


//T = poly<U, M>& || T = poly<U, M>&&
namespace detail {

    template <typename T>
    struct is_poly : public std::false_type
    {};

    // JP specjalizacja
    template <typename T, std::size_t N>
    struct is_poly<poly<T, N>> : public std::true_type
    {};

    template <typename T>
    constexpr bool is_poly_v = is_poly<T>::value;

    template <typename T>
    concept is_poly_ref = std::is_reference_v<T> && detail::is_poly_v<std::remove_reference_t<T>>;

    template <typename T>
    concept is_const_lref = std::is_lvalue_reference_v<T> && std::is_const_v<std::remove_reference_t<T>>;

    template <typename T>
    concept is_good_poly_ref = detail::is_poly_ref<T> && (detail::is_const_lref<T> || std::is_rvalue_reference_v<T>);

    template <typename T>
    concept is_coefficient = requires (T x, T y)
    {
        {x + y} -> std::same_as<T>;
        {x - y} -> std::same_as<T>;
        {x * y} -> std::same_as<T>;
        {-x} -> std::same_as<T>;
//        {T() == T{}} -> std::convertible_to<bool>; // JP
    };

    template <typename T>
    concept is_at_return_t = true; // JP

    template <std::size_t M, std::size_t N>
    concept leq = M <= N;

    template <typename U, typename T>
    concept rref_convertible_to = std::is_rvalue_reference_v<U&&> && std::convertible_to<std::decay_t<U>, T>;


    template<typename T>
    struct default_val {
         constexpr static T value = T();
    };

    template <typename T>
    requires std::is_arithmetic_v<T>
    struct default_val<T> {
        constexpr static T value = static_cast<T>(0);
    }; // JP

    /*template <typename U, typename T>
    concept rref_convertible_to = std::is_rvalue_reference_v<U> && std::convertible_to<std::remove_cvref_t<U>, T>*/  // JP dlaczego nie dziala

}



template <typename T, std::size_t N = 0>
class poly {
private:
    std::array<T, static_cast<std::size_t>(std::max(N, 1UL))> coefficients;

public:
    // konstruktory
    /*constexpr poly(void) {
        static_assert(coefficients.size() == 1UL);
        // coefficients[0] = T();
    }*/
   constexpr poly(void) = default;

    template <std::convertible_to<T> U, std::size_t M = 0>
    requires detail::leq<M, N>
    constexpr poly(poly<U, M> const& rhs) {
        for (std::size_t i = 0; i < N; ++i)
            coefficients[i] = i < M ? rhs.coefficients[i] : detail::default_val<T>::value; // loop
    }
    // JP deduction guides (-> poly<std::common_type_t<T, U>, M>)?

    template <std::convertible_to<T> U, std::size_t M = 0>
    requires detail::leq<M, N>
    constexpr poly(poly<U, M>&& rhs) {
        for (std::size_t i = 0; i < N; ++i)
            coefficients[i] = i < M ? std::move(rhs.coefficients[i]) : detail::default_val<T>::value;
    } // JP to samo

    template <std::convertible_to<T> U>
    constexpr poly(U u) : coefficients(std::array<std::common_type_t<T, U>, 1>{u}) {} // JP to samo


    template <typename... Args>
    requires (sizeof...(Args) <= N && (detail::rref_convertible_to<Args, T> && ...))
    constexpr poly(Args&&... args) : coefficients{std::forward<Args>(args)...} {}




    // poly<poly<int, 1>> p(poly(1)); uniemozliwic zrobienie czegos takiego


    template <std::convertible_to<T> U, std::size_t M = 0>
    constexpr poly& operator=(poly<U, M> const& rhs) requires detail::leq<M, N> {
        /*coefficients = rhs.coefficients;
        return *this;*/
        for (std::size_t i = 0; i < N; ++i)
          coefficients[i] = i < M ? rhs.coefficients[i] : detail::default_val<T>::value;
        return *this;
    }

    template <std::convertible_to<T> U, std::size_t M = 0>
    constexpr poly& operator=(poly<U, M>&& rhs) requires detail::leq<M, N> {
        for (std::size_t i = 0; i < N; ++i)
            coefficients[i] = i < M ? std::move(rhs.coefficients[i]) : detail::default_val<T>::value; // T()
        return *this;
    }


    // arytmetyka
    /*template <std::convertible_to<T> U, std::size_t M = 0>
    constexpr poly& operator+=(poly<U, M> const& rhs);*/

    template <std::convertible_to<T> U, std::size_t M = 0>
    requires (M <= N) && requires (T t, U u) {
        t += u;
    }
    constexpr poly& operator+=(poly<U, M> const& rhs) {
        for (std::size_t i = 0; i < M; ++i)
            coefficients[i] += rhs.coefficients[i];
        return *this;
    }

    template <std::convertible_to<T> U>
    constexpr poly& operator+=(U const& rhs) {
        coefficients[0] += rhs;
        return *this;
    }

    template <std::convertible_to<T> U, std::size_t M = 0>
    constexpr poly& operator-=(poly<U, M> const& rhs) {
        for (std::size_t i = 0; i < M; ++i)
            coefficients[i] -= rhs.coefficients[i];
        return *this;
    }

    template <std::convertible_to<T> U>
    constexpr poly& operator-=(U const& rhs) {
        coefficients[0] -= rhs;
        return *this;
    }

    // merge those declarations with those above with some concepts

    template <std::convertible_to<T> U, std::size_t M = 0>
    constexpr poly& operator*=(poly<U, M> const& rhs);

    template <std::convertible_to<T> U>
    constexpr poly& operator*=(U const& rhs);

    // operatory
    /*constexpr poly operator+(poly const& lhs, poly const& rhs);

    constexpr poly operator-(poly const& lhs, poly const& rhs);

    constexpr poly operator*(poly const& lhs, poly const& rhs);

    constexpr poly operator-(poly const& rhs);*/

    // working version; possibly replace with concept
    template <std::size_t I>
    requires detail::leq<I, N>
    constexpr T const& operator[](std::size_t i) const {
        return coefficients[i];
    }

    // working version; possibly replace with concept; possibly remove const from signature
//    template <std::size_t I>
//    requires detail::leq<I, N>
//    constexpr T& operator[](std::size_t i) const {
//        return coefficients[i];
//    }

    constexpr T const& operator[](std::size_t index) const {
        return coefficients[index];
    }

    constexpr T& operator[](std::size_t index) {
        return coefficients[index];
    } // JP


    // logika
    constexpr size_t size() {
        return N;
    }


    poly cross(poly const& p, poly const& q);

    template <typename U, std::size_t M>
    constexpr friend poly<poly<U, M>, 1> const_poly(poly<U, M> const& rhs);

};

template <typename T, std::size_t N = 0>
constexpr poly<poly<T, N>, 1> const_poly(poly<T, N> const& rhs) {
    poly<poly<T, N>, 1> tmp;
    tmp.coefficients[0] = rhs;
    return tmp;
}


template <typename... Args>
requires (sizeof...(Args) >= 2)
poly(Args&&...) -> poly<std::common_type_t<Args...>, sizeof...(Args)>;

#endif