#ifndef POLY_H
#define POLY_H

#include <cstddef>
#include <type_traits>
#include <concepts>
#include <algorithm>
#include <array>

// Forward declaration
template <typename T, std::size_t N>
class poly;

namespace detail {

    // Polynomial type checking

    template <typename T>
    struct is_poly : public std::false_type
    {};

    template <typename T, std::size_t N>
    struct is_poly<poly<T, N>> : public std::true_type
    {};

    template <typename T>
    inline static constexpr bool is_poly_v = is_poly<T>::value;

    template <typename T>
    concept is_any_poly = detail::is_poly_v<std::decay_t<T>>;


    // Logic behind specialisation of std::common_type

    template <typename T, typename U>
    struct my_common_type : std::common_type<T, U> {};

    template <typename T, std::size_t N, typename U>
    struct my_common_type<poly<T, N>, U> {
        using type = poly<typename detail::my_common_type<T, U>::type, N>;
    };

    template <typename T, typename U, std::size_t N>
    struct my_common_type<T, poly<U, N>> : my_common_type<poly<U, N>, T> {};

    template <typename T, std::size_t N, typename U, std::size_t M>
    struct my_common_type<poly<T, N>, poly<U, M>> {
        using type = poly<typename detail::my_common_type<T, U>::type, static_cast<std::size_t>(std::max(N, M))>;
    };

    template <typename T, typename U>
    using my_common_type_t = typename detail::my_common_type<T, U>::type;


    // Concept to check ordering of two compile-time constants

    template <std::size_t M, std::size_t N>
    concept leq = (M <= N);


    // Logic behind evaluating the type of multiplication operation

    template <typename T, typename U>
    struct mul_type : std::common_type<T, U> {};

    template <typename T, typename U, std::size_t N>
    struct mul_type<T, poly<U, N>> {
        using type = poly<typename detail::mul_type<T, U>::type, N>;
    };

    template <typename T, std::size_t N, typename U>
    struct mul_type<poly<T, N>, U> : detail::mul_type<U, poly<T, N>>
    {};

    template <typename T, std::size_t N, typename U, std::size_t M>
    struct mul_type<poly<T, N>, poly<U, M>> {
        using type = poly<typename detail::mul_type<T, U>::type, 0>;
    };

    template <typename T, std::size_t N, typename U, std::size_t M>
    requires (N > 0 && M > 0)
    struct mul_type<poly<T, N>, poly<U, M>> {
        using type = poly<typename detail::mul_type<T, U>::type, N + M - 1>;
    };

    template <typename T, typename U>
    using mul_type_t = typename detail::mul_type<T, U>::type;


    // Logic behind evaluating the type of cross operation

    template <typename T, typename U>
    struct cross_type : std::common_type<T, U> {};

    template <typename T, std::size_t N, typename U, std::size_t M>
    struct cross_type<poly<T, N>, poly<U, M>> {
        using type = poly<typename detail::cross_type<T, poly<U, M>>::type, N>;
    };

    template <typename T, typename U>
    using cross_type_t = typename detail::cross_type<T, U>::type;
}

namespace std {
    // Custom specialisation of standard type trait common_type for polynomial types
    template <typename T, typename U>
    requires (detail::is_any_poly<T> || detail::is_any_poly<U>)
    struct common_type<T, U> : std::conditional<
                         std::is_same_v<T, typename std::decay_t<T>> && std::is_same_v<U, typename std::decay_t<U>>,
                         detail::my_common_type<T, U>,
                         detail::my_common_type<typename std::decay_t<T>, typename std::decay_t<U>>
                         >::type {};
}

// Forward declarations of binary and unary operators

template <typename T, typename U>
requires (detail::is_any_poly<T> || detail::is_any_poly<U>)
constexpr auto operator+(T const& lhs, U const& rhs);

template <typename T, typename U>
requires (detail::is_any_poly<T> || detail::is_any_poly<U>)
constexpr auto operator-(T const& lhs, U const& rhs);

template <typename T, std::size_t N>
constexpr auto operator-(poly<T, N> const& p);

template <typename T, typename U>
requires (detail::is_any_poly<T> || detail::is_any_poly<U>)
constexpr auto operator*(T const& lhs, U const& rhs);

template <typename T, std::size_t N = 0>
class poly {
    std::array<T, static_cast<std::size_t>(std::max(N, 1UL))> coefficients;

    // Helper methods for later use in at()
    template <std::size_t I, typename U>
    constexpr auto at_helper(U const& x) const {
        if constexpr (N == 0 || I == N - 1)
            return coefficients[I];
        else
            return coefficients[I] + x * at_helper<I + 1, U>(x);
    }

    template <typename U, std::size_t M, std::size_t... Args>
    constexpr auto at_array(std::array<U, M> const& arr, std::index_sequence<Args...> const&) const {
        return at(arr[Args]...);
    }

public:

    // Subscript operators

    constexpr T const& operator[](std::size_t i) const {
        return coefficients[i];
    }

    constexpr T& operator[](std::size_t i) {
        return coefficients[i];
    }

    // Constructors

    constexpr poly(void) : coefficients{} {}

    template <std::convertible_to<T> U, std::size_t M>
    requires (detail::leq<M, N>)
    constexpr poly(poly<U, M> const& rhs) : coefficients{} {
        for (std::size_t i = 0; i < M; ++i)
            coefficients[i] = rhs[i];
    }

    template <std::convertible_to<T> U, std::size_t M = 0>
    requires (detail::leq<M, N>)
    constexpr poly(poly<U, M>&& rhs) : coefficients{} {
        for (std::size_t i = 0; i < M; ++i)
            coefficients[i] = std::move(rhs[i]);
    }

    template <std::convertible_to<T> U>
    requires std::is_rvalue_reference_v<U&&>
    constexpr poly(U&& u) : coefficients{static_cast<T>(std::move(u))} {}

    template <std::convertible_to<T> U>
    constexpr poly(U const& u) : coefficients{static_cast<T>(u)} {}

    template <std::convertible_to<T>... Args>
    requires (detail::leq<sizeof...(Args), N> && detail::leq<2, sizeof...(Args)>)
    constexpr poly(Args&&... args) : coefficients{static_cast<T>(std::forward<Args>(args))...} {};

    // Operators

    template <std::convertible_to<T> U, std::size_t M = 0>
    requires (detail::leq<M, N>)
    constexpr poly<T, N>& operator=(poly<U, M> const& rhs) {
        for (std::size_t i = 0; i < N; ++i)
            coefficients[i] = i < M ? static_cast<T>(rhs[i]) : T();
        return *this;
    }

    template <std::convertible_to<T> U, std::size_t M = 0>
    requires (detail::leq<M, N>)
    constexpr poly<T, N>& operator=(poly<U, M>&& rhs) {
        for (std::size_t i = 0; i < N; ++i)
            coefficients[i] = i < M ? std::move(rhs[i]) : T();
        return *this;
    }

    template <std::convertible_to<T> U, std::size_t M = 0>
    requires (detail::leq<M, N>)
    constexpr poly<T, N>& operator+=(poly<U, M> const& rhs) {
        for (std::size_t i = 0; i < M; ++i)
            coefficients[i] += rhs[i];
        return *this;
    }

    template <std::convertible_to<T> U>
    constexpr poly<T, N>& operator+=(U const& rhs) {
        coefficients[0] += rhs;
        return *this;
    }

    template <std::convertible_to<T> U, std::size_t M = 0>
    requires (detail::leq<M, N>)
    constexpr poly<T, N>& operator-=(poly<U, M> const& rhs) {
        for (std::size_t i = 0; i < M; ++i)
            coefficients[i] -= rhs[i];
        return *this;
    }

    template <std::convertible_to<T> U>
    constexpr poly<T, N>& operator-=(U const& rhs) {
        coefficients[0] -= rhs;
        return *this;
    }

    template <std::convertible_to<T> U>
    constexpr poly<T, N>& operator*=(U const& rhs) {
        for (std::size_t i = 0; i < N; ++i)
            coefficients[i] *= rhs;
        return *this;
    }

    template <std::convertible_to<T> U>
    constexpr poly<T, N>& operator*=(poly<U, 1> const& rhs) {
        for (std::size_t i = 0; i < N; ++i)
          coefficients[i] *= rhs[0];
        return *this;
    }

    // Class logic

    constexpr std::size_t size() const {
        return N;
    }

    constexpr auto at() const {
        return *this;
    }

    template <typename U, typename... Args>
    constexpr auto at(U const& x, Args const&... args) const {
        if constexpr (!detail::is_poly_v<T>)
            return at_helper<0, U>(x);
        else
            return at_helper<0, U>(x).at(args...);
    }

    template <typename U, std::size_t M>
    constexpr auto at(std::array<U, M> const& arr) const {
        return at_array(arr, std::make_index_sequence<M>{});
    }

    template <typename U, std::size_t M>
    constexpr friend poly<poly<U, M>, 1> const_poly(poly<U, M> const& rhs);

}; // class poly

// Implementation of unary and binary operators

template <typename T, typename U>
requires (detail::is_any_poly<T> || detail::is_any_poly<U>)
constexpr auto operator+(T const& lhs, U const& rhs) {
    using res_type = std::common_type_t<std::decay_t<decltype(lhs)>, std::decay_t<decltype(rhs)>>;
    res_type ret_val{std::convertible_to<std::decay_t<decltype(rhs)>, std::decay_t<decltype(lhs)>>
                         ? static_cast<res_type>(std::forward<T const&>(lhs))
                         : static_cast<res_type>(std::forward<U const&>(rhs))};

    if constexpr (std::convertible_to<std::decay_t<decltype(rhs)>, std::decay_t<decltype(lhs)>>)
        ret_val += rhs;
    else
        ret_val += lhs;
    return ret_val;
}

template <typename T, typename U>
requires (detail::is_any_poly<T> || detail::is_any_poly<U>)
constexpr auto operator-(T const& lhs, U const& rhs) {
    using res_type = std::common_type_t<std::decay_t<decltype(lhs)>, std::decay_t<decltype(rhs)>>;
    res_type ret_val{std::forward<T const&>(lhs)};

    ret_val -= rhs;
    return ret_val;
}

template <typename T, std::size_t N = 0>
constexpr auto operator-(poly<T, N> const& p) {
    poly<T, N> ret_val{};
    ret_val -= p;
    return ret_val;
}

template <typename T, typename U>
requires (detail::is_any_poly<T> || detail::is_any_poly<U>)
constexpr auto operator*(T const& lhs, U const& rhs) {
    using res_type = detail::mul_type_t<std::decay_t<decltype(lhs)>, std::decay_t<decltype(rhs)>>;
    res_type ret_val{};

    if constexpr (ret_val.size() != 0) {
        if constexpr (detail::is_any_poly<T> && detail::is_any_poly<U>) {
            for (std::size_t i = 0; i < lhs.size(); ++i)
                for (std::size_t j = 0; j < rhs.size(); ++j)
                    ret_val[i + j] += lhs[i] * rhs[j];
        } else if constexpr (detail::is_any_poly<T>) {
            ret_val = lhs;
            ret_val *= rhs;
        } else {
            ret_val = rhs;
            ret_val *= lhs;
        }
     }
     return ret_val;
}

template <typename T, std::size_t N = 0>
constexpr poly<poly<T, N>, 1> const_poly(poly<T, N> const& rhs) {
    poly<poly<T, N>, 1> ret_val;
    ret_val.coefficients[0] = rhs;
    return ret_val;
}

template <typename T, typename U>
constexpr auto cross(T const& lhs, U const& rhs) {
    using res_type = detail::cross_type_t<std::decay_t<decltype(lhs)>, std::decay_t<decltype(rhs)>>;
    res_type ret_val{};

    if constexpr (detail::is_any_poly<T>) {
        for (std::size_t i = 0; i < lhs.size(); ++i)
            ret_val[i] += cross(lhs[i], rhs);
    } else {
        ret_val = lhs * rhs;
    }
    return ret_val;
}

// Deduction guide

template <typename... Args>
requires (sizeof...(Args) >= 1)
poly(Args&&...) -> poly<std::common_type_t<Args...>, sizeof...(Args)>;

#endif