#ifndef POLY_H
#define POLY_H

#include <cstddef>
#include <type_traits>
#include <concepts>
#include <algorithm>
#include <array>
#include <iostream> // debug

template <typename T, std::size_t N>
class poly;

namespace detail {
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

    template <std::size_t M, std::size_t N>
    concept leq = (M <= N);

}


namespace std {
    template <typename T, std::size_t N, typename U>
    struct common_type<poly<T, N>, U> {
        using type = poly<typename std::common_type<T, U>::type, N>;
    };

    template <typename U, typename T, std::size_t N>
    struct common_type<U, poly<T, N>> : std::common_type<poly<T, N>, U> {};

    template <typename T, std::size_t N, typename U, std::size_t M>
    struct common_type<poly<T, N>, poly<U, M>> {
        using type = poly<typename std::common_type<T, U>::type, static_cast<std::size_t>(std::max(N, M))>;
    };
}

template <typename T, std::size_t N = 0>
class poly {
    std::array<T, static_cast<std::size_t>(std::max(N, 1UL))> coefficients;

public:
    using type = T;

    constexpr poly(void) = default;

    constexpr T const& operator[](std::size_t i) const {
        return coefficients[i];
    }

    constexpr T& operator[](std::size_t i) {
        return coefficients[i];
    }

    template <std::convertible_to<T> U, std::size_t M>
    requires (detail::leq<M, N>)
    constexpr poly(poly<U, M> const& rhs) {
        std::cerr << "copy ctor called\n";
        for (std::size_t i = 0; i < M; ++i)
            coefficients[i] = rhs[i];
    }

    template <std::convertible_to<T> U, std::size_t M = 0>
    requires (detail::leq<M, N>)
    constexpr poly(poly<U, M>&& rhs) {
        std::cerr << "move ctor called\n";
        for (std::size_t i = 0; i < M; ++i)
            coefficients[i] = std::move(rhs[i]);
    }


    template <std::convertible_to<T> U>
    requires std::is_rvalue_reference_v<U&&>
    constexpr poly(U&& u) : coefficients{static_cast<T>(std::move(u))} {
        std::cerr << "move conversion ctor called\n";
    }

    template <std::convertible_to<T> U>
    constexpr poly(U const& u) : coefficients{static_cast<T>(u)} {
        //std::cerr << "copy conversion ctor called\n";
    }

    template <std::convertible_to<T>... Args>
    requires (detail::leq<sizeof...(Args), N> && detail::leq<2, sizeof...(Args)>)
    constexpr poly(Args&&... args) : coefficients{static_cast<T>(std::forward<Args>(args))...} {
        //std::cerr << "variadic ctor called\n";
    };

    template <std::convertible_to<T> U, std::size_t M = 0>
    requires (detail::leq<M, N>)
    constexpr poly<T, N>& operator=(poly<U, M> const& rhs) {
        //std::cerr << "copy assignment operator called\n";
        for (std::size_t i = 0; i < N; ++i)
            coefficients[i] = i < M ? rhs[i] : T();
        return *this;
    }

    template <std::convertible_to<T> U, std::size_t M = 0>
    requires (detail::leq<M, N>)
    constexpr poly<T, N>& operator=(poly<U, M>&& rhs) {
        //std::cerr << "move assignment operator called\n";
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

    constexpr std::size_t size() const {
        return N;
    }

//    template<typename = void>
//    auto at(Args... args) {
//
//    }
//
//    template <typename U, typename... Args>
//    auto at();

    template <typename U, std::size_t M>
    constexpr friend poly<poly<U, M>, 1> const_poly(poly<U, M> const& rhs);

};

template <typename T, typename U>
requires (detail::is_any_poly<T> || detail::is_any_poly<U>)
constexpr auto operator+(T const& lhs, U const& rhs) {
    using CommonType = std::common_type_t<std::decay_t<decltype(lhs)>, std::decay_t<decltype(rhs)>>;
    CommonType ret_val = detail::is_any_poly<T> ? std::forward<T const&>(lhs) : std::forward<U const&>(rhs);

    if constexpr (!detail::is_any_poly<T>)
        ret_val += lhs;
    else
        ret_val += rhs;
    return ret_val;
}

template <typename T, typename U>
requires (detail::is_any_poly<T> || detail::is_any_poly<U>)
constexpr auto operator-(T const& lhs, U const& rhs) {
    using CommonType = std::common_type_t<std::decay_t<decltype(lhs)>, std::decay_t<decltype(rhs)>>;
    CommonType ret_val = detail::is_any_poly<T> ? std::forward<T const&>(lhs) : std::forward<U const&>(rhs);

    if constexpr (!detail::is_any_poly<T>)
        ret_val -= lhs;
    else
        ret_val -= rhs;
    return ret_val;
}

template <typename T, std::size_t N = 0>
constexpr auto operator-(poly<T, N> const& p) {
    auto ret_val = p;
    ret_val *= -T();
    return ret_val;
}

template <typename T, typename U>
requires (detail::is_any_poly<T> || detail::is_any_poly<U>)
constexpr auto operator*(T const& lhs, U const& rhs) {
    using MulType = detail::mul_type_t<std::decay_t<decltype(lhs)>, std::decay_t<decltype(rhs)>>;
    MulType ret_val{};

    if constexpr (ret_val.size() != 0) {
         for (std::size_t i = 0; i < lhs.size(); ++i) {
             for (std::size_t j = 0; j < rhs.size(); ++j) {
                 if constexpr (std::convertible_to<decltype(rhs[j]), decltype(lhs[i])>) {
                     auto tmp = lhs[i];
                     tmp *= rhs[j];
                     ret_val[i + j] += tmp;
                 } else {
                     auto tmp = rhs[j];
                     tmp *= lhs[i];
                     ret_val[i + j] += tmp;
                 }
             }
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

template <typename... Args>
requires (sizeof...(Args) >= 1)
poly(Args&&...) -> poly<std::common_type_t<Args...>, sizeof...(Args)>;

#endif