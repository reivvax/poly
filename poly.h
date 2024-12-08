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
}

namespace std {
    // template <typename T, std::size_t N, typename U>
    // struct common_type<poly<T, N>, U> {
    //     using type = poly<typename std::common_type<T, U>::type, N>;
    // };

    // template <typename U, typename T, std::size_t N>
    // struct common_type<U, poly<T, N>> : std::common_type<poly<T, N>, U> {};


    // template <typename T, std::size_t N, typename U, std::size_t M>
    // struct common_type<poly<T, N>, poly<U, M>> {
    //     using type = poly<typename std::common_type<std::decay_t<T>, std::decay_t<U>>::type, static_cast<std::size_t>(std::max(N, M))>;
    // };
    template <typename T, typename U>
    requires (detail::is_any_poly<T> || detail::is_any_poly<U>)
    struct common_type<T, U> : std::conditional<std::is_same_v<T, typename std::decay_t<T>> && std::is_same_v<U, typename std::decay_t<U>>,
        detail::my_common_type<T, U>, detail::my_common_type<typename std::decay_t<T>, typename std::decay_t<U>>>::type {};
}

namespace detail {

    template <std::size_t M, std::size_t N>
    concept leq = (M <= N);

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

    template <typename T, typename U>
    struct cross_type : std::common_type<T, U> {};

    template <typename T, std::size_t N, typename U, std::size_t M>
    struct cross_type<poly<T, N>, poly<U, M>> {
        using type = poly<typename detail::cross_type<T, poly<U, M>>::type, N>;
    };

    template <typename T, typename U>
    using cross_type_t = typename detail::cross_type<T, U>::type;



    template <typename T, typename U, std::size_t M>
    struct pow_type {
        using type = T;
    };

    template <typename T, typename U, std::size_t M>
    requires (leq<2, M>)
    struct pow_type<T, U, M> : pow_type<mul_type_t<T, U>, U, M - 1> {};

    template <typename T, typename U, std::size_t M>
    using pow_type_t = typename pow_type<T, U, M>::type;

    // template <typename T>
    // struct coeff_type {
    //     using type = T;
    // };

    // template <typename T, std::size_t N>
    // struct coeff_type<poly<T, N>> {
    //     using type = T;
    // };

    // template <typename T>
    // using coeff_type_t = typename coeff_type<T>::type;

    // template <typename T>
    // constexpr static std::size_t arg_size = 1;

    // template <typename T, std::size_t N>
    // constexpr static std::size_t arg_size<poly<T, N>> = N;


    template <typename T, typename... Args>
    struct at_type {
        using type = T;
    };

    // template <typename T, std::size_t N, typename U>
    // struct at_type<poly<T, N>, U> : mul_type<pow_type_t<U, U, N - 1>, T> {};

    template <typename T, std::size_t N, typename U>
    struct at_type<poly<T, N>, U> : mul_type<U, T> {};

    template <typename T, std::size_t N, typename U, std::size_t M>
    struct at_type<poly<T, N>, poly<U, M>> {
        using type = poly<mul_type_t<T, pow_type_t<U, U, N - 1>>, (M - 1) * (N - 1) + 1>;
    };

    //at_type<poly<T, N>, Args...> -> typ wywołania at na wielomianie typu poly<T, N> z argumentami Args...
    // template <typename T, std::size_t N, typename U, typename... Args>
    // struct at_type<poly<T, N>, U, Args...> : mul_type<pow_type_t<U, U, N - 1>, typename at_type<T, Args...>::type> {};

    template <typename T, std::size_t N, typename U, typename... Args>
    struct at_type<poly<T, N>, U, Args...> : at_type<T, Args...> {};

    template <typename T, std::size_t N, typename U, std::size_t M, typename... Args>
    struct at_type<poly<T, N>, poly<U, M>, Args...> {
        using type = mul_type_t<pow_type_t<poly<U, M>, poly<U, M>, N - 1>, typename at_type<T, Args...>::type>;
    };

    template <typename T, typename... Args>
    using at_type_t = typename at_type<T, Args...>::type;



}

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
    constexpr poly(poly<U, M> const& rhs) : coefficients{} {
        //std::cerr << "copy ctor called\n";
        for (std::size_t i = 0; i < M; ++i)
            coefficients[i] = rhs[i];
    }

    template <std::convertible_to<T> U, std::size_t M = 0>
    requires (detail::leq<M, N>)
    constexpr poly(poly<U, M>&& rhs) : coefficients{} {
        //std::cerr << "move ctor called\n";
        for (std::size_t i = 0; i < M; ++i)
            coefficients[i] = std::move(rhs[i]);
    }


    template <std::convertible_to<T> U>
    requires std::is_rvalue_reference_v<U&&>
    constexpr poly(U&& u) : coefficients{static_cast<T>(std::move(u))} {
        //std::cerr << "move conversion ctor called\n";
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

    template <typename C, typename... Args>
    constexpr auto evaluate_coefficient(C const& coeff, Args const&... args) const {
        if constexpr (detail::is_poly_v<C>) {
            return coeff.at(args...);
        } else {
            return coeff;
        }
    }
    /*
x -> poly<int, 2>
N -> 2
x^{N-1} -> poly<int, 2>
coeff[N - 1] -> int



mul_type<U, V, M> -> mul_type<mul_type<U, V>, V, M - 1> -> z tego mamy typ x^{N-1} = P (mul_type<V, V, N - 1>)
mul_type<U, V, 1> -> V

at_type<poly<T, N>, U, M> -> at_type<mul_type<poly<T, N>, U>, U, M - 1> -  deprecated

potem mul_type<P, at_type<T,

template <typename T, typename... Args>
at_type = T


template <typename T, std::size_t N, typename U>
at_type<poly<T, N>, U> = mul_type<mul_type<U, U, N - 1>, T>

//at_type<poly<T, N>, Args...> -> typ wywołania at na wielomianie typu poly<T, N> z argumentami Args...
template <typename T, std::size_t N, typename U, typename... Args>
at_type<poly<T, N>, U, Args...> = mul_type<mul_type<U, U, N - 1>, at_type<T, Args...>>


x^{N-1} * coeff[N - 1].at(args)
*/
    /*template <typename U, typename... Args>
    constexpr auto at(U const& first, Args const&... args) const {
        using ResultType = detail::mul_type_t<U, decltype(evaluate_coefficient(coefficients[size() - 1], args...))>;
        ResultType res_val = evaluate_coefficient(coefficients[size() - 1], args...);

//        auto res_val = res_val * first + evaluate_coefficient(coefficients[i], args...);
        auto res_val =
        return res_val;
    }

    constexpr auto at(void) const {
        return *this;
    }*/

    template <typename U, std::size_t M>
    constexpr friend poly<poly<U, M>, 1> const_poly(poly<U, M> const& rhs);


};

template <typename T, typename U>
requires (detail::is_any_poly<T> || detail::is_any_poly<U>)
constexpr auto operator+(T const& lhs, U const& rhs) {
    using CommonType = std::common_type_t<std::decay_t<decltype(lhs)>, std::decay_t<decltype(rhs)>>;
    // CommonType ret_val{detail::is_any_poly<T> ? std::forward<T const&>(lhs) : std::forward<U const&>(rhs)};
    CommonType ret_val{std::convertible_to<std::decay_t<decltype(rhs)>, std::decay_t<decltype(lhs)>> ? std::forward<T const&>(lhs) : std::forward<U const&>(rhs)};

    if constexpr (std::convertible_to<std::decay_t<decltype(rhs)>, std::decay_t<decltype(lhs)>>)
        ret_val += rhs;
    else
        ret_val += lhs;
    return ret_val;
}

template <typename T, typename U>
requires (detail::is_any_poly<T> || detail::is_any_poly<U>)
constexpr auto operator-(T const& lhs, U const& rhs) {
    using CommonType = std::common_type_t<std::decay_t<decltype(lhs)>, std::decay_t<decltype(rhs)>>;
    //CommonType ret_val{detail::is_any_poly<T> ? std::forward<T const&>(lhs) : std::forward<U const&>(rhs)};
    //CommonType ret_val{std::convertible_to<std::decay_t<decltype(rhs)>, std::decay_t<decltype(lhs)>> ? std::forward<T const&>(lhs) : std::forward<U const&>(rhs)};
    CommonType ret_val{std::forward<T const&>(lhs)};

    /*if constexpr (std::convertible_to<std::decay_t<decltype(rhs)>, std::decay_t<decltype(lhs)>>)
        ret_val -= rhs;
    else
        ret_val -= lhs;*/
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
//     constexpr auto&& ret_val = detail::mul_type_t<std::decay_t<decltype(lhs)>, std::decay_t<decltype(rhs)>>();
    using MulType = detail::mul_type_t<std::decay_t<decltype(lhs)>, std::decay_t<decltype(rhs)>>;
    MulType ret_val{};

    if constexpr (ret_val.size() != 0) {
        if constexpr (detail::is_any_poly<T> && detail::is_any_poly<U>) {
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

// template <typename T, std::size_t N, typename U, std::size_t M>
// constexpr auto cross(poly<T, N> const& lhs, poly<U, M> const& rhs) {
//     using CrossType = detail::cross_type_t<std::decay_t<decltype(lhs)>, std::decay_t<decltype(rhs)>>;
//     CrossType ret_val{};
//     return ret_val;
// }

template <typename T, typename U>
constexpr auto cross(T const& lhs, U const& rhs) {
    using CrossType = detail::cross_type_t<std::decay_t<decltype(lhs)>, std::decay_t<decltype(rhs)>>;
    CrossType ret_val{};

    if constexpr (detail::is_any_poly<T>) {
        for (int i = 0; i < lhs.size(); ++i)
            ret_val[i] += cross(lhs[i], rhs);
    } else {
        ret_val = lhs * rhs;
    }
    return ret_val;
}

// template <typename T, std::size_t N = 0>
// poly(poly<T, N> const& rhs) -> poly<T, N>;

// template <typename T, std::size_t N = 0>
// poly(poly<T, N>&& rhs) -> poly<T, N>;

template <typename... Args>
requires (sizeof...(Args) >= 1)
poly(Args&&...) -> poly<std::common_type_t<std::decay_t<Args>...>, sizeof...(Args)>;

#endif