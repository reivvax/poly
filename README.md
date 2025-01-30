# Polynomials

This project implements a class template for handling rings of multivariate polynomials. The core component is the following class template:
```cpp
template <typename T, std::size_t N = 0> class poly;
```
An object of this class represents a polynomial in a single variable:
$$a_0 + a_1 x + a_2 x^2 + … + a_{N-1} x^{N-1}$$
where the coefficients $a_i$ are of type T. The value N represents the size of the polynomial (to avoid referring to it as "degree + 1"). The type T is required to form a ring, meaning it must support the binary operations +, -, *, as well as the unary -, and its default constructor should yield the additive identity (zero).

To extend the implementation to multivariate polynomials, the type T itself can be another poly. In general, a polynomial of m variables is represented recursively using the poly class up to depth m, with the innermost coefficient type no longer being a poly.

Compile with `-std=c++20`.

# Example:
```cpp
poly<poly<poly<int, 3>, 2>, 4>
```
represents a polynomial in three variables over the integer type. Conceptually, a polynomial in m variables is structured as follows:
- The outermost definition represents a polynomial in $x_1$, with coefficients being polynomials in variables $x_2$ through $x_m$.
- The coefficient type of this polynomial is itself a polynomial in $x_2$, with coefficients being polynomials in variables $x_3$ through $x_m$, and so on.

This recursive approach enables the representation and manipulation of polynomials with multiple variables in a structured and efficient manner.

# Implemented Features
### Constructors
- A default constructor initializes a polynomial identically equal to zero.
- Copy and move constructors allow initialization from another `poly<U, M>` where M ≤ N and U is convertible to T.
- A conversion constructor creates a polynomial of size 1 from a type convertible to T.
- A variadic constructor initializes the polynomial using up to N coefficients with perfect forwarding.
- The const_poly function constructs a polynomial of size 1 where the sole coefficient is another polynomial.
- Deduction guides enable template argument inference for poly objects.
### Assignment Operators
- Copy and move assignment operators are implemented to support `poly<U, M>` where M ≤ N and U is convertible to T.
### Arithmetic Operators
- Supports +=, -=, and *= with both polynomials and scalars.
- Implements +, -, * in binary form and unary -.
- Type deduction ensures the result has the minimal required size and a coefficient type of `std::common_type_t<U, V>`.
### Indexing and Evaluation
- `operator[](std::size_t i)`: Returns a reference to the coefficient $a_i$.
- `at(...)`: Evaluates the polynomial as a function of multiple variables, supporting different argument counts and `std::array<U, K>` inputs.
### Utility Methods
- `size()`: Returns the polynomial's size.
### Cross Multiplication
- `cross(p, q)`: multiplying two polynomials p and q of different variables, producing a new polynomial in the combined variable set.
