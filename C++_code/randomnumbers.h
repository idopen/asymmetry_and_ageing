#ifndef MILS_RANDOMNUMBERS_H
#define MILS_RANDOMNUMBERS_H

#include <random>
#include <Eigen/Eigenvalues> // needed for multivariate normal random vectors

extern std::mt19937 rng;

// initialize and seed engine; returns seed
unsigned long randomize();


// generalized uniform takes integral types and floating point types between a and b
// At compile time only one distribution is actually instantiated (C++17)
template <typename T>
T random_uniform(const T a, const T b) {
    if constexpr(std::is_integral<T>::value) {
        static std::uniform_int_distribution<T> d{a,b};
        return d(rng);
    }
    else {
        static std::uniform_real_distribution<T> d{a,b};
        return d(rng);
    }
}


// random uniform integral type (int, size_t, long, etc) [0,n) (not including n)
template <typename T>
T rn(const T n){
    static std::uniform_int_distribution<T> d{0,n-1};
    return d(rng);
}

// random uniform [0,1]
template <typename T = double>
T ru()
{
    static std::uniform_real_distribution<T> d{};
    return d(rng);
}

// randomly rounded floating point type to integral type
// Int obviously cannot be deduced, so has to be supplied if not int
template <typename Int = int, typename FP = double>
Int rf2i(FP x){
    FP intpart;
    auto y = modf(x,&intpart);
    if (ru<FP>()<y) return static_cast<Int>(intpart+1);
    return static_cast<Int>(intpart);
}

// special case of rf2i where x<=1 guaranteed
template <typename Int = int, typename FP = double>
Int rf2i1(FP x){
    if (ru<FP>()<x) return static_cast<Int>(1);
    return static_cast<Int>(0);
}


// random normal with default mean and sd
// when used with single argument sets sd (keeping mean=0)
template <typename T = double>
T rnorm(const T sd=1.0, const T mu=0.0){
    static std::normal_distribution<T> d{mu, sd};
    return d(rng);
}

// random bool
template <typename T = double>
bool r2(const T p=0.5){
    static std::bernoulli_distribution d{p};
    return d(rng);
}

// random Poisson integer
template <typename T = double>
int rpois(const T lambda){
    static std::poisson_distribution<T> d{lambda};
    return d(rng);
}

// random exponential
template <typename T = double>
T rexp(const T lambda){
    static std::exponential_distribution<T> d{lambda};
    return d(rng);
}


// templated functor for random index to vector of weights
// T1 type of vector content, T2 intType index
// be careful the reference to w doesn't expire!
template <typename T1 = double, typename T2 = size_t>
class ridx{
public:
    ridx(const std::vector<T1>& w) :
        w{w},
        d{w.begin(),w.end()}
    {}
    T2 operator()() {
        return d(rng);
    }
private:
    const std::vector<T1>& w;
    std::discrete_distribution<T2> d;
};


// multivariate normal (doubles)
// needs library Eigen
class rmvnorm{
public:
    rmvnorm(Eigen::VectorXd const& mean, Eigen::MatrixXd const& covar) : mean(mean) {
        Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> eigenSolver(covar);
        transform = eigenSolver.eigenvectors() * eigenSolver.eigenvalues().cwiseSqrt().asDiagonal();
    }

    rmvnorm(Eigen::MatrixXd const& covar)
        : rmvnorm(Eigen::VectorXd::Zero(covar.rows()), covar) {}

    Eigen::VectorXd operator()() const{
         return mean + transform * Eigen::VectorXd{ mean.size() }.unaryExpr([&](auto) { return rnorm(); });
    }

    Eigen::VectorXd mean;
    Eigen::MatrixXd transform;
};


#endif //MILS_RANDOMNUMBERS_H
