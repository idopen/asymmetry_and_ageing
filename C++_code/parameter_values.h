#ifndef PARAMETER_VALUES_H
#define PARAMETER_VALUES_H

#include <array>
#include <bitset>

struct Cell_params{
    using reaction_norm = std::array<double,2>;
    constexpr static reaction_norm init_reprod{0.0,0.0};
    constexpr static reaction_norm init_repair{0.0,0.0};
    // proportion resources for foraging: 1-reprod-repair
    constexpr static reaction_norm init_harvest{1.0,1.0}; // (relative) effort into harvesting foraged resources
    constexpr static reaction_norm init_damalloc{0.0,0.0}; // damage equally distributed at first
    constexpr static std::array<reaction_norm,4> g_init{init_reprod,init_repair,init_harvest,init_damalloc};

    // mutations
    constexpr static double mu{0.05};
    constexpr static double mu_v{0.001}; // viability loci 0->1
    constexpr static double pr_back{0.1}; // rel. prob 1->0: abs pr =mu_v*pr_back
    constexpr static double mu_t{0.05}; // for damage transmission locus
    constexpr static double sd_mu{0.5};
    constexpr static double mu_h{0.05}; // for harvest (division of resources between cells)

    // forage parameters
    constexpr static double forage_max{6.0};
    constexpr static double q_f{0.3}; // half-max
    constexpr static bool type_2{true}; // else (partially) convex type III

    // repro params
    // rate at which Pr(repro) approaches 1 with amount of resources 1-exp(-b*res)
    constexpr static double b_r{0.1};

    // mortality parameters
    // adult cell mortality = c*(d/d0)+(1-c)*(d/d0)^4
    constexpr static double d0a{1.0};
    constexpr static double c{0.2};
    // adult mortality when solitary for a bit:
    constexpr static double m0{0.0};

    // delta damage (lambda of exponential distribution; mean=1/dd)
    constexpr static double dd{2.0};
    // damage repair rate constant
    constexpr static double b_re{0.5};

    // viability loci stuff

    // nr of bi-allelic loci
    constexpr static unsigned n_v_loci{16};
    constexpr static std::bitset<n_v_loci> null_v_genotype = 0;
    // extra mort round 1-exp(-b_svg*I(1))
    constexpr static double b_svg{0.05}; // here
    // max damage to which max-damage allele corresponds
    constexpr static double d_max{1.0};
};

struct Pop_params{
    constexpr static unsigned long size{100000};

    // cell resources in generation 0
    constexpr static double init_resources{1.0};

};

// nr of traits to calc stats for
constexpr static unsigned n_traits{45};
constexpr static unsigned long n_timesteps{10000};
constexpr static unsigned skip{10};


#endif // PARAMETER_VALUES_H
