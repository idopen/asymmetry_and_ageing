#ifndef CELL_H
#define CELL_H

#include "parameter_values.h"
#include "utils.h"
#include "randomnumbers.h"


class Cell{
    // Parameters for Cells
    inline static Cell_params p{};

    // Define aliases
    using reaction_norm = std::array<double, 2>;   // 2 cell types
    using genotype = std::array<reaction_norm, 4>; // repro, repair (forage=1-repro-repair), harvest, damage transmitted
    using v_genotype = std::bitset<p.n_v_loci>;

    genotype g{p.g_init};
    unsigned type{0};
    double resources{0.0};
    double damage{0.0};
    v_genotype vg{p.null_v_genotype};

public:
    // constructor with type
    Cell(const unsigned t) : type{t} {}

    // regular copy constructor
    Cell(const Cell& c) :
        g{c.g},
        type{c.type},
        resources{c.resources},
        damage{c.damage},
        vg{c.vg}
    {}

    // Special copy constructor used in reproduction (from mother cell c)
    // NB: resources are mom's investment in repro
    Cell(const Cell& c, const double&& damage) :
        g{c.g},
        type{0},
        resources{c.resources*logist3(c.g[0][c.type],c.g[1][c.type],0)},
        damage{std::move(damage)},
        vg{c.vg}
    {}

    // move constructor required because copy constructor defined
    // noexcept required or vector will use copy constructors instead
    Cell(Cell&& c) noexcept :
        g{c.g},
        type{c.type},
        resources{c.resources},
        damage{c.damage},
        vg{c.vg}
    {}

    // copy assignment operator
    Cell& operator=(const Cell& rhs){
        if (this != &rhs){
            g=rhs.g;
            type=rhs.type;
            resources=rhs.resources;
            damage=rhs.damage;
            vg=rhs.vg;
        }
        return *this;
    }

    // NB: move assignment disabled


    void set_type(const unsigned t) { type = t; }
    void set_resources(const double r) { resources = r; }

    double forage_returns(const bool concave) const {
        double alloc_forage{ resources * logist3(g[0][type],g[1][type],2) };
        if (concave) return p.forage_max * alloc_forage/(alloc_forage + p.q_f);
        double af_sqr = alloc_forage*alloc_forage;
        return p.forage_max * af_sqr/(af_sqr + p.q_f*p.q_f);
    }

    double get_harvest() const { return g[2][type]; }

    // convert damage to index for viability loci vector
    size_t d2i(const double d) const {
        if (d>p.d_max) return p.n_v_loci-1;
        else return static_cast<size_t>(floor(d*(p.n_v_loci-1)/p.d_max));
    }

    bool dies_ad() const {
        double x{damage/p.d0a};
        double p1{p.c*x+(1-p.c)*pow(x,4)};
        double p2{1-exp(-p.b_svg*vg[d2i(damage)])};
        double pr_dead{p1+(1-p1)*p2};
        return ru() < pr_dead;
    }

    bool dies_sol() const { return ru() < p.m0; }

    bool reproduces() const { return ru() < 1.0-exp(-p.b_r*logist3(g[0][type],g[1][type],0)*resources); }

    void mutate();
    void accumulate_damage() { damage += rexp(p.dd); } // could make less stochastic
    //void accumulate_damage() { damage += p.dd; }

    // additive repair (was multiplicative: damage*=exp(-...)
    void repair_damage() { damage *= exp(-p.b_re*logist3(g[0][type],g[1][type],1)*resources); }

    unsigned get_type() const { return type; }

    genotype get_g() const { return g; }
    v_genotype get_vg() const { return vg; }

    double get_damage() const { return damage; }
    void set_damage(const double d) { damage = d; }
    double damage_transmitted() const { return damage*logist(g[3][type]); }

    double get_resources() const { return resources; }
};



#endif // CELL_H
