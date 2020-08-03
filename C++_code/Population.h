#ifndef POPULATION_H
#define POPULATION_H

#include "Individual.h"

class Statistics{
public:
    std::valarray<double> means{}; // pop means
    std::valarray<double> sds{};   // pop standard deviations
};

class Population{

    inline static Pop_params p{};
    Statistics stats;
    std::vector<Individual> moms;
    std::vector<Cell> gametes;

public:
    // initialize
    Population();

    // one cyle of foraging, reproduction, repair, survival
    void next_timestep();

    void calc_stats();
    void write_stats(std::ofstream&, size_t);
    void write_pop(std::ofstream&);

};

#endif // POPULATION_H
