#ifndef INDIVIDUAL_H
#define INDIVIDUAL_H

#include "Cell.h"
#include <valarray>

class Individual{
    // Parameters for Cells
    inline static Cell_params p{};

    std::array<Cell,2> body;
    unsigned age{0};
    double resources{0};

public:
    // construct from two cells (rvals)
    Individual(Cell&& c0, Cell&& c1) : body{std::move(c0),std::move(c1)} {}

    // access individual cells with brackets
    Cell& operator[](size_t i){
        return const_cast<Cell&>(std::as_const(body)[i]);
    }

    void grow_older() { age++; }
    void forage() { resources=0.0; for (auto& c : body) resources += c.forage_returns(p.type_2); }
    void divide_resources();
    bool dies();

    unsigned get_age() const { return age; }

    std::valarray<double> get_traits() const;

    // for debugging:
    double get_resources() const { return resources; }
};

#endif // INDIVIDUAL_H
