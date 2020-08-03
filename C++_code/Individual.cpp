#include "Individual.h"

void Individual::divide_resources(){
    double h0{body[0].get_harvest()};
    double h1{body[1].get_harvest()};
    double share0{h0/(h0+h1)};
    body[0].set_resources(resources * share0);
    body[1].set_resources(resources * (1.0-share0));
}

bool Individual::dies() {
    bool dead0 = body[0].dies_ad();
    bool dead1 = body[1].dies_ad();

    if (dead0 && dead1) return true;
    else if (dead0) {
        // duplicate cell 1. This leads to additional damage, mutation and/or death
        //body[1].accumulate_damage();
        if (body[1].dies_sol()) return true;
        body[0]=body[1];
        body[0].set_type(0);

        // NB: asymmetric mutation
        //body[0].mutate();
    }
    else if (dead1){
        // duplicate cell 0
        //body[0].accumulate_damage();
        if (body[0].dies_sol()) return true;
        body[1]=body[0];
        body[1].set_type(1);

        // NB: asymmetric mutation
        //body[1].mutate();
    }

    return false;
}

std::valarray<double> Individual::get_traits() const{
    auto x = std::valarray<double>(0.0, n_traits);
    auto g = body[0].get_g();

    x[0] = age;
    x[1] = logist3(g[0][0],g[1][0],0); // repro type 0
    x[2] = logist3(g[0][1],g[1][1],0); // repro type 1
    x[3] = logist3(g[0][0],g[1][0],1); // repair type 0
    x[4] = logist3(g[0][1],g[1][1],1); // repair type 1
    x[5] = g[2][0]; // harvest type 0
    x[6] = g[2][1]; // harvest type 0
    x[7] = logist(g[3][0]);
    x[8] = logist(g[3][1]);
    x[9] = body[0].get_damage();
    x[10] = body[1].get_damage();
    x[11] = body[0].get_resources();
    x[12] = body[1].get_resources();
    for (unsigned i=13; i<29; ++i) x[i] = body[0].get_vg()[i-13];
    for (unsigned i=29; i<45; ++i) x[i] = body[1].get_vg()[i-29];

    return x;
}
