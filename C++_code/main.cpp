#include <iostream>
#include "Population.h"
#include "chrono"

// This version with damage-specific vibility

using namespace std;

int main()
{
    auto start = std::chrono::high_resolution_clock::now();

    unsigned long seed = randomize();
    cout << seed << "\n";

    Population pop{};
    pop.calc_stats();
    ofstream ef("../SymBreak6/evol.txt");
    pop.write_stats(ef,0);


    for (unsigned long t=1; t<=n_timesteps; ++t){
        //cout << i << " ";
        pop.next_timestep();
        if (t % skip == 0){
            pop.calc_stats();
            pop.write_stats(ef,t);
        }
    }

    ofstream pf("../SymBreak6/pop.txt");
    pop.write_pop(pf);

    auto end = std::chrono::high_resolution_clock::now();
    auto diff = end - start;
    cout << endl;
    cout << " That took " << std::chrono::duration<double>(diff).count() << " seconds" << endl;

    return 0;
}
