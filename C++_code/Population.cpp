#include "Population.h"
#include <execution>

Population::Population(){

    // allocate enough memory once and for all
    moms.reserve(p.size);
    gametes.reserve(2*p.size);

    for (size_t i=0; i<p.size; ++i){
        // Create default cell with type:
        Cell c0{0}, c1{1};
        // Set initial amount of resources per cell: (1.0 units)
        c0.set_resources(p.init_resources);
        c1.set_resources(p.init_resources);

        // Construct new mom from cells:
        moms.emplace_back(std::move(c0),std::move(c1));

    }

    /*
    using std::cout;
    for (auto& mom : moms){
        cout << mom[0].get_resources() << mom[1].get_resources() << "\n";
    }
    */

    // print types for debugging; cȟecks out
    /*
    using std::cout;
    for (auto& mom : moms){
        cout << mom[0].get_type() << mom[1].get_type() << "\n";
    }
    */

    // set initial age to 1
    for (auto& mom : moms) mom.grow_older();

    // print ages for debugging; checks out
    /*
    using std::cout;
    for (auto& mom : moms){
        cout << mom.get_age() << "\n";
    }
    */
}

void Population::next_timestep(){
    // forage (add foraging_returns for both cells; returns depend on cells' investment in foraging)
    for (auto& mom : moms) mom.forage();

    // divide foraged resources among cells:
    for (auto& mom : moms) mom.divide_resources();

    // accumulate damage in cells of adults
    std::for_each(std::execution::par, begin(moms), end(moms),
                  [](auto& mom){ mom[0].accumulate_damage(); mom[1].accumulate_damage(); });

    // repair damage in cells of adults
    std::for_each(std::execution::par, begin(moms), end(moms),
                  [](auto& mom){ mom[0].repair_damage(); mom[1].repair_damage(); });

    // create gametes and pass on damage
    for (auto& mom : moms){

        // using special copy constructors here:
        // set gamete type to zero and allocate damage

        for (unsigned i=0; i<2; ++i) if (mom[i].reproduces()){
            double dt{mom[i].damage_transmitted()}; // amount damage transmitted to gamete
            mom[i].set_damage(mom[i].get_damage()-dt);
            gametes.emplace_back(mom[i],std::move(dt));
        }
    }

    // mutate gametes
    std::for_each(std::execution::par, begin(gametes),end(gametes),[](auto& g){ g.mutate(); });

    // add gametes to moms
    for (auto gam : gametes){
        Cell c1{gam}; // regular copy constructor
        // copies damage too; might want extra locus for damage asymmetry here too

        //c1.mutate(); // extra mutation in type 1 ("mother keeps template")
        // turns out this may push type 1 to specialize in reproduction and retaining damage

        c1.set_type(1);
        moms.emplace_back(std::move(gam),std::move(c1));
    }

    // mortality moms according to damage etc.(now including duplicated gametes)
    // adults die if both cells die
    // if 1 cell dies, the other is duplicated (with extra risks)
    moms.erase(std::remove_if(begin(moms),end(moms),[](auto& mom){ return mom.dies(); }),end(moms));

    // remove excess moms until popsize const
    while (moms.size()>p.size){
        size_t rm{rn(moms.size())}; // random mom index
        remove_from_vec(moms,rm);   // kill her
    }

    // can get rid of gametes now
    gametes.clear();

    // age++
    for (auto& mom : moms) mom.grow_older();
}

// parallelized stats:
void Population::calc_stats(){
    auto x0 = std::valarray(0.0,n_traits);
    using vd = std::valarray<double>;

    vd sum = std::transform_reduce(std::execution::par, moms.begin(), moms.end(),
                                   x0, std::plus<vd>(),[](auto& mom){ return mom.get_traits(); });
    stats.means = sum/moms.size();

    /*
    auto ss = [&](auto& mom){ return (mom.get_traits()-stats.means)*(mom.get_traits()-stats.means); };
    vd sum_squares = std::transform_reduce(std::execution::par, moms.begin(), moms.end(), x0, std::plus<vd>(), ss);
    stats.sds = sqrt(sum_squares/moms.size());
    */

}

void Population::write_stats(std::ofstream& file, size_t time){
    ostreamFork osf(file, std::cout); // write same stuff to file and screen

    osf << std::setw(8) << time;
    osf << std::setw(8) << moms.size();
    osf << std::setw(8) << std::setprecision(3);
    for (size_t i=0; i<n_traits; ++i) osf << std::setw(9) << stats.means[i];
    /*
    osf << "    ";
    for (size_t i=0; i<n_traits; ++i) osf << std::setw(8) << stats.sds[i];
    */
    osf << '\n';
}

// write whole pop to file at end of simulation
void Population::write_pop(std::ofstream& file){
    for (auto& mom : moms){
        auto b = mom.get_traits();
        Individual mom_copy{mom};
        bool dead{mom_copy.dies()};
        for (size_t i=0; i<b.size(); ++i)
            file << std::setw(15) << std::setprecision(3) << b[i];
        if (dead) file << std::setw(5) << 1;
        else file << std::setw(5) << 0;
        file << '\n';
    }
}
