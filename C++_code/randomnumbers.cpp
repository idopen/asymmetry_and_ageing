#include <iostream>
#include <chrono>
#include "randomnumbers.h"

std::mt19937 rng; // this is (probably) the default random engine

unsigned long randomize() {
    static std::random_device rd{}; // only used to generate seed for engine
    auto seed = rd();
    rng.seed(seed); // seed the engine
    return seed;
}


