#ifndef UTILS_H
#define UTILS_H

#include <math.h>
#include <iomanip>
#include <fstream>
#include <iostream>
#include <vector>


template<typename T = double>
inline T logist(const T x) { return 1 / (1 + exp(-x)); }

template<typename T=double>
inline void clip0(T& x){
    if (x<0.0) x=0.0;
}

template<typename T=double>
inline void clip0_epsilon(T& x){
    if (x<0.0) x=0.001;
}

template<typename T=double>
inline void clip01(T& x){
    if (x<0.0) x=0.0; else if (x>1.0) x=1.0;
}

template<typename T=double>
inline double logist3(const double x0, const double x1, const unsigned i){
    double denom = 1.0+exp(x0)+exp(x1);
    if (i==0) return exp(x0)/denom;
    else if (i==1) return exp(x1)/denom;
    return 1.0/denom;
}

// Remove a single element (of type T) from a vector v at index idx.
// This works by moving the last element from v to v[idx] and then popping the vector
template <typename T = double>
void remove_from_vec(std::vector<T>& v, const size_t idx){
    if (auto back_idx = v.size()-1; idx != back_idx) v[idx] = std::move(v[back_idx]);
    v.pop_back();
}

//=== Write same data to two ostreams (e.g. file and cout) ===================================

class ostreamFork{
public:
  std::ostream& os1 ;
  std::ostream& os2 ;
  ostreamFork( std::ostream& os_one , std::ostream& os_two ): os1(os_one) , os2(os_two) {}
};

// For data: int, long , ...
template <class Data>
ostreamFork& operator<<( ostreamFork& osf , Data d ){
   osf.os1 << d ;
   osf.os2 << d ;
   return osf ;
 }

 // For manipulators: endl, flush
 inline ostreamFork& operator<<(ostreamFork& osf , std::ostream& (*f)(std::ostream&)  )
 {
   osf.os1 << f ;
   osf.os2 << f ;
   return osf ;
 }

// For setw() , ...
template<class ManipData>
ostreamFork& operator<<( ostreamFork& osf , std::ostream& (*f)(std::ostream&, ManipData)){
   osf.os1 << f ;
   osf.os2 << f ;
   return osf ;
}

//=== end Write same data to two ostreams =================================================


#endif //UTILS_H
