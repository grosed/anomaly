
#ifndef ___BARD_H___
#define ___BARD_H___

#include<list>
#include <tuple>
#include<vector>

#include <boost/random/linear_congruential.hpp>
#include <boost/random/uniform_real.hpp>
#include <boost/random/variate_generator.hpp>



typedef std::tuple<std::list<std::tuple<double,double,int> >,
		   int,
		   std::list<std::vector<double> >,
		   std::list<std::vector<double> >,
		   std::vector<double>,
		   double,
		   double,
		   double,
		   double,
		   double,
		   int,
		   int,
		   int> state_type;

typedef boost::minstd_rand base_generator_type;

// method for sequential bard - use move semantics on state_type for efficiency if required
state_type bard(state_type, const std::vector<double>&);

// method for prunung R from state returned by bard - use move semantics on state_type for efficiency if required
state_type prune_bard_result(state_type, const double&, boost::variate_generator<base_generator_type&, boost::uniform_real<> >&);





#endif
