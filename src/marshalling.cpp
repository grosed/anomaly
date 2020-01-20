

#include <Rcpp.h>

#include <vector>
#include <list>
#include <tuple>
#include <algorithm>
#include <thread>
#include <chrono>
#include <future>


#include "MeanVarAnomaly.h"
#include "MeanAnomaly.h"
#include "RobustMeanAnomaly.h"
#include "RobustMeanAnomalyMV.h"
#include "sequential_quantiles.h"
#include "recursive_anomalies.h"
#include "recursive_mvanomalies.h"
#include "MultivariateCOD.h"
#include "MeanAnomalyMV.h"
#include "pass.h"
#include "bard.h"

//[[Rcpp::export]]
std::list<std::list<std::vector<double> > > marshall_bard(const std::list<std::vector<double> >& data,
								    const double& p_N,
								    const double& p_A,
								    const double& k_N,
								    const double& k_A,
								    const double& pi_N,
								    const double& alpha,
								    const double& paffected,
								    const double& lower,
								    const double& upper,
								    const double& h,
								    const int& seed)
{
  base_generator_type generator(seed);
  boost::uniform_real<> uni_dist(0,1);
  boost::variate_generator<base_generator_type&, boost::uniform_real<> > uni(generator, uni_dist);
  auto pi_A = 1.0 - pi_N;
  int t = 0;
  int N = data.front().size();
  // helper for creating the equivalent tof a python arange
  auto arange = [](const double& a,const double& b,const double& s)
    {
      auto n = int(0.5 + (b-a)/s);
      std::vector<double> res(n);
      double val = a;
      for(int i = 0; i < n; i++)
	{
	  res[i] = val;
	  val += s;
	}
      return res;
    };
  auto museq = arange(lower,upper,h);
  auto SY = std::list<std::vector<double> >(1,std::vector<double>(N,0));
  auto SYY = SY;
  std::list<std::tuple<double,double,int> > R;

  
  // initialise state
  auto S = std::make_tuple(R,t,SY,SYY,museq,paffected,pi_N,pi_A,p_N,p_A,k_N,k_A,N);
  // run sequential bard accumulating the pruned R values from each iteration
  std::list<std::list<std::vector<double> > > Rs;

  int count = 0;
  for(auto& y : data)
    {
      try
	{
	  Rcpp::checkUserInterrupt();
	  S = bard(std::move(S),y);
	  // ok - see if we can add the pruning
	  //if(std::get<1>(S) > 2) // do not prune until the probabilities have been normalized.
	  //  {
	  //    S = prune_bard_result(std::move(S),alpha,uni);
	  //  }
	  R = std::get<0>(S);
	  // convert tuples to vectors for marshalling through Rcpp
	  std::list<std::vector<double> > vR;
	  for(auto& r : R)
	    {
	      std::vector<double> vr(3);
	      vr[0] = std::get<0>(r);
	      vr[1] = std::get<1>(r);
	      vr[2] = std::get<2>(r);
	      vR.push_back(vr);
	    }
	  Rs.push_back(vR);
	  count++;
	}
      catch(...)
	{
	  throw(std::exception());
	}
    }
  return Rs;
}



//[[Rcpp::export]]
std::vector<int> marshall_MeanVarAnomaly(SEXP a,
				      SEXP b,
				      SEXP c,
				      SEXP d,
				      SEXP e,
				      SEXP f,
				      SEXP g)
{
  return MeanVarAnomaly(a,b,c,d,e,f,g);
}



//[[Rcpp::export]]
std::vector<int> marshall_MeanAnomaly(SEXP a,
				      SEXP b,
				      SEXP c,
				      SEXP d,
				      SEXP e,
				      SEXP f,
				      SEXP g)
{
  return MeanAnomaly(a,b,c,d,e,f,g);
}

//[[Rcpp::export]]
std::vector<int> marshall_RobustMeanAnomaly(SEXP a,
				            SEXP b,
				            SEXP c,
				            SEXP d,
				            SEXP e,
				            SEXP f,
				            SEXP g)
{
  return RobustMeanAnomaly(a,b,c,d,e,f,g);
}


//[[Rcpp::export]]
std::vector<int> marshall_recursive_anomalies(SEXP a,
					      SEXP b,
					      SEXP c)
{
  return recursive_anomalies(a,b,c);
}

//[[Rcpp::export]]
std::vector<int> marshall_recursive_mvanomalies(SEXP a,
						SEXP b,
						SEXP c,
						SEXP d,
						SEXP e,
						SEXP f,
						SEXP g)
						
{
  return recursive_mvanomalies(a,b,c,d,e,f,g);
}


//[[Rcpp::export]]
std::vector<int> marshall_MeanVarAnomalyMV(SEXP a,
					   SEXP b,
					   SEXP c,
					   SEXP d,
					   SEXP e,
					   SEXP f,
					   SEXP g,
					   SEXP h,
					   SEXP i
					   )
{
  return MeanVarAnomalyMV(a,b,c,d,e,f,g,h,i);
}

//[[Rcpp::export]]
std::vector<int> marshall_RobustMeanAnomalyMV(SEXP a,
					SEXP b,
					SEXP c,
					SEXP d,
					SEXP e,
					SEXP f,
					SEXP g,
					SEXP h,
					SEXP i)
{
  return RobustMeanAnomalyMV(a,b,c,d,e,f,g,h,i);
}

//[[Rcpp::export]]
std::vector<int> marshall_MeanAnomalyMV(SEXP a,
					SEXP b,
					SEXP c,
					SEXP d,
					SEXP e,
					SEXP f,
					SEXP g,
					SEXP h,
					SEXP i)
{
  return MeanAnomalyMV(a,b,c,d,e,f,g,h,i);
}


// [[Rcpp::export]]
Rcpp::List marshall_sequential_ests(const std::vector<double>& data, int n, int burnin, double lqs, double lqf0, double meds, double medf0, double uqs, double uqf0)
{

  std::tuple<double,double> lqinit = std::make_tuple(lqs, lqf0);
  std::tuple<double,double> medinit = std::make_tuple(meds, medf0);
  std::tuple<double,double> uqinit = std::make_tuple(uqs, uqf0);
  
  std::tuple<std::vector<double>,std::vector<double>> result = sequential_ests(data, n, burnin, lqinit, medinit, uqinit);

  return Rcpp::List::create(Rcpp::Named("mu")=std::get<0>(result),
			    Rcpp::Named("sigma")=std::get<1>(result));
}



// [[Rcpp::export]]
std::list<std::vector<double> > marshall_pass(const std::list<std::vector<double> >& Xi,
					      const int& Lmax,
					      const int& Lmin,
					      const int& alpha,
					      const double& lambda)
{
  // Create a std::promise object
  std::promise<void> exitSignal;
  // fetch std::future object associated with promise
  std::shared_future<void> futureObj = exitSignal.get_future();

  // start worker thread
  auto future = std::async(std::launch::async,pass,Xi,Lmax,Lmin,alpha,lambda,std::move(futureObj));

  // check for user interrupt
  try
    {
      while(std::future_status::ready != future.wait_for(std::chrono::milliseconds(0)))
	{
	  Rcpp::checkUserInterrupt();
	}
    }  
  catch(std::bad_alloc &e)
    {
      exitSignal.set_value();
      auto result = future.get(); // wait for it to tidy up
      Rcpp::stop("insufficient memory");
    }
  catch(...)
    {
      exitSignal.set_value();
      auto result = future.get(); // wait for it to tidy up
      Rcpp::stop("user interrupt");
    }
  
  auto result = future.get();
  auto cpts = std::get<0>(result);
  auto xstar = std::get<1>(result);
  std::list<std::vector<double> > marshalled_result(cpts.size());
  if(marshalled_result.size() > 0)
    {
      transform(cpts.begin(),cpts.end(),xstar.begin(),marshalled_result.begin(),[](auto& cpt,auto& x)
		{
		  std::vector<double> entry(3);
		  entry[0] = (double)std::get<0>(cpt);
		  entry[1] = (double)std::get<1>(cpt);
		  entry[2] = x;
		  return(entry);
		});
    }
  
  return marshalled_result;  
}



