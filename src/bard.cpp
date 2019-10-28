
#include <tuple>
#include <list>
#include <vector>
#include <cmath>
#include <limits>
#include <numeric>

#include <boost/math/distributions/negative_binomial.hpp>

#include "bard.h"


#include <iostream>


state_type prune_bard_result(state_type S,
			       const double& alpha,
			       boost::variate_generator<base_generator_type&, boost::uniform_real<> >& uni)
{
  // to make things more readable - unpack the state (should be a zero cost operation when using move semantics)
  std::list<std::tuple<double,double,int> > R(std::move(std::get<0>(S)));
  int t(std::move(std::move(std::get<1>(S))));
  std::list<std::vector<double> > SY(std::move(std::move(std::get<2>(S))));
  std::list<std::vector<double> > SYY(std::move(std::move(std::get<3>(S))));
  std::vector<double> museq(std::move(std::get<4>(S)));
  double paffected(std::move(std::move(std::get<5>(S))));
  double pi_N(std::move(std::move(std::get<6>(S))));
  double pi_A(std::move(std::move(std::get<7>(S))));
  double p_N(std::move(std::get<8>(S)));
  double p_A(std::move(std::move(std::get<9>(S))));
  int k_N(std::move(std::move(std::get<10>(S))));
  int k_A(std::move(std::move(std::get<11>(S))));
  int N(std::move(std::move(std::get<12>(S))));

  auto inf = std::numeric_limits<double>::infinity();
  
  auto n = R.size();
  auto logalpha = log(alpha);
  auto logu = log(alpha) + log(uni());
  // auto logu = log(alpha) + log(0.5);
  // std::cout << logu << std::endl;
  // std::cout << logalpha << std::endl;
 
  for(auto& r : R)
    {
      for(int j = 0; j < 2; j++)
	{
	  double logs = j == 0 ? std::get<0>(r) : std::get<1>(r);
	  // std::cout << logs << std::endl;
	  if(logs > logalpha)
	    {
	    }
	  else if(logu < logs)
	    {
	      auto c = std::max(logalpha,logs);
	      auto logalphaweight = c + log(exp(logalpha-c)-exp(logs-c));
	      c = std::max(logu,logalphaweight);
	      // logu = c + log(exp(logu-c)-exp(logalphaweight-c));
	      logu = c + log(exp(logu-c)+exp(logalphaweight-c));
	      if(j == 0)
		{
		  std::get<0>(r) = logalpha;
		}
	      else
		{
		  std::get<1>(r) = logalpha;
		} 
	    }
	  else
	    {
	      auto c = std::max(logu,logs);
	      logu = c + log(exp(logu-c)-exp(logs-c));
	      if(j == 0)
		{
		  std::get<0>(r) = -inf;
		}
	      else
		{
		  std::get<1>(r) = -inf;
		} 
	    } 
	}      
    }


  // std::cout << R.size() << std::endl;
  R.remove_if([&inf](const std::tuple<double,double,int>& r)
	      {
		// std::cout << std::get<0>(r) << " : " << std::get<1>(r) << std::endl;
		return std::get<0>(r) == -inf && std::get<1>(r) == -inf;
	      });
  // std::cout << R.size() << std::endl;
   
  return std::make_tuple(std::move(R),
			 std::move(t),
			 std::move(SY),
			 std::move(SYY),
			 std::move(museq),
			 std::move(paffected),
			 std::move(pi_N),
			 std::move(pi_A),
			 std::move(p_N),
			 std::move(p_A),
			 std::move(k_N),
			 std::move(k_A),
			 std::move(N));
}


std::list<std::vector<double> > cusum_Y(std::list<std::vector<double> > SY, std::vector<double> data)
{
  auto last = SY.back();
  std::transform(last.begin(),last.end(),data.begin(),data.begin(),[](const double& a,const double& b){return a+b;});
  SY.push_back(data);
  return SY;
}

std::list<std::vector<double> > cusum_YY(std::list<std::vector<double> > SY, std::vector<double> data)
{
  auto last = SY.back();
  std::transform(last.begin(),last.end(),data.begin(),data.begin(),[](const double& a,const double& b){return a+b*b;});
  SY.push_back(data);
  return SY;
}

double log_P_A(const int& s,
	       const int& t,
	       const std::list<std::vector<double> >& SY,
	       const std::vector<double>& mus,
	       const double& p)
{
  auto N = SY.back().size();
  auto n = SY.size();
  auto mu_dens = 1.0/(mus[mus.size()-1] - mus[0]);
  // NB - assumes mus are equally spaced - might change later
  auto mu_width = mus[1]-mus[0];
  std::vector<double> res(mus.size());
  auto itSY_t = SY.begin();
  auto itSY_s_minus_1 = SY.begin();
  std::advance(itSY_t,t);
  std::advance(itSY_s_minus_1,s-1);
  for(int k = 0; k < mus.size(); k++)
    {
      std::vector<double> to_sum(N);
      std::transform((*itSY_t).begin(),(*itSY_t).end(),(*itSY_s_minus_1).begin(),to_sum.begin(),
		     [&k,&t,&s,&p,&mus](const double& x,const double& y)
		     {
		       return log(1+exp(mus[k]*(x-y - mus[k]*(t-s+1)/2) + log(p) - log(1-p)));
		     });
      res[k] = (double)N*log(1-p)+std::accumulate(to_sum.begin(),to_sum.end(),0.0);
    }
  auto cmax = *std::max_element(res.begin(),res.end());
  std::transform(res.begin(),res.end(),res.begin(),[&cmax](const double& x){return exp(x-cmax);});
  return cmax + log(std::accumulate(res.begin(),res.end(),0.0)) + log(mu_dens) + log(mu_width);
}



double log_P_N(const int& s,const int& t,const std::list<std::vector<double> >& SYY)
{
  auto N = SYY.back().size();
  auto itSYY_t = SYY.begin();
  auto itSYY_s_minus_1 = SYY.begin();
  std::advance(itSYY_t,t);
  std::advance(itSYY_s_minus_1,s-1);
  std::vector<double> res(N);
  std::transform((*itSYY_t).begin(),(*itSYY_t).end(),(*itSYY_s_minus_1).begin(),res.begin(),[](const double& x,const double& y){return x-y;});
  return -0.5*std::accumulate(res.begin(),res.end(),0.0);
}



std::list<std::tuple<double,double,int> > normalise_logprobs(std::list<std::tuple<double,double,int> > R)
{
  std::vector<double> x(R.size());
  std::transform(R.begin(),R.end(),x.begin(),[](const std::tuple<double,double,int>& r) { return std::get<0>(r);});
  auto c = *std::max_element(x.begin(),x.end());  
  std::transform(R.begin(),R.end(),x.begin(),[](const std::tuple<double,double,int>& r) { return std::get<1>(r);});
  c = std::max(*std::max_element(x.begin(),x.end()),c);
  std::transform(R.begin(),R.end(),x.begin(),
		 [&c](const std::tuple<double,double,int>& r)
		 {
		   return exp(std::get<0>(r)-c)+exp(std::get<1>(r)-c);
		 });
  auto d = c + log(std::accumulate(x.begin(),x.end(),0.0));
  for(auto& r : R)
    {
      std::get<0>(r) -= d;
      std::get<1>(r) -= d; 
    }
  return(R);
}



state_type bard(state_type S, const std::vector<double>& y)
{
  // to make things more readable - unpack the state (should be a zero cost operation when using move semantics)
  std::list<std::tuple<double,double,int> > R(std::move(std::get<0>(S)));
  int t(std::move(std::move(std::get<1>(S))));
  std::list<std::vector<double> > SY(std::move(std::move(std::get<2>(S))));
  std::list<std::vector<double> > SYY(std::move(std::move(std::get<3>(S))));
  std::vector<double> museq(std::move(std::get<4>(S)));
  double paffected(std::move(std::move(std::get<5>(S))));
  double pi_N(std::move(std::move(std::get<6>(S))));
  double pi_A(std::move(std::move(std::get<7>(S))));
  double p_N(std::move(std::get<8>(S)));
  double p_A(std::move(std::move(std::get<9>(S))));
  int k_N(std::move(std::move(std::get<10>(S))));
  int k_A(std::move(std::move(std::get<11>(S))));
  int N(std::move(std::move(std::get<12>(S))));

  // update data
  SY = cusum_Y(std::move(SY),y);
  SYY = cusum_YY(std::move(SYY),y);
  std::get<2>(S) = SY;
  std::get<3>(S) = SYY;

  auto E_N = k_N * (1-p_N)/p_N;
  auto E_A = k_A * (1-p_A)/p_A;

  // std::cout << log_P_A(1,1,SY,museq,paffected) << std::endl;
  // std::cout << log_P_N(1,1,SYY) << std::endl;

  
  // initial state
  if(t == 0)
    {
      R.push_back(std::make_tuple(log(pi_N*E_N) - log(pi_N*E_N+E_A) + log_P_N(1,1,SYY),
				  log(E_A) - log(pi_N*E_N+E_A) + log_P_N(1,1,SYY) + log_P_A(1,1,SY,museq,paffected),
				  0)
		  );
      t = t+1;
      return std::make_tuple(std::move(R),
			     std::move(t),
			     std::move(SY),
			     std::move(SYY),
			     std::move(museq),
			     std::move(paffected),
			     std::move(pi_N),
			     std::move(pi_A),
			     std::move(p_N),
			     std::move(p_A),
			     std::move(k_N),
			     std::move(k_A),
			     std::move(N));
    }
  
  auto inf = std::numeric_limits<double>::infinity();
  auto new_sN = -inf;
  {
    auto nbinom = boost::math::negative_binomial_distribution<double>(k_A,p_A);
    std::vector<double> log_alpha_s(R.size());
    std::transform(R.begin(),R.end(),log_alpha_s.begin(),
		   [&nbinom,&pi_N,&t](const std::tuple<double,double,int>& r)
		   {
		     auto i = std::get<2>(r);
		     return std::get<1>(r) + log(pi_N) + log(pdf(nbinom,t-i)) - log(cdf(complement(nbinom,t-i-1))); 
		   });
    auto c = *std::max_element(log_alpha_s.begin(),log_alpha_s.end());
    if(c > -inf)
      {
	std::vector<double> res(log_alpha_s.size());
	std::transform(log_alpha_s.begin(),log_alpha_s.end(),res.begin(),
		       [&c](const double& x)
		       {
			 return exp(x-c);
		       });	
	new_sN = log_P_N(t+1,t+1,SYY) + c + log(std::accumulate(res.begin(),res.end(),0.0));			 
      }
  }

  auto new_sA = -inf;
  {
    auto nbinom_N = boost::math::negative_binomial_distribution<double>(k_N,p_N);
    std::vector<double> log_alpha_sN(R.size());
    std::transform(R.begin(),R.end(),log_alpha_sN.begin(),
		   [&nbinom_N,&t](const std::tuple<double,double,int>& r)
		   {
		     auto i = std::get<2>(r);
		     return std::get<1>(r) + log(pdf(nbinom_N,t-i)) - log(cdf(complement(nbinom_N,t-i-1))); 
		   });
    auto nbinom_A = boost::math::negative_binomial_distribution<double>(k_A,p_A);
    std::vector<double> log_alpha_sA(R.size());
    std::transform(R.begin(),R.end(),log_alpha_sA.begin(),
		   [&nbinom_A,&pi_A,&t](const std::tuple<double,double,int>& r)
		   {
		     auto i = std::get<2>(r);
		     return std::get<1>(r) + log(pi_A) + log(pdf(nbinom_A,t-i)) - log(cdf(complement(nbinom_A,t-i-1))); 
		   });
    std::vector<double> log_alpha_s(log_alpha_sN);
    log_alpha_s.insert(log_alpha_s.end(),log_alpha_sA.begin(),log_alpha_sA.end());
    auto c = *std::max_element(log_alpha_s.begin(),log_alpha_s.end());
    if(c > -inf)
      {
	std::vector<double> res(log_alpha_s.size());
	std::transform(log_alpha_s.begin(),log_alpha_s.end(),res.begin(),
		       [&c](const double& x)
		       {
			 return exp(x-c);
		       });
	new_sA = log_P_A(t+1,t+1,SY,museq,paffected) + log_P_N(t+1,t+1,SYY) + c + log(std::accumulate(res.begin(),res.end(),0.0));			 
      }
  }

  for(auto& r : R)
    {
      auto i = std::get<2>(r);
      auto nbinom_N = boost::math::negative_binomial_distribution<double>(k_N,p_N);
      std::get<0>(r) += log_P_N(i+1,t+1,SYY) - log_P_N(i+1,t,SYY)  +  log(cdf(complement(nbinom_N,t-i))) - log(cdf(complement(nbinom_N,t-i-1)));
      auto nbinom_A = boost::math::negative_binomial_distribution<double>(k_A,p_A);
      std::get<1>(r) += log_P_N(i+1,t+1,SYY) - log_P_N(i+1,t,SYY) + log_P_A(i+1,t+1,SY,museq,paffected) - log_P_A(i+1,t,SY,museq,paffected)
	+ log(cdf(complement(nbinom_A,t-i))) - log(cdf(complement(nbinom_A,t-i-1)));
    }
  R.push_back(std::make_tuple(new_sN,new_sA,t));

  // normalise probabilities
  R = normalise_logprobs(std::move(R));

  // increment epoch
  t = t + 1;

  return std::make_tuple(std::move(R),
			 std::move(t),
			 std::move(SY),
			 std::move(SYY),
			 std::move(museq),
			 std::move(paffected),
			 std::move(pi_N),
			 std::move(pi_A),
			 std::move(p_N),
			 std::move(p_A),
			 std::move(k_N),
			 std::move(k_A),
			 std::move(N));

}



