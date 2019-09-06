#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <math.h>
#include <stdlib.h>

#include "Functions_mean.h"

#include <vector>

using namespace anomalymv;

std::vector<int> MeanAnomalyMV(SEXP Rx, SEXP Rn, SEXP Rp, SEXP Rl, SEXP Rminlength, SEXP Rbetachange, SEXP Rbetaanomaly, SEXP Rmaxlength, SEXP Ronline)
// SEXP MeanAnomalyMV(SEXP Rx, SEXP Rn, SEXP Rp, SEXP Rl, SEXP Rminlength, SEXP Rbetachange, SEXP Rbetaanomaly, SEXP Rmaxlength, SEXP Ronline)
{
  
  /* 
  Rx    : Data
  Rn    : Length of data
  */
	 
 	PROTECT(Rx) ; 
 	PROTECT(Rn) ;
 	PROTECT(Rp) ;
	PROTECT(Rl) ;
	PROTECT(Rminlength) ;
	PROTECT(Rbetachange) ;
	PROTECT(Rbetaanomaly) ;
	PROTECT(Rmaxlength) ;
	PROTECT(Ronline) ;
	
  	int n = 0, p = 0, l = 0, minlength = 0, ii = 0, status = 0, maxlength = 0, online = 0;
  	double betaanomaly = 0.0;
  	double* x = NULL;
  	double* betachange_DUMMY = NULL;
  	double* betachange = NULL;
	
	maxlength        = *(INTEGER(Rmaxlength));
 	minlength        = *(INTEGER(Rminlength));
	n                = *(INTEGER(Rn));
	p                = *(INTEGER(Rp));
	l  		 = *(INTEGER(Rl));
	online           = *(INTEGER(Ronline));
  	x          	 = REAL(Rx);
  	betachange_DUMMY = REAL(Rbetachange);
  	betaanomaly      = *REAL(Rbetaanomaly);


	betachange = (double*)calloc(p,sizeof(double));

  	for (ii = 0; ii < p; ii++)
  	{
  		betachange[ii] = betachange_DUMMY[ii];
  	}


	struct orderedobservationlist_mean* mylist;

	populate_mean(&mylist, x, n, p, l); 
	
	status = solveorderedobservationlist_mean(mylist, n, p, l, betachange, betaanomaly, minlength, maxlength);

	if(status)
	{

		for (ii = 0; ii < n + l + 2; ii++)
		{

			if(mylist[ii].observation){free(mylist[ii].observation);}
			if(mylist[ii].mean_of_xs){free(mylist[ii].mean_of_xs);}
			if(mylist[ii].segmentcosts){free(mylist[ii].segmentcosts);}
			if(mylist[ii].best_end_costs){free(mylist[ii].best_end_costs);}
			if(mylist[ii].affectedcomponents){free(mylist[ii].affectedcomponents);}
			if(mylist[ii].startlag){free(mylist[ii].startlag);}
			if(mylist[ii].endlag){free(mylist[ii].endlag);}

		}

		if(betachange){free(betachange);}
		if(mylist){free(mylist);}

	  
	  	UNPROTECT(9);

	  	// return(R_NilValue) ; 
		return std::vector<int>();
	}

	SEXP Rout ;
	std::vector<int> vout;	

	if (online)
	{

		PROTECT(Rout = allocVector(INTSXP, n*(2 + 3*p)));
		vout.resize(n*(2 + 3*p));
		int *out;
  		out  = INTEGER(Rout);
		
		changepointreturn_mean_online(mylist, n, p, out);
		

	} 
	else
	{

		int numberofchanges = 0, *changes = NULL, *components = NULL, *startlag = NULL, *endlag = NULL;
	
		changepointreturn_mean(mylist, n, p, &numberofchanges, &changes, &components, &startlag, &endlag);
		
  		PROTECT(Rout = allocVector(INTSXP, numberofchanges*(3 + 3*p)));
		vout.resize(numberofchanges*(3 + 3*p));
		
		int *out;
  		out  = INTEGER(Rout);
  		
		for (ii = 0; ii < 3*numberofchanges; ii++)
		{
			out[ii] = changes[ii];
		}

		for (ii = 0; ii < numberofchanges*p; ii++)
		{
			out[ii + 3*numberofchanges] = components[ii];
		}

		for (ii = 0; ii < numberofchanges*p; ii++)
		{
			out[ii + numberofchanges*(3 + p)] = startlag[ii];
		}

		for (ii = 0; ii < numberofchanges*p; ii++)
		{
			out[ii + numberofchanges*(3 + 2*p)] = endlag[ii];
		}

		if(components){free(components);}
		if(startlag){free(startlag);}
		if(endlag){free(endlag);}
		if(changes){free(changes);}

	}
	
	for (ii = 0; ii < n + l + 2; ii++)
	{

		if(mylist[ii].observation){free(mylist[ii].observation);}
		if(mylist[ii].mean_of_xs){free(mylist[ii].mean_of_xs);}
		if(mylist[ii].segmentcosts){free(mylist[ii].segmentcosts);}
		if(mylist[ii].best_end_costs){free(mylist[ii].best_end_costs);}
		if(mylist[ii].affectedcomponents){free(mylist[ii].affectedcomponents);}
		if(mylist[ii].startlag){free(mylist[ii].startlag);}
		if(mylist[ii].endlag){free(mylist[ii].endlag);}

	}

	int *out;
	out  = INTEGER(Rout);
	for(unsigned int cursor = 0; cursor < vout.size(); cursor++)
	  {
	    vout[cursor] = out[cursor];
	  }

	
	if(mylist){free(mylist);}
	if(betachange){free(betachange);}

  	UNPROTECT(10);

  	// return(Rout) ;
	return(vout);
}










