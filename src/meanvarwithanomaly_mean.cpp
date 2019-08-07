#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <math.h>
#include <stdlib.h>

#include "Functions.h"

#include <vector>

using namespace anomaly;

std::vector<int> MeanAnomaly(SEXP Rx, SEXP Rn, SEXP Rminlength, SEXP Rmaxlength, SEXP Rbetachange, SEXP Rbetaanomaly, SEXP Ronline)
// SEXP MeanAnomaly(SEXP Rx, SEXP Rn, SEXP Rminlength, SEXP Rmaxlength, SEXP Rbetachange, SEXP Rbetaanomaly, SEXP Ronline)
{
  
  /* 
  Rx    : Data
  Rn    : Length of data
  */
	 
 	PROTECT(Rx) ; 
 	PROTECT(Rn) ;
	PROTECT(Rminlength) ;
	PROTECT(Rmaxlength) ;
	PROTECT(Rbetachange) ;
	PROTECT(Rbetaanomaly) ;
	PROTECT(Ronline) ;
	
  	int n = 0, minlength = 0, maxlength = 0, error = 0, online = -1, ii = 0;
  	double betaanomaly = 0.0;
  	double* x = NULL, *betachange = NULL, *betavector = NULL ;
	
  
 	minlength        = *(INTEGER(Rminlength));
	maxlength        = *(INTEGER(Rmaxlength));
	n                = *(INTEGER(Rn));
  	x          	 =   REAL(Rx);
  	betachange       =   REAL(Rbetachange);
  	betaanomaly      = *REAL(Rbetaanomaly);
	online           = *INTEGER(Ronline);

	struct orderedobservationlist_mean* mylist;

	populateorderedobservationlist_mean(&mylist, x, n); 

	betavector = (double*) calloc(maxlength, sizeof(double));
	
	for (ii = 0; ii < minlength-1; ii++){betavector[ii] = 0;}
	for (ii = minlength-1; ii < maxlength; ii++){betavector[ii] = betachange[ii+1-minlength];}

	
	error = solveorderedobservationlist_mean(mylist, n, betavector, betaanomaly, minlength, maxlength);
	
	if (error)
	{
		free(betavector);
	  	free(mylist);
	  	UNPROTECT(7);
		return std::vector<int>();
	  	// return R_NilValue ; 
	}

	int numberofchanges = 0, *changes = NULL;

	/*
	SEXP Rout ;

	if (online == 0)
	{

		changepointreturn_mean(mylist, n, &numberofchanges, &changes);

  		PROTECT(Rout = allocVector(INTSXP, 3*numberofchanges));

		int *out;
  		out  = INTEGER(Rout);
  	
		for (ii = 0; ii < 3*numberofchanges; ii++)
		{
			out[ii] = changes[ii];
		}

	}
	else
	{

		changepointreturn_online_mean(mylist, n, &changes);
	 
  		PROTECT(Rout = allocVector(INTSXP, 2*n));

		int *out;
  		out  = INTEGER(Rout);
  	
		for (ii = 0; ii < 2*n; ii++)
		{
			out[ii] = changes[ii];
		}

	}
	*/


	std::vector<int> Rout;

	if (online == 0)
	{

		changepointreturn_mean(mylist, n, &numberofchanges, &changes);

		Rout.resize(3*numberofchanges);
  	
		for (ii = 0; ii < 3*numberofchanges; ii++)
		{
			Rout[ii] = changes[ii];
		}

	}
	else
	{

		changepointreturn_online_mean(mylist, n, &changes);
		Rout.resize(2*n);
		for (ii = 0; ii < 2*n; ii++)
		{
			Rout[ii] = changes[ii];
		}

	}





	
	free(changes);
	free(betavector);
	free(mylist); 

  	// UNPROTECT(8);
	UNPROTECT(7);
  	return(Rout) ; 
}










