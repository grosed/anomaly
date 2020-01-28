#include "Functions_robustmean.h"
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include "user_interupt.h"
#include "check_user_interrupt.h"

namespace anomalymv
{

void solveorderedobservationlist_robustmean(struct orderedobservationlist_robustmean *list, int n, int p, int l, double* penaltycomponent, double penaltyanomaly, int minseglength, int maxseglength)
{

	int ii, jj;

	double *componentcost = NULL;
	componentcost = (double *) calloc(p, sizeof(double));

	struct position_saving *savingvector = NULL;
	savingvector = (struct position_saving *) calloc(p, sizeof(struct position_saving));

	double totalpenalty = 0.0, sqrt_penaltyanomaly = 0.0;

	for (jj = 0; jj < p; jj++)
	{

		totalpenalty = totalpenalty + penaltycomponent[jj];

	}

	sqrt_penaltyanomaly = sqrt(penaltyanomaly);

	
	for (ii = 1; ii < n+1; ii++)
	{

		update_cumsums_and_segmentcosts_robustmean(list,ii,n,p,l,minseglength,sqrt_penaltyanomaly,penaltyanomaly);
		compute_cost_of_starting_anomalies_robustmean(list,ii,n,p,l,minseglength,penaltycomponent,componentcost);
		find_best_option_robustmean(list,ii,n,p,l,minseglength,penaltycomponent,penaltyanomaly,savingvector);
		pruner_robustmean(list, ii, p, l, minseglength, maxseglength, totalpenalty);

		if (ii % 16 == 0)
		{

			if(check_user_interrupt())
		  	{

				if(componentcost){free(componentcost);}
				if(savingvector){free(savingvector);}
				user_interupt a;
				throw(a);				

		  	}

		}

	}
	

	if(componentcost){free(componentcost);}
	if(savingvector){free(savingvector);}

}

} // namespace anomalymv
