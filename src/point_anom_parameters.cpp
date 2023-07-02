#include <math.h>
#include <stdlib.h>
#include <float.h>
#include "Functions.h"
#include <limits>
#include <cmath>


namespace anomalymv
{

void point_anom_parameters(struct orderedobservationlist *list, int ii, int p, double penaltyanomaly)
{

	int jj;
	double obs, extra;

	for (jj = 0; jj < p; jj++)
	{

		obs = list[ii].observationsquared[jj];
		double gamma = std::max(std::numeric_limits<double>::min(),std::exp(-(1.0 + penaltyanomaly)));
		extra = penaltyanomaly + log(gamma + obs) + 1 - obs;

		if (extra < 0)
		{
			list[ii].affectedcomponents[jj] = 1;
		}

	}

}

} // namespace anomalymv
