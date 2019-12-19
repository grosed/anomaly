#include <math.h>
#include <stdlib.h>
#include <list>
#include "tukey.h"

class Online_tukey
{
	public:
	std::list<tukey_object> object_list;
	Online_tukey();
	void Add_observation(double,const double, const double);
	double Find_minimum();

};