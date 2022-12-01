# Things that need looking into

1. grep TODO in the package files for bits that aren't finished

1. Use of BH (boost)
```
smithpj1@fyb033000010:~/Documents/Software/anomaly$ grep -r boost
R/bard.R:#    # set boost seed (used by c++) to sink with R random number generator
src/pass.cpp:#include <boost/math/distributions/normal.hpp>
src/pass.cpp:  boost::math::normal norm;
grep: src/anomaly.so: binary file matches
grep: src/pass.o: binary file matches
```

3. Clear out some of the dependancies e.g. Rdpack, BH
