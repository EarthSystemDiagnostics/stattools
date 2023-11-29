# stattools 0.5.0

* **New** functions `t_test()` and `ks_test()` to perform Student's t-Test and
  Kolmogorov-Smirnov Test on autocorrelated data.
* The addition of these functions makes a license change necessary. `stattools`
  is now distributed under GPL (>= 3).

# stattools 0.4.0

* **New** function `getEffectiveDOF()` to calculate the effective degrees of
  freedom of an autocorrelated dataset.

# stattools 0.3.0

* **New** function `ShiftCorrelation()` to run a shift correlation analysis
  between two data vectors in order to find the optimal shift maximising the
  correlation;
* added unit test infrastructure.

# stattools 0.2.0

Minor changes:

* `rmsd()` now includes the option to calculate the RMSD of one vector against
  zero automatically when only a single vector is passed as first argument.
* Souce files in `R/` have been reorganized.
* R CMD check now passes with zero notes or warnings.
* Code base was beautified to adhere to style guide.
* Function documentations were improved.

# stattools 0.1.0

* Initial package version with number and scope of the functions incl. their
  documentation identical to their original versions in deprecated package
  'ecustools' which they were a part of ('ecustools'
  [`main`](https://github.com/EarthSystemDiagnostics/ecustools/tree/master)
  branch as of 2020-11-20).
