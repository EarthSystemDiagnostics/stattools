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
