This is a bugfix release with no new features and no breaking changes. The 
revdep failure was investigated and it was unrelated to ggraph

## Test environments
* local R installation, R 4.0.1
* ubuntu 16.04 (on travis-ci), R 4.0.1
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

## revdepcheck results

We checked 39 reverse dependencies (38 from CRAN + 1 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 1 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* TextMiningGUI
  checking examples ... ERROR
  checking whether package ‘TextMiningGUI’ can be installed ... WARNING

### Failed to check

* phylopath (NA)
