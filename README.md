# fair

Reproduce results from YAHPO Gym fair.

## Installation

You can install the released version from GitHub using:

``` r
remotes::install_github("sumny/fair")
```

Note that for *exact* reproduction, you need the R packages with the exact version as specified in the DESCRIPTION
(i.e., == instead of >=) and also R version 4.0.5.
Also note that some measures (ram and time related ones) can never be exactly reproduced because they depend on system
hardware and system load.

By default, in all `eval_*` functions, the original fixed seed is used and therefore, runs are fully deterministic,
(except for the ram and time measures mentioned above).

To change this, simply provide a custom seed.

Note that data is obtained via `mlr3oml` and is cached via `qs`.

## Example

```r
library(fair)
scenario = "fair_rpart"
configuration = list(cp = 0.5, maxdepth = 7, minbucket = 50, minsplit = 20, pre_post = "pre", reweighing_os_alpha = 0.05, trainsize = 0.11, task_id = "190424")
eval_yahpo(scenario, configuration)
```
