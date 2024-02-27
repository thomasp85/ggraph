# ggdag

<details>

* Version: 0.2.11
* GitHub: https://github.com/r-causal/ggdag
* Source code: https://github.com/cran/ggdag
* Date/Publication: 2024-01-24 15:30:05 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "ggdag")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggdag)
      
      Attaching package: 'ggdag'
      
      The following object is masked from 'package:stats':
    ...
      • quick_plots/ggdag-collider-triangle-is-triangle-too.svg
      • quick_plots/ggdag-confounder-triangle-is-triangle.svg
      • relations/ggdag-ancestors-identifies-v-w1-and-z1.svg
      • relations/ggdag-descendants-identifies-y-x-and-z1.svg
      • relations/ggdag-parents-identifies-z2-x-w1-and-w2.svg
      • themes/theme-dag-gray-grid.svg
      • themes/theme-dag-gray.svg
      • themes/theme-dag-grid.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        doc    6.6Mb
        help   1.1Mb
    ```

