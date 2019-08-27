# ggdag

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/ggdag
* URL: https://ggdag.netlify.com https://github.com/malcolmbarrett/ggdag
* BugReports: https://github.com/malcolmbarrett/ggdag/issues
* Date/Publication: 2018-03-27 19:13:32 UTC
* Number of recursive dependencies: 77

Run `revdep_details(,"ggdag")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > tidy_dagitty(dag) %>% dag_adjustment_sets()
    [90m# A tibble: 36 x 10[39m
       name      x     y direction to     xend  yend circular adjusted   set       
       [3m[90m<chr>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<fct>[39m[23m     [3m[90m<chr>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<lgl>[39m[23m    [3m[90m<chr>[39m[23m      [3m[90m<chr>[39m[23m     
    [90m 1[39m v      23.5  16.9 ->        z1     22.6  16.0 FALSE    unadjusted w1, w2, z2
    [90m 2[39m v      23.5  16.9 ->        z2     24.5  15.9 FALSE    unadjusted w1, w2, z2
    [90m 3[39m w1     23.4  15.1 ->        x      22.5  14.7 FALSE    adjusted   w1, w2, z2
    [90m 4[39m w1     23.4  15.1 ->        y      23.9  14.6 FALSE    adjusted   w1, w2, z2
    [90m 5[39m w1     23.4  15.1 ->        z1     22.6  16.0 FALSE    adjusted   w1, w2, z2
    [90m 6[39m w1     23.4  15.1 <->       w2     24.8  14.9 FALSE    adjusted   w1, w2, z2
    [90m 7[39m w2     24.8  14.9 ->        y      23.9  14.6 FALSE    adjusted   w1, w2, z2
    [90m 8[39m w2     24.8  14.9 ->        z2     24.5  15.9 FALSE    adjusted   w1, w2, z2
    [90m 9[39m x      22.5  14.7 ->        y      23.9  14.6 FALSE    unadjusted w1, w2, z2
    [90m10[39m z1     22.6  16.0 ->        x      22.5  14.7 FALSE    unadjusted w1, w2, z2
    [90m# … with 26 more rows[39m
    > 
    > ggdag_adjustment_set(dag)
    Error in -params$strength : invalid argument to unary operator
    Calls: <Anonymous> ... f -> <Anonymous> -> f -> <Anonymous> -> f -> create_arc
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggforce’ ‘plyr’
      All declared Imports should be used.
    ```

# phylopath

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/phylopath
* URL: http://Ax3man.github.io/phylopath/
* BugReports: https://github.com/Ax3man/phylopath/issues
* Date/Publication: 2019-07-12 21:30:03 UTC
* Number of recursive dependencies: 76

Run `revdep_details(,"phylopath")` for more info

</details>

## Newly broken

*   checking Rd cross-references ... WARNING
    ```
    Missing link or links in documentation object 'plot.DAG.Rd':
      ‘[ggraph:create_layout.igraph]{ggraph::create_layout.igraph()}’
    
    Missing link or links in documentation object 'plot.fitted_DAG.Rd':
      ‘[ggraph:create_layout.igraph]{ggraph::create_layout.igraph()}’
    
    See section 'Cross-references' in the 'Writing R Extensions' manual.
    ```

# visNetwork

<details>

* Version: 2.0.7
* Source code: https://github.com/cran/visNetwork
* URL: http://datastorm-open.github.io/visNetwork/
* BugReports: https://github.com/datastorm-open/visNetwork/issues
* Date/Publication: 2019-05-27 17:00:02 UTC
* Number of recursive dependencies: 82

Run `revdep_details(,"visNetwork")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘ggraph::den_to_igraph’
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.1Mb
      sub-directories of 1Mb or more:
        doc           4.2Mb
        docjs         1.4Mb
        htmlwidgets   3.9Mb
    ```

