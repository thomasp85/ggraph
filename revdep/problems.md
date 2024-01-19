# LabApplStat

<details>

* Version: 1.4.3
* GitHub: NA
* Source code: https://github.com/cran/LabApplStat
* Date/Publication: 2021-11-29 09:40:06 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "LabApplStat")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘LabApplStat-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: DD
    > ### Title: Design diagram for a linear model
    > ### Aliases: DD
    > ### Keywords: design manip
    > 
    > ### ** Examples
    > 
    ...
    > 
    > #Making the factor diagram closed under minima
    > mydata <- data.frame(age=rep(c("boy","girl","adult","adult"),4),
    +                      gender=rep(c("child","child","man","woman"),4))
    > myDD <- DD(~0+age+gender,data=mydata)
    > plot(myDD)
    Error in unit(attr(ggraph::label_rect(text0, fontsize = 18), "width"),  : 
      'x' and 'units' must have length > 0
    Calls: <Anonymous> ... <Anonymous> -> convertUnit -> upgradeUnit -> is.newUnit -> unit
    Execution halted
    ```

# migraph

<details>

* Version: 0.12.0
* GitHub: https://github.com/snlab-ch/migraph
* Source code: https://github.com/cran/migraph
* Date/Publication: 2022-09-27 07:50:02 UTC
* Number of recursive dependencies: 131

Run `cloud_details(, "migraph")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─base::as.character(test_networkers[["layers"]][[2]][["aes_params"]][["end_cap"]])
        5. └─vctrs:::as.character.vctrs_vctr(test_networkers[["layers"]][[2]][["aes_params"]][["end_cap"]])
        6.   └─vctrs::vec_cast(x, character())
        7.     └─vctrs (local) `<fn>`()
        8.       └─vctrs::vec_default_cast(...)
        9.         └─vctrs::stop_incompatible_cast(...)
       10.           └─vctrs::stop_incompatible_type(...)
       11.             └─vctrs:::stop_incompatible(...)
       12.               └─vctrs:::stop_vctrs(...)
       13.                 └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 520 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc   3.0Mb
    ```

