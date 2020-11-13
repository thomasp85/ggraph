# TextMiningGUI

<details>

* Version: 0.1
* Source code: https://github.com/cran/TextMiningGUI
* URL: https://c0reyes.github.io/TextMiningGUI/
* Date/Publication: 2020-08-11 15:20:08 UTC
* Number of recursive dependencies: 158

Run `cloud_details(, "TextMiningGUI")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TextMiningGUI-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: TextMiningGUI
    > ### Title: TextMiningGUI
    > ### Aliases: TextMiningGUI
    > 
    > ### ** Examples
    > 
    > library(TextMiningGUI)
    > if(TextMiningGUI()){}
    Error in structure(.External(.C_dotTclObjv, objv), class = "tclObj") : 
      [tcl] invalid command name "toplevel".
    Calls: TextMiningGUI ... tktoplevel -> tkwidget -> tcl -> .Tcl.objv -> structure
    Execution halted
    ```

*   checking whether package ‘TextMiningGUI’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: loading Rplot failed
    See ‘/tmp/workdir/TextMiningGUI/new/TextMiningGUI.Rcheck/00install.out’ for details.
    ```

