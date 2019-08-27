# EcoNetGen

<details>

* Version: 0.2.3
* Source code: https://github.com/cran/EcoNetGen
* URL: https://github.com/cboettig/EcoNetGen
* BugReports: https://github.com/cboettig/EcoNetGen/issues
* Date/Publication: 2019-07-13 23:30:14 UTC
* Number of recursive dependencies: 73

Run `revdep_details(,"EcoNetGen")` for more info

</details>

## In both

*   checking whether package ‘EcoNetGen’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/thomas/Dropbox/GitHub/ggraph/revdep/checks.noindex/EcoNetGen/new/EcoNetGen.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘EcoNetGen’ ...
** package ‘EcoNetGen’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
gfortran  -fPIC  -Wall -g -O2  -c  FortranNetGen.f90 -o FortranNetGen.o
gfortran  -fPIC  -Wall -g -O2  -c  FortranSampling.f90 -o FortranSampling.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c zzz.c -o zzz.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o EcoNetGen.so FortranNetGen.o FortranSampling.o zzz.o -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [EcoNetGen.so] Error 1
ERROR: compilation failed for package ‘EcoNetGen’
* removing ‘/Users/thomas/Dropbox/GitHub/ggraph/revdep/checks.noindex/EcoNetGen/new/EcoNetGen.Rcheck/EcoNetGen’

```
### CRAN

```
* installing *source* package ‘EcoNetGen’ ...
** package ‘EcoNetGen’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
gfortran  -fPIC  -Wall -g -O2  -c  FortranNetGen.f90 -o FortranNetGen.o
gfortran  -fPIC  -Wall -g -O2  -c  FortranSampling.f90 -o FortranSampling.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c zzz.c -o zzz.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o EcoNetGen.so FortranNetGen.o FortranSampling.o zzz.o -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin15/6.1.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [EcoNetGen.so] Error 1
ERROR: compilation failed for package ‘EcoNetGen’
* removing ‘/Users/thomas/Dropbox/GitHub/ggraph/revdep/checks.noindex/EcoNetGen/old/EcoNetGen.Rcheck/EcoNetGen’

```
# ReactomePA

<details>

* Version: 
* Source code: ???
* URL: https://github.com/thomasp85/ggraph
* BugReports: https://github.com/thomasp85/ggraph/issues
* Number of recursive dependencies: 0

Run `revdep_details(,"")` for more info

</details>

## Error before installation

### Devel

```

  There are binary versions available but the source versions are later:
         binary source needs_compilation
bookdown   0.12   0.13             FALSE
sys         3.2    3.3              TRUE

  Binaries will be installed


installing the source packages ‘bookdown’, ‘reactome.db’



```
### CRAN

```

  There are binary versions available but the source versions are later:
         binary source needs_compilation
bookdown   0.12   0.13             FALSE
sys         3.2    3.3              TRUE

  Binaries will be installed


installing the source packages ‘bookdown’, ‘reactome.db’



```
