## Test environments
* local OS X install, R 3.3.3
* ubuntu 14.04.5 LTS (on travis-ci), R 3.4.1
* win-builder

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs for OS X and ubuntu.

I used `devtools::build_win()` to run on win-builder. The tail of my 
`00check.log` reads as follows. I'm not sure what to make of that. 
The `00install.out` seems to indicate that the package installs correctly.

```
* checking tests ...
** running tests for arch 'i386' ... ERROR
Check process probably crashed or hung up for 20 minutes ... killed
Most likely this happened in the example checks (?),
if not, ignore the following last lines of example output:
======== End of example output (where/before crash/hang up occured ?) ========
```


## Downstream dependencies

There are currently no downstream dependencies for this package.

