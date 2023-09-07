# **prefio** 0.1.1

This submission contains the second release of **prefio**.

## `R CMD check` results

#### GitHub Actions (via `usethis::use_github_action_check_standard()`):

* ✅ macOS (12.6.7 21G651), R==4.3.1
* ✅ Windows Server (2022 10.0.20348), R==4.3.1-win
* ✅ Ubuntu (20.04.3), R==4.2.3
* ✅ Ubuntu (20.04.3), R==4.3.1
* ✅ Ubuntu (20.04.3), R==devel

`0 errors ✔ | 0 warnings ✔ | 0 notes ✔`


#### **Rhub** checks

```R
rhub::check_for_cran()
```

We see three notes:
* One identifying this as a new submission:
  ```
  N  checking CRAN incoming feasibility
     Maintainer: 'Floyd Everest <me@floydeverest.com>'

     New submission
  ```
* Another which is unrelated to prefio:
  ```
  N  checking HTML version of manual
     Skipping checking math rendering: package 'V8' unavailable
  ```

* The last is related to an [ongoing issue](https://github.com/r-hub/rhub/issues/503) on Rhub:
  ```
  N  checking for detritus in the temp directory
     Found the following files/directories:
       'lastMiKTeXException'
  ```
