# **prefio** 0.1.0

This submission contains the initial release of **prefio**.

## `R CMD check` results

#### GitHub Actions (via `usethis::use_github_action_check_standard()`):

* ✅ macOS (12.6.2 21G320), R==4.2.2
* ✅ Windows Server (2022 10.0.20348), R==4.2.2-win
* ✅ Ubuntu (20.04.2), R==4.1.3
* ✅ Ubuntu (20.04.2), R==4.2.2
* ✅ Ubuntu (20.04.2), R==devel

`0 errors ✔ | 0 warnings ✔ | 0 notes ✔`

#### Other **Rhub** checks

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
