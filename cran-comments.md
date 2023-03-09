# **prefio** 0.1.0

This submission contains the initial release of **prefio**.

## Addressing comments from previous submission:

```
If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
auto-linking. (If you want to add a title as well please put it in
quotes: "Title")
```

I have added one `doi` link to the DESCRIPTION, but there were two references
in the `.Rd` files. The other reference is for the Netflix Prize, which refers
only to the data fetched in the `dontrun` example.

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
