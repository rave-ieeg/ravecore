## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

In addition:

1. I tried to remove `dontrun` and create toy examples as much as possible. However, due to the fact that this package works with `RAVE` format - typically several `GB` per subject, it is extremely hard to do so for every examples.  

2. In the previous submission,

```
On 11.09.2025 20:21, Dipterix wrote:
Dear CRAN team,
I was trying to fix these issues to comply with the CRAN policy.

Flavor: r-devel-linux-x86_64-debian-gcc
Check: for detritus in the temp directory, Result: NOTE
 Found the following files/directories:
   'calibre-3w338n25'

However, it is unclear to me why this file (calibre-3w338n25) was created:


This suggests you started a web browser. You should not do that in non interactive mode. Please protect such call by if(interactive())

Please fix and resubmit.
```

This was caused by `htmltools` printing `HTML` tags. The problematic example has been fixed.
