## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.


In the meanwhile, here are the fixes to the comments:

```
please make sure to only write package/software names in single quotes in the description.
```

Thanks, I removed the single quotes in the `DESCRIPTION` and left only packages and software names in single quotes. 

* `RAVE` - software containing multiple packages
* `YAEL` - software for electrode localization
* `Matlab` - software
* `FreeSurfer` - external third-party package
* `threeBrain` - R package

I did not find explicit quotation rules for the function titles. However, for consistency, I also did the same to the the 'Title' field in function documentations. Hopefully this complies with the `CRAN` policy and does not add burdens to you.

* `AFNI` - third-party external program set
* `NiftyReg` - image registration library
* `ANTs` - advanced image normalization library
* `nipy` - an external `Python` package
* `conda` - third-party external package management system
* `dcm2niix` - third-party external program
* `RStudio` - software


```
Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited.
e.g.:
...
oldpar <- par(no.readonly = TRUE) # code line i
on.exit(par(oldpar)) # code line i + 1
...
par(mfrow=c(2,2)) # somewhere after
...
...
oldwd <- getwd() # code line i
on.exit(setwd(oldwd)) # code line i+1
...
setwd(...) # somewhere after
...
e.g.: -> R/workflow-archive-subject.R

If you're not familiar with the function, please check ?on.exit. This function makes it possible to restore options before exiting a function even if the function breaks. Therefore it needs to be called immediately after the option change within a function.
For more details: <https://contributor.r-project.org/cran-cookbook/code_issues.html#change-of-options-graphical-parameters-and-working-directory>

Please fix and resubmit.

Best,
Konstanze Lauseker (she/her)
```

Thanks for pointing it out. I have been very cautious on "breaking the user space".

For `par`, all the calls to `par` are properly closed as instructed.

For `setwd`, there is indeed one function under `R/workflow-archive-subject.R` that sets the working directory. The function call with `setwd` at Line 423 is immediately followed by `on.exit`, leaving Line-438 the only line violating the `i+1` rule. 

However, I believe the working directory is properly handled here and hence this is a false-positive. Here is the reason:

At Line-438, `setwd(current_wd)` is an additional line that *resets* the working directory to where it should be. Basically it calls the exiting expression located at Line-424 early because the rest of the script assumes the previous working directory. 

Since Line-438 resets the working directory, there is no need to call `on.exit`. 

For your convenience, here is the function outline. You can see that I also documented very carefully at Line 436 and 437 for this matter.

```r

L-055 archive_subject <- function(...) {

  ...

L-423   setwd(dirname(root_dir))
L-424   on.exit({ setwd(current_wd) }, add = TRUE, after = FALSE)

  ...

L-436   # This is to make sure the work directory is set back to current directory
L-437   # hence no on.exit is needed, see Line 423
L-438   setwd(current_wd)

  ...
  
L-543 }
```



