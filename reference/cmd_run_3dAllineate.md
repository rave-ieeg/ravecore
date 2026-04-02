# Align images using 'AFNI'

This is a legacy script and possibly contain errors. Please use
[`cmd_run_ants_coreg`](http://rave.wiki/ravecore/reference/cmd_run_ants_coreg.md)
for faster and stable implementation instead.

## Usage

``` r
cmd_run_3dAllineate(
  subject,
  mri_path,
  ct_path,
  overwrite = FALSE,
  command_path = NULL,
  dry_run = FALSE,
  verbose = dry_run
)
```

## Arguments

- subject:

  subject ID

- ct_path, mri_path:

  absolute paths to 'CT' and 'MR' image files

- overwrite:

  whether to overwrite existing files

- command_path:

  path to 'AFNI' home

- dry_run:

  dry-run flag

- verbose:

  whether to print out script
