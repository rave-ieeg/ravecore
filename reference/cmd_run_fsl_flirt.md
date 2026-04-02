# Run 'FSL' linear registration

Run 'FSL' linear registration

## Usage

``` r
cmd_run_fsl_flirt(
  subject,
  mri_path,
  ct_path,
  dof = 6,
  cost = c("mutualinfo", "leastsq", "normcorr", "corratio", "normmi", "labeldiff", "bbr"),
  search = 90,
  searchcost = c("mutualinfo", "leastsq", "normcorr", "corratio", "normmi", "labeldiff",
    "bbr"),
  overwrite = FALSE,
  command_path = NULL,
  dry_run = FALSE,
  verbose = dry_run
)
```

## Arguments

- subject:

  'RAVE' subject or subject ID

- mri_path:

  path to 'MRI' (fixed image)

- ct_path:

  path to 'CT' (moving image)

- dof:

  degrees of freedom; default is 6 (rigid-body); set to 12 ('affine')

- cost, searchcost:

  cost function name

- search:

  search degrees; default is 90 to save time, set to 180 for full search

- overwrite:

  overwrite existing files

- command_path:

  path to `'FSLDIR'` environment variable

- dry_run:

  whether to dry-run

- verbose:

  whether to print out command

## Value

A command set running the terminal command; a `'coregistration'` folder
will be created under the subject imaging directory
