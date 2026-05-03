# Convert DICOM to NIfTI via `'dcm2niix'`

Check <https://rave.wiki> on how to set up `'conda'` environment for
'RAVE' using `'ravemanager'`.

## Usage

``` r
cmd_run_dcm2niix(
  subject,
  src_path,
  type = c("MRI", "CT"),
  merge = c("Auto", "No", "Yes"),
  float = c("Yes", "No"),
  crop = c("No", "Yes", "Ignore"),
  overwrite = FALSE,
  command_path = NULL,
  dry_run = FALSE,
  verbose = dry_run
)
```

## Arguments

- subject:

  'RAVE' subject or a subject ID

- src_path:

  source directory

- type:

  image type

- merge, float, crop:

  `'dcm2niix'` parameters

- overwrite:

  overwrite existing files

- command_path:

  path to program `'dcm2niix'`

- dry_run:

  whether to dry-run

- verbose:

  whether to print out command

## Value

A command set running the terminal command; a folder named with `type`
will be created under the subject image input folder

## Examples

``` r

if (FALSE) { # \dontrun{

cmd_run_dcm2niix(
  "YAEL/pt02",
  "/path/to/DICOMDIR",
  "MRI"
)

} # }
```
