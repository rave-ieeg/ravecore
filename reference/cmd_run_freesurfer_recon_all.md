# Workflow: 'FreeSurfer' surface reconstruction

Runs 'FreeSurfer' `recon-all` command underneath; must have 'FreeSurfer'
installed.

## Usage

``` r
cmd_run_freesurfer_recon_all(
  subject,
  mri_path,
  args = c("-all", "-autorecon1", "-autorecon2", "-autorecon3", "-autorecon2-cp",
    "-autorecon2-wm", "-autorecon2-pial"),
  work_path = NULL,
  overwrite = FALSE,
  command_path = NULL,
  dry_run = FALSE,
  verbose = dry_run
)

cmd_run_freesurfer_recon_all_clinical(
  subject,
  mri_path,
  work_path = NULL,
  overwrite = FALSE,
  command_path = NULL,
  dry_run = FALSE,
  verbose = dry_run,
  ...
)
```

## Arguments

- subject:

  'RAVE' subject or subject ID

- mri_path:

  path to 'T1'-weighted 'MRI', must be a 'NIfTI' file

- args:

  type of workflow; see 'FreeSurfer' `recon-all` command documentation;
  default choice is `'-all'` to run all workflows

- work_path:

  working directory; 'FreeSurfer' errors out when working directory
  contains white spaces. By default, 'RAVE' automatically creates a
  symbolic link to a path that contains no white space. Do not set this
  input manually unless you know what you are doing

- overwrite:

  whether to overwrite existing work by deleting the folder; default is
  false. In case of errors, set this to true to restart the workflow;
  make sure you back up the files first.

- command_path:

  'FreeSurfer' home directory. In some cases, 'RAVE' might not be able
  to find environment variable `'FREESURFER_HOME'`. Please manually set
  the path if the workflow fails. Alternatively, you can manually set
  FreeSurfer' home directory via 'RAVE' options
  [`raveio_setopt("freesurfer_path", "/path/to/freesurfer/home")`](http://dipterix.org/ravepipeline/reference/raveio-option.md)
  prior to running the script

- dry_run:

  avoid running the code, but print the process instead

- verbose:

  print messages

- ...:

  ignored

## Value

A list of shell command set.

## Examples

``` r
# Requires `FreeSurfer` and only works on MacOS or Linux
# as `FreeSurfer` does not support Windows

if (FALSE) { # \dontrun{

# Create subject instance; strict=FALSE means it's OK if the subject
# is missing
subject <- as_rave_subject("YAEL/s01", strict = FALSE)

cmd_run_freesurfer_recon_all(
  subject = subject,
  mri_path = "/path/to/T1.nii.gz"
)

} # }
```
