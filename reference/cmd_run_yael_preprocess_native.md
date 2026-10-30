# Run the native `YAEL` imaging processing workflow (without 'Python')

The image processing pipeline of
[`cmd_run_yael_preprocess`](http://rave.wiki/ravecore/reference/cmd_run_yael_preprocess.md)
([doi:10.1523/ENEURO.0328-23.2023](https://doi.org/10.1523/ENEURO.0328-23.2023)
), with every step computed in 'R' by
[`YAELNativeProcess`](http://rave.wiki/ravecore/reference/YAELNativeProcess.md)
on top of ravetools (version 0.3.3 or later) instead of `ANTs` through
'Python': cross-modality rigid registration, normalization of the
'T1'-weighted 'MRI' to template brains, and subject-level atlases from
the inverse normalization. The functions take the same arguments, run
the same steps and write the same files as `yael_preprocess` and
`cmd_run_yael_preprocess`, so the results can be read by either engine.

## Usage

``` r
yael_preprocess_native(
  subject,
  t1w_path = NULL,
  ct_path = NULL,
  t2w_path = NULL,
  fgatir_path = NULL,
  preopct_path = NULL,
  flair_path = NULL,
  t1w_contrast_path = NULL,
  register_policy = c("auto", "all"),
  register_reversed = FALSE,
  normalize_template = "mni_icbm152_nlin_asym_09b",
  normalize_policy = c("auto", "all"),
  normalize_images = c("T1w", "T2w", "T1wContrast", "fGATIR", "preopCT"),
  normalize_back = ifelse(length(normalize_template) >= 1, normalize_template[[1]], NA),
  normalize_affine = NULL,
  atlases = list(),
  add_surfaces = FALSE,
  use_antspynet = TRUE,
  verbose = TRUE,
  additional_images = list(),
  ...
)

cmd_run_yael_preprocess_native(
  subject,
  t1w_path = NULL,
  ct_path = NULL,
  t2w_path = NULL,
  fgatir_path = NULL,
  preopct_path = NULL,
  flair_path = NULL,
  t1w_contrast_path = NULL,
  register_reversed = FALSE,
  normalize_template = "mni_icbm152_nlin_asym_09b",
  normalize_images = c("T1w", "T2w", "T1wContrast", "fGATIR", "preopCT"),
  normalize_affine = NULL,
  run_recon_all = TRUE,
  dry_run = FALSE,
  use_antspynet = TRUE,
  verbose = TRUE,
  overwrite = FALSE,
  ...
)
```

## Arguments

- subject:

  subject ID

- t1w_path:

  path to the 'T1'-weighted preoperative 'MRI', used as underlay and
  base image. To have an `ACPC`-aligned scanner coordinate system,
  please align the image before feeding it into this function. All
  images must contain skulls (do not strip skulls)

- ct_path, t2w_path, fgatir_path, preopct_path, flair_path,
  t1w_contrast_path:

  additional optional images to be aligned to the underlay; the
  registration will be symmetric and the rigid-body transforms will be
  stored. Other images can be fed with named list `additional_images`

- register_policy:

  whether to skip already registered images; default is true (`'auto'`);
  set to `'all'` to ignore existing registrations and force calculation

- register_reversed:

  whether to swap the moving images and the fixing image; default is
  false

- normalize_template:

  template to normalize to: default is `'mni_icbm152_nlin_asym_09b'`
  (`MNI152b`, 0.5 mm resolution). The script of
  `cmd_run_yael_preprocess_native` switches to
  `'mni_icbm152_nlin_asym_09a'` (`MNI152a`, 1 mm resolution) when the
  computer memory is below 32 gigabytes (see 'Details'), and
  `yael_preprocess_native` gives a warning instead. Other choices are
  `'mni_icbm152_nlin_asym_09c'` and `'fsaverage'` (also known as
  `MNI305`)

- normalize_policy:

  whether to skip existing normalization, if calculated; default is
  `'auto'` (yes); set to `'all'` to ignore

- normalize_images:

  images used for normalization; default is to include common images
  before the implantation (if available)

- normalize_back:

  length of one (select from `normalize_template`), which template is to
  be used to generate native brain mask and transform matrices

- normalize_affine:

  optional path to an initial linear transform: the normalization then
  starts from this transform, and its first registration only runs the
  non-linear stage; for advanced users only

- atlases:

  a named list: the names must be template names from
  `normalize_template` and the values must be directories of atlases of
  the corresponding templates (see 'Examples').

- add_surfaces:

  whether to add surfaces for the subject; default is `FALSE`. The
  surfaces are created by reversing the normalization from the template
  brain, hence the results will not be accurate. Enable this option only
  if cortical surface estimation is not critical (and `FreeSurfer`
  reconstructions are inaccessible)

- use_antspynet:

  ignored: the native engine has no `deep_atropos`, and the tissue
  segmentation always uses the template priors; kept so that the
  arguments are the same as
  [`cmd_run_yael_preprocess`](http://rave.wiki/ravecore/reference/cmd_run_yael_preprocess.md)

- verbose:

  whether to print out the information; default is `TRUE`

- additional_images:

  named list offering additional image types that are not built-in. For
  example `'MEG'`

- ...:

  reserved for legacy code and deprecated arguments

- run_recon_all:

  whether to run `FreeSurfer`; default is true

- dry_run:

  whether to dry-run

- overwrite:

  whether the command should start over: when true, it removes the
  temporary folder `'tmp'` under the imaging folder, calculates the
  normalization again even if it exists, and deletes the existing
  'FreeSurfer' folder; default is false, which keeps existing
  normalization, and lets 'FreeSurfer' continue with its existing folder

## Value

`yael_preprocess_native` returns nothing; the subject imaging folder is
created under the 'RAVE' raw folder. It will take a while to run the
workflow. `cmd_run_yael_preprocess_native` returns, invisibly, a list
with the script, its path, the log file, the image paths, and a function
`execute` that runs the script; the script is saved as
`'scripts/cmd-yael-preprocess-native-full.R'` (or `'-simple.R'` without
`FreeSurfer`) under the imaging folder when it runs.

## Details

Templates are located (and downloaded when missing) by rpyANTs, whose
template functions are pure 'R'; 'Python' is never started. The full
template resolution is used, which takes a lot of memory. With a 1 mm
'T1' image (256 by 204 by 256 voxels), the normalization to
`'mni_icbm152_nlin_asym_09a'` (1 mm, with 0.5 mm tissue maps) took about
2 minutes and peaked at about 8 gigabytes of resident memory; the
normalization to `'mni_icbm152_nlin_asym_09b'` (0.5 mm) took about 7
minutes and peaked at 15 gigabytes of resident memory, 22 gigabytes with
the memory that the operating system compressed. Hence the script built
by `cmd_run_yael_preprocess_native` switches from
`'mni_icbm152_nlin_asym_09b'` to `'mni_icbm152_nlin_asym_09a'` when the
memory recorded in the 'RAVE' settings (`max_mem`, the computer memory
unless set otherwise) is below 32 gigabytes, where the script of
`cmd_run_yael_preprocess` switches below 12 gigabytes.
`yael_preprocess_native` does not switch templates; it warns when
`max_mem` is below 32 gigabytes and a template has more than 30 million
voxels.

The algorithms differ from `ANTs` in a few ways, described in
[`YAELNativeProcess`](http://rave.wiki/ravecore/reference/YAELNativeProcess.md):
no `deep_atropos`, local cross-correlation instead of mutual information
in the non-linear stage, greedy single-field `SyN`, whose update-field
smoothing takes the rpyANTs values (3 and 3.5) as a `Gaussian` standard
deviation in voxels where `ANTs` reads them as a variance, and is
increased for templates with more voxels per brain (doubled for the 0.5
mm templates) so that the deformation stays one-to-one; additional
images resampled onto the native 'T1' grid (and left out when they cover
less than 95 percent of the brain), and a binary template brain mask.
Streamlines in `VTK` format are not read or written, since ieegio needs
'Python' for them.

## See also

[`YAELNativeProcess`](http://rave.wiki/ravecore/reference/YAELNativeProcess.md),
[`as_yael_process`](http://rave.wiki/ravecore/reference/as_yael_process.md),
[`cmd_run_yael_preprocess`](http://rave.wiki/ravecore/reference/cmd_run_yael_preprocess.md)

## Examples

``` r

if (FALSE) { # \dontrun{

# For T1 normalization only; add ct_path to include coregistration
cmd_run_yael_preprocess_native(
  subject = "pt01",
  t1w_path = "/path/to/T1w.nii.gz",

  additional_images = list(
    MEG = "/path/to/meg.nii.gz"
  ),

  # normalize T1 to MNI152
  normalize_template = 'mni_icbm152_nlin_asym_09a'
)

} # }
```
