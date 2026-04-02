# Run a built-in 'YAEL' imaging processing workflow

Image processing pipeline
[doi:10.1523/ENEURO.0328-23.2023](https://doi.org/10.1523/ENEURO.0328-23.2023)
, allowing cross-modality image registration, T1-weighted MRI
normalization to template brain, creating subject-level brain atlas from
inverse normalization.

## Usage

``` r
yael_preprocess(
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
  atlases = list(),
  add_surfaces = FALSE,
  use_antspynet = TRUE,
  verbose = TRUE,
  ...
)

cmd_run_yael_preprocess(
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
  run_recon_all = TRUE,
  dry_run = FALSE,
  use_antspynet = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- subject:

  subject ID

- t1w_path:

  path to 'T1'-weighted preoperative 'MRI', used as underlay and base
  image. If you want to have 'ACPC' aligned scanner coordinate system.
  Please align the image before feeding into this function. All images
  must contain skulls (do not strip skulls)

- ct_path, t2w_path, fgatir_path, preopct_path, flair_path,
  t1w_contrast_path:

  additional optional images to be aligned to the underlay; the
  registration will be symmetric and the rigid-body transforms will be
  stored.

- register_policy:

  whether to skip already registered images; default is true (`'auto'`);
  set to `'all'` to ignore existing registrations and force calculation

- register_reversed:

  whether to swap the moving images and the fixing image; default is
  false

- normalize_template:

  template to normalize to: default is `'mni_icbm152_nlin_asym_09b'`
  ('MNI152b', 0.5 mm resolution); when the computer memory is below 12
  gigabytes, the template will automatically switch to
  `'mni_icbm152_nlin_asym_09a'` (known as 'MNI152a', 1 mm voxel
  resolution). Other choices are `'mni_icbm152_nlin_asym_09c'` and
  `'fsaverage'` (or known as 'MNI305')

- normalize_policy:

  whether to skip existing normalization, if calculated; default is
  `'auto'` (yes); set to `'all'` to ignore

- normalize_images:

  images used for normalization; default is to include common images
  before the implantation (if available)

- normalize_back:

  length of one (select from `normalize_template`), which template is to
  be used to generate native brain mask and transform matrices

- atlases:

  a named list: the names must be template names from
  `normalize_template` and the values must be directories of atlases of
  the corresponding templates (see 'Examples').

- add_surfaces:

  whether to add surfaces for the subject; default is `FALSE`. The
  surfaces are created by reversing the normalization from template
  brain, hence the results will not be accurate. Enable this option only
  if cortical surface estimation is not critical (and 'FreeSurfer'
  reconstructions are inaccessible)

- use_antspynet:

  whether to try `'antspynet'` if available; default is true, which uses
  `deep_atropos` instead of the conventional `atropos` to speed up and
  possibly with more accurate results.

- verbose:

  whether to print out the information; default is `TRUE`

- ...:

  reserved for legacy code and deprecated arguments

- run_recon_all:

  whether to run 'FreeSurfer'; default is true

- dry_run:

  whether to dry-run

## Value

Nothing, a subject imaging folder will be created under 'RAVE' raw
folder. It will take a while to run the workflow.

## Examples

``` r
if (FALSE) { # \dontrun{

# For T1 normalization only; add ct_path to include coregistration
cmd_run_yael_preprocess(
  subject = "pt01",
  t1w_path = "/path/to/T1w.nii.gz",

  # normalize T1 to MNI152
  normalize_template = 'mni_icbm152_nlin_asym_09b'
)


} # }
```
