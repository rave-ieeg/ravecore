# Generate `YBA` atlas for a 'RAVE' subject

Morphs a `YBA` atlas from the `MNI152` symmetric template to the subject
native space, calculates the anatomical labels for the electrode
contacts, and optionally creates a quality-control report. This function
requires the subject to be normalized to a `MNI152` symmetric template
first; see
[`cmd_run_yael_preprocess`](http://rave.wiki/ravecore/reference/cmd_run_yael_preprocess.md).

## Usage

``` r
generate_atlas_YBA(
  subject,
  name = c("YBA690", "YBA696"),
  template_names = NULL,
  radius = 2,
  verbose = TRUE,
  create_report = TRUE,
  disable_viewer = TRUE
)
```

## Arguments

- subject:

  'RAVE' subject instance or character; see
  [`as_rave_subject`](http://rave.wiki/ravecore/reference/new_rave_subject.md)

- name:

  atlas name; choices are `"YBA690"` (default) and `"YBA696"`

- template_names:

  template names to search for existing non-linear mappings; default is
  `NULL`, which uses the `MNI152` symmetric templates; the first
  template with a valid mapping will be used

- radius:

  search radius, in millimeters, when assigning the atlas labels to the
  electrode contacts; default is `2`

- verbose:

  whether to print out the progress; default is `TRUE`

- create_report:

  whether to create a quality-control report; default is `TRUE`

- disable_viewer:

  whether to omit the embedded three-dimensional viewer in the report;
  default is `TRUE`. Please be aware that enabling the viewer results in
  a self-contained report that can be hundreds of megabytes large.

## Value

An invisible named list of

- `atlas_path`:

  path to the atlas volume created under the subject `FreeSurfer`
  `'mri'` folder

- `label_path`:

  path to the electrode label table, or `NA` if no electrode contact has
  been localized

- `colormap_path`:

  path to the color lookup table saved along with the atlas volume

- `report_path`:

  path to the report folder, or `NA` if `create_report` is false

- `report_file`:

  path to the report file, or `NA` if `create_report` is false

## Details

The workflow requires an existing non-linear normalization from the
subject native `T1w` image to a `MNI152` symmetric template. Please run
[`cmd_run_yael_preprocess`](http://rave.wiki/ravecore/reference/cmd_run_yael_preprocess.md)
with `normalize_template` set to one of `"mni_icbm152_nlin_sym_09a"`,
`"mni_icbm152_nlin_sym_09b"`, or `"mni_icbm152_nlin_sym_09c"` if such
mapping is missing.

Given the mapping, the template atlas is inverse-transformed to the
native space via
[`generate_atlases_from_template`](http://rave.wiki/ravecore/reference/generate_atlases_from_template.md).
The resulting `NIfTI` volume is stored under the subject `'atlases'`
imaging folder, with a copy (and its color lookup table) placed under
the subject `FreeSurfer` `'mri'` folder such that the atlas can be
displayed by the threeBrain viewer.

When the subject has electrode contacts localized, the anatomical labels
are calculated within a sphere of `radius` millimeters around each
contact center, and are stored as `'electrode_atlas_<name>.csv'` under
the subject meta folder.

## See also

[`generate_atlases_from_template`](http://rave.wiki/ravecore/reference/generate_atlases_from_template.md),
[`cmd_run_yael_preprocess`](http://rave.wiki/ravecore/reference/cmd_run_yael_preprocess.md)

## Examples

``` r

# Please check out https://rave.wiki to configure Python for RAVE
# or run ravemanager::configure_python()
if (FALSE) { # \dontrun{

subject_id <- "YAEL/subject_code"

# ---- Step 1: normalization ------------------------------------------
# Map the native `T1w` image to a `MNI152` symmetric template. This step
# is only needed once per subject; skip it when the mapping is available
cmd_run_yael_preprocess(
  subject = subject_id,
  t1w_path = "/path/to/T1w.nii.gz",

  # `YBA` atlases are defined in the `MNI152` symmetric space
  normalize_template = "mni_icbm152_nlin_sym_09b",

  # set to `TRUE` to also run `FreeSurfer` reconstruction
  # if you haven't...
  run_recon_all = FALSE
)

# ---- Step 2: atlas, electrode labels, and report ---------------------
# Morph the atlas back to the native space, label the electrode contacts,
# and create the quality-control report
results <- generate_atlas_YBA(subject_id, name = "YBA696")

# ---- Step 3: inspect --------------------------------------------------
# Electrode labels: `results$label_path`
utils::read.csv(results$label_path)

# Quality-control report
utils::browseURL(results$report_file)

} # }
```
