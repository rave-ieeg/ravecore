# Run YAEL-based FreeSurfer reconstruction for macaque brains

A two-stage pipeline that adapts human cortical reconstruction workflow
for `ACPC`-aligned macaque T1w MRI.

## Usage

``` r
yael_macaque(
  subject,
  t1w_path,
  folder_name = "fs",
  steps = c("normalization", "reconstruction")
)
```

## Arguments

- subject:

  character or RAVE subject object. The subject identifier in
  `"project/subject"` format (e.g. `"YAEL/Snorlax"`), or an object
  returned by
  [`as_rave_subject`](http://rave.wiki/ravecore/reference/new_rave_subject.md).

- t1w_path:

  character. Absolute path to the T1-weighted NIfTI image (`.nii` or
  `.nii.gz`). Non-NIfTI formats are accepted and will be converted
  automatically. Only used by the `"normalization"` step.

- folder_name:

  character. Name of the FreeSurfer subject folder created under the
  subject imaging directory. Defaults to `"fs"`.

- steps:

  character vector. Which stages to run. One or both of
  `"normalization"` and `"reconstruction"`, matched with
  [`match.arg`](https://rdrr.io/r/base/match.arg.html). Defaults to
  both. Pass a single value to resume from stage 2 without rerunning
  stage 1.

## Value

Called for its side effects. Returns `NULL` invisibly. On success the
FreeSurfer subject directory at `<imaging_path>/<folder_name>/` is
populated with `surf/`, `mri/`, `label/` and related child directories,
including `lh.white`, `rh.white`, `lh.pial`, and `rh.pial`.

## Details

To run this workflow, a dedicated `Python` library and `FreeSurfer`
library must be installed. The binaries are not shipped with this
package. Please use `ravemanager::configure_python()` to configure the
`Python` environment. For `FreeSurfer`, version `8.0.0` has been tested.
Other versions should work too.

`RAVE` looks for `FreeSurfer` using option `'freesurfer_path'` and
please use `ravepipeline::raveio_setopt('freesurfer_path', 'path')` to
allow `RAVE` to access the `FreeSurfer` home.

The workflow consists of two stages.

Stage 1 (`"normalization"`) runs YAEL normalization against the `ACPC`
aligned `NMT v2` template, crops and rescales the image into a space
that satisfies FreeSurfer atlas assumptions, and writes all intermediate
imaging inputs under `'folder_name/mri/raw/'`. This stage takes around
10 min to run.

Stage 2 (`"reconstruction"`) drives individual FreeSurfer binaries (no
`recon-all`) to build white-matter and `pial` surfaces, bypassing steps
known to fail on macaque data (human `GCA` atlas registration,
`mri_fill` seed placement). This stage takes around one to three hours
to finish, depending on the computing power and normalization quality.

## Examples

``` r

if (FALSE) { # \dontrun{
# Full pipeline
yael_macaque(
  subject  = "YAEL/Snorlax",
  t1w_path = "/data/macaque/native_acpc_aligned.nii.gz"
)

# Resume from reconstruction only (normalization already done)
yael_macaque(
  subject     = "YAEL/Snorlax",
  t1w_path    = "/data/macaque/native_acpc_aligned.nii.gz",
  folder_name = "fs",
  steps       = "reconstruction"
)

} # }
```
