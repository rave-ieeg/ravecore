# Rigidly register a computerized tomography (CT) image to MRI with the built-in `YAEL-reg3d` engine

Aligns the 'CT' to the 'MRI' with
[`register_volume3d`](https://dipterix.org/ravetools/reference/register_volume3d.html),
which is built into 'RAVE' and needs no external program. The images are
registered and saved at their original resolution. Please use
`cmd_run_reg3d_rigid` from pipelines: it saves a script that runs
`reg3d_rigid` and records the results for the electrode localization.

## Usage

``` r
reg3d_rigid(
  ct_path,
  mri_path,
  coreg_path,
  reg_type = c("Rigid", "DenseRigid", "FastRigid"),
  cost = c("mattes", "cc"),
  interp = c("trilinear", "bspline", "nearest"),
  verbose = TRUE,
  ...
)

cmd_run_reg3d_rigid(
  subject,
  ct_path,
  mri_path,
  reg_type = c("Rigid", "DenseRigid", "FastRigid"),
  cost = c("mattes", "cc"),
  interp = c("trilinear", "bspline", "nearest"),
  verbose = TRUE,
  dry_run = FALSE
)
```

## Arguments

- ct_path, mri_path:

  absolute paths to 'CT' and 'MR' image files

- coreg_path:

  directory where the results are saved; required. An existing directory
  is backed up (renamed) once the new results are complete, so a failed
  registration leaves it untouched

- reg_type:

  registration preset; choices are `'Rigid'` (three resolution levels,
  20 percent of the voxels sampled), `'DenseRigid'` (same levels, every
  voxel sampled; slower), or `'FastRigid'` (coarser levels, fewer
  iterations, 5 percent of the voxels sampled; fast)

- cost:

  cost function; choices are `'mattes'` (mutual information, suitable
  for 'CT' to 'MRI') or `'cc'` (normalized cross-correlation, for images
  of the same modality)

- interp:

  interpolation used to re-sample the 'CT' onto the 'MRI' grid
  (`'ct_in_t1.nii.gz'`); choices are `'trilinear'`, `'bspline'`, or
  `'nearest'`; the alignment itself does not depend on it

- verbose:

  whether to print the registration progress

- ...:

  passed to
  [`register_volume3d`](https://dipterix.org/ravetools/reference/register_volume3d.html);
  overrides the preset, for example `iterations` or `sampling_rate`

- subject:

  'RAVE' subject

- dry_run:

  whether to only build the script without running it; default is false

## Value

`reg3d_rigid` invisibly returns a list with the result directory
(`coreg_path`) and the names of the files in it:

- `'CT_RAW.nii.gz'`, `'MRI_reference.nii.gz'`:

  the 'CT' and 'MR' images at their original resolution

- `'ct_in_t1.nii.gz'`:

  the 'CT' re-sampled onto the grid of `'MRI_reference.nii.gz'`

- `'CT_RAS_to_MR_RAS.txt'`:

  transform from the 'CT' scanner 'RAS' to the 'MR' scanner 'RAS'
  coordinates

- `'CT_IJK_to_MR_RAS.txt'`:

  transform from the voxel indices of `'CT_RAW.nii.gz'` (starting
  from 0) to the 'MR' scanner 'RAS' coordinates

- `'reg3d/reg3d.dcf'`:

  registration record written by
  [`save_registration`](https://dipterix.org/ravetools/reference/save_registration.html),
  with the 'ANTs'-style affine transform
  `'reg3d/reg3d0GenericAffine.mat'`

`cmd_run_reg3d_rigid` returns a list with the script, its path, the log
file, the image paths, and a function `execute` that runs the script.
