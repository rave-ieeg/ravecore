# Extract the brain from a 'T1'-weighted image (without 'Python')

The brain mask of a 'T1'-weighted 'MRI', computed in 'R' by the native
`YAEL` engine on top of ravetools: the brain mask of the template
`'mni_icbm152_nlin_asym_09a'`, brought to the image by a registration
(or a given initial mask), is adjusted to the subject.

## Usage

``` r
yael_brain_extract(
  image,
  mask = NULL,
  work_path = NULL,
  affine_transform = NULL,
  verbose = TRUE
)
```

## Arguments

- image:

  the 'T1'-weighted image: a file path, an `ieegio_volume`, or anything
  else that
  [`ieegio::as_ieegio_volume`](http://dipterix.org/ieegio/reference/as_ieegio_volume.md)
  accepts

- mask:

  optional initial brain mask in the same scanner space (the voxels
  above zero), in the same forms as `image`; when given, the template
  and its registration are not used. A mask on another grid, such as a
  'FreeSurfer' `brainmask.mgz`, is resampled onto the grid of `image`
  (nearest neighbor)

- work_path:

  folder for the working files, which are kept; default `NULL` uses a
  temporary folder that is removed at the end

- affine_transform:

  optional initial `affine` transform from the template to the image
  (scanner 'RAS' to scanner 'RAS', the point map of `'ANTs'`): an
  `'ANTs'` `.mat` file or a 4-by-4 matrix; the registration then only
  runs its non-linear stage. Ignored when `mask` is given

- verbose:

  whether to log the steps and print the registration progress

## Value

A binary `ieegio_volume` (1 inside the brain) on the grid of `image`.

## Details

The steps, with the files they write into `work_path` (named after the
stages of the native normalization):

1.  bias-field correction with the settings of `abp_n4` in 'ANTsPy'
    (`nu.nii.gz`), for the registration;

2.  unless `mask` is given, the registration of the whole head to the
    template image without masks, affine and then deformable (only
    deformable from `affine_transform`), as the initial registration
    (`'SyNabp'`) of the native normalization; it is saved by
    [`ravetools::save_registration`](https://dipterix.org/ravetools/reference/save_registration.html)
    as `SyNabp.dcf` with its `'ANTs'` transform files, and the
    registered image as `SyNabp_warped_mov.nii.gz`;

3.  the initial mask: the template brain mask through that registration,
    or `mask` (`initial_mask.nii.gz`);

4.  the brain extraction of the native engine (see the item "Brain
    extraction" of
    [`YAELNativeProcess`](http://rave.wiki/ravecore/reference/YAELNativeProcess.md))
    on the image corrected by `N4` again without the intensity
    truncation of `abp_n4` (which makes bright marrow look like the
    white matter), which keeps the largest connected gray and white
    matter, leaves out what dark voxels cut off from it (skull marrow,
    dura or scalp) and thin strands, and keeps the initial mask, with a
    warning, when the result differs from it by more than 20 percent in
    volume (`brainmask.nii.gz`).

Every call computes all the steps again; existing files in `work_path`
are replaced.

## Examples

``` r
if (FALSE) { # \dontrun{

# downloads the template on first use, and takes a few minutes
mask <- yael_brain_extract("sub-01_T1w.nii.gz", work_path = "brain_extraction")
ieegio::write_volume(mask, "sub-01_desc-brain_mask.nii.gz")

# adjust a mask of the same image instead
mask2 <- yael_brain_extract("sub-01_T1w.nii.gz", mask = "fs/mri/brainmask.mgz")

} # }
```
