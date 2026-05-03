# Create brain atlases from template

Reverse-transform from template atlases via volume mapping

## Usage

``` r
generate_atlases_from_template(
  subject,
  atlas_folders,
  template_name = "mni_icbm152_nlin_asym_09b",
  any_mni152 = TRUE,
  surfaces = TRUE,
  as_job = FALSE,
  extra_transform = NULL,
  extra_transform_type = c("ants", "native")
)
```

## Arguments

- subject:

  'RAVE' subject instance or character; see
  [`as_rave_subject`](http://rave.wiki/ravecore/reference/new_rave_subject.md)

- atlas_folders:

  paths to the atlas folders

- template_name:

  template name where the atlases are created; see
  [`get_available_morph_to_template`](http://rave.wiki/ravecore/reference/get_available_morph_to_template.md)
  to get available templates and
  [`cmd_run_yael_preprocess`](http://rave.wiki/ravecore/reference/cmd_run_yael_preprocess.md)
  to generate transforms to the templates

- any_mni152:

  if the template is 'MNI152', then whether to check other template
  folders; default is true

- surfaces:

  whether to generate surfaces; if true, then a 'GIfTI' file and a 'STL'
  file will be created for each atlas, if applicable. The 'GIfTI' file
  will be in right-anterior-superior 'RAS' coordinate system to comply
  with the 'NIfTI' standard, and 'STL' will be in
  left-posterior-superior 'LPS' system that can be imported to
  'ITK'-based software such as 'BrainLab' or 'ANTs'.

- as_job:

  whether to run as a background job for cleaner environment; default is
  false.

- extra_transform:

  additional 'affine' transform (a four-by-four matrix) that maps the
  subject native image (not points) to the target image. This argument
  is for pipelines that take in transformed/processed images as input;
  default is `NULL`, see 'Details'.

- extra_transform_type:

  type of transform if `extra_transform` is provided.

## Value

The function returns nothing, but will create a folder named `'atlases'`
under raw subject `'rave-imaging'` folder.

## Details

The workflow generates atlases from a template. To run this function, a
separate normalization is required (see
[`cmd_run_yael_preprocess`](http://rave.wiki/ravecore/reference/cmd_run_yael_preprocess.md)).
That function normalizes native subject's brain (`'T1w'` image) to a
specified template (typically 'MNI152b' non-linear asymmetric version)
via a deformation field. Once obtaining the normalization transform,
`generate_atlases_from_template` inverse the process, creating
subject-level atlases based on that template.

When `surfaces` is `TRUE`, each atlas or mask file will be binned with a
threshold of `0.5`. The resulting binary mask will be used to generate a
mask surface using
[`volume_to_surf`](https://dipterix.org/threeBrain/reference/volume_to_surf.html)
function via implicit Laplacian smoothing.

The resulting surfaces and atlases typically sit at native space aligned
with input `'T1w'` image. For each volume, a 'GIfTI' surface will be
created under right-anterior-superior ('RAS') coordinate system for
further inferences. In addition, an 'STL' surface will be created under
left-posterior-superior ('LPS') coordinate system. This file is mainly
to be used as object file for visualizations in software such as
'BrainLab'.

Some users might have extra processing before 'YAEL' pipeline (such as
'ACPC' realignment). This function provides a set of extra arguments
(`extra_transform` and `extra_transform_type`) allowing generating
atlases to align with the raw 'DICOM' images. `extra_transform` needs to
be an `'affine'` matrix that maps the image from the input of the 'YAEL'
pipeline to the raw 'DICOM' image. Be aware that this transform is not
the point transformation matrix. When `extra_transform` is non-empty, an
extra folder `atlases_extra` will be created under the subject imaging
folder, with transformed files aligned to 'DICOM' stored.

## Examples

``` r

# Please check out https://rave.wiki to configure Python for RAVE
# or run ravemanager::configure_python()
if (FALSE) { # \dontrun{

generate_atlases_from_template(
  "YAEL/OCD07", "/path/to/OCD_atlases")

# If the image was ACPC realigned before being fed into the YAEL
# pipeline
generate_atlases_from_template(
  "YAEL/OCD07", "/path/to/OCD_atlases",
  extra_transform =
    "/path/to/sub-OCD7_from-T1wACPC_to-T1wNative_mode-image_xfm.mat"
)

} # }

```
