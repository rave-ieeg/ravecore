# Class definition of the native `YAEL` image pipeline

Rigid registration across multiple types of images, non-linear
normalization from the native brain to common templates, and mapping of
template atlases or regions of interest back to the native brain,
without 'Python'. The class has the same methods, arguments and active
fields as
[`YAELProcess`](http://rave.wiki/ravecore/reference/YAELProcess.md) and
writes the same files (names, folders, `'ANTs'`-compatible `.mat` and
warp files, `'JSON'` logs), so the results of one class can be read by
the other. The registrations, bias-field correction and tissue
segmentation come from ravetools (version 0.3.3 or later). See examples
at
[`as_yael_process`](http://rave.wiki/ravecore/reference/as_yael_process.md)
(with `engine = "native"`) and
[`yael_preprocess_native`](http://rave.wiki/ravecore/reference/cmd_run_yael_preprocess_native.md).

## Details

Differences from
[`YAELProcess`](http://rave.wiki/ravecore/reference/YAELProcess.md) and
the rpyANTs pipeline behind it:

- There is no `deep_atropos`: argument `use_antspynet` of
  `map_to_template` is ignored, and the tissue segmentation with the
  template priors (the `Atropos` branch) always runs.

- The non-linear stage uses the local cross-correlation for every image
  (ravetools has no non-linear mutual information), also when an initial
  `affine` transform is given.

- Algorithms: ravetools runs a greedy `SyN` with a single field and a
  fixed-point inverse; its linear stages use an automatic step size,
  random (`Bernoulli`) sampling and no histogram matching, and its
  `affine` registration starts with a rigid pass. The smoothing of the
  `SyN` update field takes the rpyANTs values (3 for the initial
  registration, 3.5 for the final one) as a `Gaussian` standard
  deviation in voxels, whereas `ANTs` reads them as a variance, so the
  native update field is smoothed more. These values hold for 1 mm
  templates; for templates with more voxels per brain they are
  multiplied by the cube root of the ratio of brain voxels, rounded to a
  half (2 for the 0.5 mm `'mni_icbm152_nlin_asym_09b'` and
  `'mni_icbm152_nlin_sym_09b'`, 1.5 for the 0.25 mm macaque template):
  with the 1 mm values, the greedy update folded the deformation in
  about 1.5 percent of the brain on `'mni_icbm152_nlin_asym_09b'`. The
  share of the template brain mask where the final deformation folds (is
  not one-to-one) is logged, with a warning above 1 percent.

- The quick three-class segmentation that finds the white matter for the
  intensity normalization runs on the raw image, as in rpyANTs; when a
  few extreme intensities inside the brain leave one of its classes
  empty (`Atropos` keeps going, ravetools stops), it is run again with
  the intensities clipped at their 99.9 percent quantile inside the
  brain (then at the 99 percent quantile).

- Grids: additional images are resampled onto the grid of the native
  `'T1w'` image, and left out (with a warning) when they cover less than
  95 percent of the brain mask; template tissue priors are resampled
  onto the grid of the template `'T1'` image; the outermost voxel layer
  of the stored warp fields has no displacement.

- The template brain mask is made binary (this matters for
  `'fsaverage'`, whose mask holds intensities); a tissue class whose
  prior is empty inside the brain is left out of the segmentation (with
  a warning) instead of failing.

- Outputs: `transform_image_from_template` and
  `transform_image_to_template` return `ieegio_volume` objects; only
  nearest-neighbor and linear (`trilinear`) interpolation are available,
  and `interpolator = "auto"` follows its documented rule (`YAELProcess`
  uses nearest-neighbor); the queries never create folders;
  `set_input_image` reads the new image before it removes the old one;
  the `'NIfTI'` data types are chosen by ieegio; normalization results
  are replaced together, after the new ones are complete, and stale
  `ants<K>` transforms of earlier layouts are removed; nothing is
  written into the template folders.

- Streamlines: ieegio reads and writes `VTK` files through 'Python', so
  `generate_streamlines_from_template` skips `VTK` inputs (with a
  warning) and refuses `format = "vtk"`; it also refuses `format = "tt"`
  and saves `TT` inputs as `TCK` files, since ieegio cannot write `TT`
  files.

## Super class

[`ravepipeline::RAVESerializable`](http://dipterix.org/ravepipeline/reference/RAVESerializable.md)
-\> `YAELNativeProcess`

## Active bindings

- `subject_code`:

  'RAVE' subject code

- `image_types`:

  allowed image types

- `work_path`:

  Working directory ('RAVE' imaging path)

## Methods

### Public methods

- [`YAELNativeProcess$@marshal()`](#method-YAELNativeProcess-@marshal)

- [`YAELNativeProcess$@unmarshal()`](#method-YAELNativeProcess-@unmarshal)

- [`YAELNativeProcess$new()`](#method-YAELNativeProcess-initialize)

- [`YAELNativeProcess$set_input_image()`](#method-YAELNativeProcess-set_input_image)

- [`YAELNativeProcess$get_input_image()`](#method-YAELNativeProcess-get_input_image)

- [`YAELNativeProcess$get_subject()`](#method-YAELNativeProcess-get_subject)

- [`YAELNativeProcess$register_to_T1w()`](#method-YAELNativeProcess-register_to_T1w)

- [`YAELNativeProcess$get_native_mapping()`](#method-YAELNativeProcess-get_native_mapping)

- [`YAELNativeProcess$map_to_template()`](#method-YAELNativeProcess-map_to_template)

- [`YAELNativeProcess$get_template_mapping()`](#method-YAELNativeProcess-get_template_mapping)

- [`YAELNativeProcess$transform_image_from_template()`](#method-YAELNativeProcess-transform_image_from_template)

- [`YAELNativeProcess$transform_image_to_template()`](#method-YAELNativeProcess-transform_image_to_template)

- [`YAELNativeProcess$generate_atlas_from_template()`](#method-YAELNativeProcess-generate_atlas_from_template)

- [`YAELNativeProcess$generate_streamlines_from_template()`](#method-YAELNativeProcess-generate_streamlines_from_template)

- [`YAELNativeProcess$transform_points_to_template()`](#method-YAELNativeProcess-transform_points_to_template)

- [`YAELNativeProcess$transform_points_from_template()`](#method-YAELNativeProcess-transform_points_from_template)

- [`YAELNativeProcess$write_talairach_xfm()`](#method-YAELNativeProcess-write_talairach_xfm)

- [`YAELNativeProcess$construct_ants_folder_from_template()`](#method-YAELNativeProcess-construct_ants_folder_from_template)

- [`YAELNativeProcess$get_brain()`](#method-YAELNativeProcess-get_brain)

- [`YAELNativeProcess$clone()`](#method-YAELNativeProcess-clone)

Inherited methods

- [`ravepipeline::RAVESerializable$@compare()`](http://dipterix.org/ravepipeline/reference/RAVESerializable.html#method-@compare)

------------------------------------------------------------------------

### `YAELNativeProcess$@marshal()`

Internal method

#### Usage

    YAELNativeProcess$@marshal(...)

#### Arguments

- `...`:

  internal arguments

------------------------------------------------------------------------

### `YAELNativeProcess$@unmarshal()`

Internal method

#### Usage

    YAELNativeProcess$@unmarshal(object, ...)

#### Arguments

- `object, ...`:

  internal arguments

------------------------------------------------------------------------

### `YAELNativeProcess$new()`

Constructor to instantiate the class

#### Usage

    YAELNativeProcess$new(subject, image_types, imaging_path = NULL, ...)

#### Arguments

- `subject`:

  'RAVE' subject or subject ID; for native standard, this can be
  character code without project names, but for 'BIDS' subjects, this
  must be a full subject ID with project information

- `image_types`:

  vector of image types, such as `'T1w'`, `'CT'`, `'fGATIR'`. All images
  except `'CT'` will be considered `'preop'` (before electrode
  implantation). Please use `'postop'` to indicate if an image is taken
  after the implantation (for example, `'postopT1w'`)

- `imaging_path`:

  imaging path (path to `'rave-imaging'` if not default); internally
  used to set the work path during serialization. Please do not set it
  manually unless you know what you are doing

- `...`:

  reserved for legacy code

------------------------------------------------------------------------

### `YAELNativeProcess$set_input_image()`

Set the raw input for different image types

#### Usage

    YAELNativeProcess$set_input_image(
      path,
      type = YAEL_IMAGE_TYPES,
      overwrite = FALSE,
      on_error = c("warning", "error", "ignore")
    )

#### Arguments

- `path`:

  path to the image files in `'NIfTI'` or `'MGZ'` format

- `type`:

  type of the image

- `overwrite`:

  whether to overwrite existing images if the same type has been
  imported before; default is false

- `on_error`:

  when the file exists and `overwrite` is false, how should this error
  be reported; choices are `'warning'` (default), `'error'` (throw error
  and abort), or `'ignore'`.

#### Returns

whether the image has been set (or replaced)

------------------------------------------------------------------------

### `YAELNativeProcess$get_input_image()`

Get image path

#### Usage

    YAELNativeProcess$get_input_image(type = YAEL_IMAGE_TYPES)

#### Arguments

- `type`:

  type of the image

#### Returns

Absolute path if the image, or `NULL` if missing

------------------------------------------------------------------------

### `YAELNativeProcess$get_subject()`

Get 'RAVE' subject instance

#### Usage

    YAELNativeProcess$get_subject(..., strict = FALSE)

#### Arguments

- `...`:

  ignored

- `strict`:

  passed to
  [`as_rave_subject`](http://rave.wiki/ravecore/reference/new_rave_subject.md)

#### Returns

'RAVE' subject instance

------------------------------------------------------------------------

### `YAELNativeProcess$register_to_T1w()`

Register other images to `'T1'` weighted `'MRI'` with two rigid passes
(mutual information)

#### Usage

    YAELNativeProcess$register_to_T1w(
      image_type = "CT",
      reverse = FALSE,
      verbose = TRUE
    )

#### Arguments

- `image_type`:

  type of the image to register, must be set via
  `process$set_input_image` first.

- `reverse`:

  whether to reverse the registration; default is false, meaning the
  fixed (reference) image is the `'T1'`. When setting to true, then the
  `'T1'` 'MRI' will become the moving image

- `verbose`:

  whether to print out the process; default is true

#### Returns

Nothing

------------------------------------------------------------------------

### `YAELNativeProcess$get_native_mapping()`

Get the mapping configurations used by `register_to_T1w`

#### Usage

    YAELNativeProcess$get_native_mapping(
      image_type = YAEL_IMAGE_TYPES,
      relative = FALSE
    )

#### Arguments

- `image_type`:

  type of the image registered to 'T1' weighted 'MRI'

- `relative`:

  whether to use relative path (to the `work_path` field)

#### Returns

A list of moving and fixing images, with rigid transformations from
different formats; `mappings` is empty before the image is registered.
No folder is created.

------------------------------------------------------------------------

### `YAELNativeProcess$map_to_template()`

Normalize native brain to `'MNI152'` template

#### Usage

    YAELNativeProcess$map_to_template(
      template_name = rpyants_builtin_templates(),
      use_images = c("T1w", "T2w", "T1wContrast", "fGATIR", "preopCT"),
      native_type = "T1w",
      use_antspynet = TRUE,
      verbose = TRUE,
      ...
    )

#### Arguments

- `template_name`:

  which template to use, choices are `'mni_icbm152_nlin_asym_09a'`,
  `'mni_icbm152_nlin_asym_09b'`, `'mni_icbm152_nlin_asym_09c'`, and
  `'fsaverage'`.

- `use_images`:

  a vector of image types to use for normalization; default types are
  `'T1w'`, `'T2w'`, `'T1wContrast'`, `'fGATIR'`, and `'preopCT'`. To use
  all available images for normalization, use wildcard `"all"`. Images
  that are not registered to `'T1w'` yet are registered first (a failure
  is reported as a warning and the image is left out)

- `native_type`:

  which type of image should be used to map to template; default is
  `'T1w'`

- `use_antspynet`:

  ignored: the native engine has no `deep_atropos`, and the tissue
  segmentation always uses the template priors

- `verbose`:

  whether to print out the process; default is true

- `...`:

  additional tuning parameters: `affine_transform` (optional initial
  `affine` transform, an `'ANTs'` `.mat` file from the template to the
  native image as point map; the initial registration then only runs its
  non-linear stage), `with_skull` (whether to register the images with
  skull; default is false), and `cost_function` (only `'CC'`, the local
  cross-correlation, is available; other values give a warning); other
  arguments are ignored

#### Returns

See method `get_template_mapping`

------------------------------------------------------------------------

### `YAELNativeProcess$get_template_mapping()`

Get configurations used for normalization

#### Usage

    YAELNativeProcess$get_template_mapping(
      template_name = rpyants_builtin_templates(),
      native_type = "T1w",
      relative = FALSE
    )

#### Arguments

- `template_name`:

  which template is used

- `native_type`:

  which native image is mapped to template

- `relative`:

  whether the paths should be relative or absolute; default is false
  (absolute paths)

#### Returns

A list of input, output images, with forward and inverse transform files
(usually two `'Affine'` with one displacement field), or `NULL` if the
image is not normalized to the template yet. The transforms of the
subject are used; transforms whose subject entity is spelled in another
letter case are used if there is only one such spelling, and the
transforms of another subject only if they are the only ones and the
native image was imported under that subject (with a warning)

------------------------------------------------------------------------

### `YAELNativeProcess$transform_image_from_template()`

Apply transform from images (usually an atlas or a region of interest)
on template to native space

#### Usage

    YAELNativeProcess$transform_image_from_template(
      template_roi_path,
      template_name = rpyants_builtin_templates(),
      native_type = "T1w",
      interpolator = c("auto", "nearestNeighbor", "linear", "gaussian", "bSpline",
        "cosineWindowedSinc", "welchWindowedSinc", "hammingWindowedSinc",
        "lanczosWindowedSinc", "genericLabel"),
      verbose = TRUE
    )

#### Arguments

- `template_roi_path`:

  path to the template image file which will be transformed into
  individuals' image

- `template_name`:

  templates to use

- `native_type`:

  which type of native image to use for calculating the coordinates
  (default `'T1w'`)

- `interpolator`:

  how to interpolate the `'voxels'`; default is `"auto"`: linear for
  probability maps (all values between 0 and 1, some of them fractional)
  and `'nearestNeighbor'` otherwise. `'nearestNeighbor'` and
  `'genericLabel'` use the nearest neighbor, all others the linear
  (`trilinear`) interpolation

- `verbose`:

  whether the print out the progress

#### Returns

transformed image on the grid of the native image, as an `ieegio_volume`
object

------------------------------------------------------------------------

### `YAELNativeProcess$transform_image_to_template()`

Apply transform to images (usually an atlas or a region of interest)
from native space to template

#### Usage

    YAELNativeProcess$transform_image_to_template(
      native_roi_path,
      template_name = rpyants_builtin_templates(),
      native_type = "T1w",
      interpolator = c("auto", "nearestNeighbor", "linear", "gaussian", "bSpline",
        "cosineWindowedSinc", "welchWindowedSinc", "hammingWindowedSinc",
        "lanczosWindowedSinc", "genericLabel"),
      verbose = TRUE
    )

#### Arguments

- `native_roi_path`:

  path to the native image file that will be transformed into template

- `template_name`:

  templates to use

- `native_type`:

  which type of native image to use for calculating the coordinates
  (default `'T1w'`)

- `interpolator`:

  how to interpolate the `'voxels'`; default is `"auto"`: linear for
  probability maps (all values between 0 and 1, some of them fractional)
  and `'nearestNeighbor'` otherwise. `'nearestNeighbor'` and
  `'genericLabel'` use the nearest neighbor, all others the linear
  (`trilinear`) interpolation

- `verbose`:

  whether the print out the progress

#### Returns

transformed image on the grid of the template `'T1'` image, as an
`ieegio_volume` object

------------------------------------------------------------------------

### `YAELNativeProcess$generate_atlas_from_template()`

Generate atlas maps from template and morph to native brain

#### Usage

    YAELNativeProcess$generate_atlas_from_template(
      template_name = rpyants_builtin_templates(),
      atlas_folder = NULL,
      surfaces = NA,
      verbose = TRUE,
      lambda = 0.2,
      degree = 2,
      threshold_lb = 0.5,
      threshold_ub = NA,
      smooth_method = c("implicit", "explicit"),
      max_vertices = 5e+05
    )

#### Arguments

- `template_name`:

  which template to use

- `atlas_folder`:

  path to the atlas folder (that contains the atlas files); every
  `'NIfTI'` file in the folder and its sub-folders is mapped to
  `'atlases/<sub-folder>/<file name>'` under the working directory (file
  names starting with a dot are skipped). Probability maps (values
  between 0 and 1, some fractional) are interpolated linearly, other
  images with the nearest neighbor

- `surfaces`:

  whether to generate surfaces (triangle mesh); default is `NA`
  (generate if not existed). Other choices are `TRUE` for always
  generating and overwriting surface files, or `FALSE` to disable this
  function. The generated surfaces will stay in native `'T1'` space.

- `verbose`:

  whether the print out the progress

- `lambda, degree, threshold_lb, threshold_ub`:

  passed to
  [`volume_to_surf`](https://dipterix.org/threeBrain/reference/volume_to_surf.html)

- `smooth_method`:

  how to smooth the surfaces: `"implicit"` (default,
  [`vcg_smooth_implicit`](https://dipterix.org/ravetools/reference/vcg_smooth.html)
  with `lambda` and `degree`) or `"explicit"`
  ([`mris_smooth`](https://dipterix.org/ravetools/reference/mris_smooth.html));
  passed to
  [`volume_to_surf`](https://dipterix.org/threeBrain/reference/volume_to_surf.html)

- `max_vertices`:

  surfaces with more vertices are reduced to about this many after
  smoothing; passed to
  [`volume_to_surf`](https://dipterix.org/threeBrain/reference/volume_to_surf.html);
  default is `500000`

#### Returns

Paths to the atlas (volume) files under the `'atlases'` folder of the
working directory

------------------------------------------------------------------------

### `YAELNativeProcess$generate_streamlines_from_template()`

Transform streamline tract files from template space to native brain
space

#### Usage

    YAELNativeProcess$generate_streamlines_from_template(
      template_name = rpyants_builtin_templates(),
      streamlines_folder = NULL,
      verbose = TRUE,
      format = c("auto", "tck", "trk", "tt", "vtk"),
      native_type = "T1w"
    )

#### Arguments

- `template_name`:

  which template to use; the template must have been used in
  `map_to_template` prior to calling this method

- `streamlines_folder`:

  path to folder containing streamline tract files (`'.tck'`, `'.trk'`,
  `'.tt'`, or `'.vtk'`); all matching files are processed recursively,
  except `VTK` files (`'.vtk'`, `'.vtp'`), which ieegio can only read
  through 'Python': they are skipped with a warning

- `verbose`:

  whether to print progress; default is `TRUE`

- `format`:

  output format for the saved streamlines; `"auto"` (default) preserves
  the original file extension, except that `TT` files are saved as `TCK`
  (ieegio has no `TT` writer); `"tck"` and `"trk"` convert every file.
  `"tt"` (no writer) and `"vtk"` (written through 'Python') are errors

- `native_type`:

  which native image type defines the target space; default is `"T1w"`

#### Returns

Invisibly returns a character vector of the relative paths to the input
streamline files that were processed (`VTK` files are left out);
transformed files are written under `fs/streamline/` from the working
directory. The points of all files are transformed together; points with
missing coordinates are dropped. 'Python' is never started: the files
are read and written with the 'Python' support of ieegio disabled.

------------------------------------------------------------------------

### `YAELNativeProcess$transform_points_to_template()`

Transform points from native images to template

#### Usage

    YAELNativeProcess$transform_points_to_template(
      native_ras,
      template_name = rpyants_builtin_templates(),
      native_type = "T1w",
      verbose = TRUE
    )

#### Arguments

- `native_ras`:

  matrix or data frame with 3 columns indicating points sitting on
  native images in right-anterior-superior (`'RAS'`) coordinate system.

- `template_name`:

  template to use for mapping

- `native_type`:

  native image type where the points sit on

- `verbose`:

  whether the print out the progress

#### Returns

A matrix of 3 columns, each row is a transformed points ( invalid rows
will be filled with `NA`)

------------------------------------------------------------------------

### `YAELNativeProcess$transform_points_from_template()`

Transform points from template images to native

#### Usage

    YAELNativeProcess$transform_points_from_template(
      template_ras,
      template_name = rpyants_builtin_templates(),
      native_type = "T1w",
      verbose = TRUE
    )

#### Arguments

- `template_ras`:

  matrix or data frame with 3 columns indicating points sitting on
  template images in right-anterior-superior (`'RAS'`) coordinate
  system.

- `template_name`:

  template to use for mapping

- `native_type`:

  native image type where the points sit on

- `verbose`:

  whether the print out the progress

#### Returns

A matrix of 3 columns, each row is a transformed points ( invalid rows
will be filled with `NA`)

------------------------------------------------------------------------

### `YAELNativeProcess$write_talairach_xfm()`

Write the linear transform `'ants/mri/transforms/talairach.xfm'` from
native `'T1w'` scanner `'RAS'` coordinates to `'MNI305'` space, using
the `affine` part of the normalization created by `map_to_template`. For
templates that are not in `'MNI152'` space, the transform maps to the
template space instead (`'fsaverage'` is already in `'MNI305'` space).
This method is called by `construct_ants_folder_from_template`.

#### Usage

    YAELNativeProcess$write_talairach_xfm(
      template_name = rpyants_builtin_templates()
    )

#### Arguments

- `template_name`:

  template that has been used in `map_to_template`

#### Returns

The path to the transform file, invisibly

------------------------------------------------------------------------

### `YAELNativeProcess$construct_ants_folder_from_template()`

Create a reconstruction folder (as an alternative option) that is
generated from template brain to facilitate the three-dimensional
viewer. Please make sure method `map_to_template` is called before using
this method (or the program will fail). The folder holds the conformed
`'mri/T1.mgz'`, the template brain mask mapped to the native image
(`'mri/brainmask.nii.gz'`, binary), the bias-corrected image
(`'mri/nu.nii.gz'`), its skull-stripped version scaled to 0 to 255
(`'mri/brain.finalsurfs.nii.gz'`), and the linear transform
`'mri/transforms/talairach.xfm'` written by `write_talairach_xfm`; if
that fails, the file is removed and a warning is logged.

#### Usage

    YAELNativeProcess$construct_ants_folder_from_template(
      template_name = rpyants_builtin_templates(),
      add_surfaces = TRUE
    )

#### Arguments

- `template_name`:

  template to use for mapping; `NULL` only writes `'mri/T1.mgz'`

- `add_surfaces`:

  whether to create surfaces that is morphed from template to local;
  default is `TRUE`. The surfaces are only created for templates with
  `'fs/surf'` `pial` surfaces. Please enable this option only if the
  cortical surfaces are not critical (for example, you are studying the
  deep brain structures). Always use `'FreeSurfer'` if cortical
  information is used.

------------------------------------------------------------------------

### `YAELNativeProcess$get_brain()`

Get three-dimensional brain model

#### Usage

    YAELNativeProcess$get_brain(
      electrodes = TRUE,
      coord_sys = c("scannerRAS", "tkrRAS", "MNI152", "MNI305"),
      ...
    )

#### Arguments

- `electrodes`:

  whether to add electrodes to the viewers; can be logical, data frame,
  or a character (path to electrode table). When the value is `TRUE`,
  the electrode file under `project_name` will be loaded; when
  `electrodes` is a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html), or path to a
  `'csv'` file, then please specify `coord_sys` on what is the
  coordinate system used for columns `"x"`, `"y"`, and `"z"`.

- `coord_sys`:

  coordinate system if `electrodes` is a data frame with columns `"x"`,
  `"y"`, and `"z"`, available choices are `'scannerRAS'` (defined by
  'T1' weighted native 'MRI' image), `'tkrRAS'` (`'FreeSurfer'` defined
  native 'TK-registered'), `'MNI152'` (template `MNI` coordinate system
  averaged over 152 subjects; this is the common `MNI` coordinate space
  we often refer to), and `'MNI305'` (template `MNI` coordinate system
  averaged over 305 subjects; this coordinate system used by templates
  such as `'fsaverage'`)

- `...`:

  passed to
  [`threeBrain`](https://dipterix.org/threeBrain/reference/threeBrain.html)

------------------------------------------------------------------------

### `YAELNativeProcess$clone()`

The objects of this class are cloneable with this method.

#### Usage

    YAELNativeProcess$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
