# ravecore 1.0.0

### New Features

* Added `yael_macaque()` and macaque support in the YAEL preprocessing pipeline (`#32dcb87`, `#6c864e2`).
* Added `generate_atlas_YBA()` to morph `YBA690` / `YBA696` atlases from the `MNI152` symmetric template to the subject, with an atlas-overlay report (`#f45c574`).
* Added streamline support: `rave_brain()` gains a `streamlines` argument, and the YAEL process can generate streamlines from a template (`#fbd913a`, `#f79c06e`, `#6adff48`).
* `yael_preprocess()` accepts `additional_images` so that more image modalities can be used for co-registration (`#e6d97ba`).
* Added `realign_trials()` to realign arrays by event; `event` can be an event string or a column name such as `"Time"` or `"Event_*"` (`#f4fc653`, `#db09e00`).
* Added `validate_condition_groupings()` to validate and clean condition-group lists; the epoch table is ordered by trial number (`#923c913`, `#509570b`).
* Added `"db_zscore"` baseline method to `power_baseline()` (`#ea95dc4`).
* Repositories gain `get_electrode_coordinate()` to subset the electrode table by channel numbers and types (`#9c7f5df`).
* `RAVEEpoch` supports an `ExcludedHint` column and an `exclude_trials()` method; saving produces a trimmed `_OutlierRemoved` epoch (`#33c828c`).
* Added `reg3d_rigid()` and `cmd_run_reg3d_rigid()` to rigidly co-register `CT` to `MRI` with the built-in `ravetools::register_volume3d()` (`YAEL-reg3d`; no external program needed), with `Rigid`, `DenseRigid` and `FastRigid` presets and mutual-information or cross-correlation cost. Requires `ravetools` 0.3.1 or later.
* Added `YAELNativeProcess`, `as_yael_process(engine = "native")`, `yael_preprocess_native()` and `cmd_run_yael_preprocess_native()`: the YAEL co-registration, template normalization and atlas mapping without `Python`, built on `ravetools` (`N4`, tissue segmentation with template priors, affine and `SyN` registration). They take the same arguments and write the same files as `YAELProcess`, and either engine reads the results of the other (checked against `ANTs` on toy images); `YAELProcess` stays the default. Differences from `YAELProcess` (details in `?YAELNativeProcess`):
    * No `deep_atropos` (`use_antspynet` is ignored): the tissue segmentation with the template priors always runs. A tissue class whose prior is empty inside the brain is left out (with a warning) instead of failing, and the template brain mask is made binary.
    * Every image of the non-linear stage uses local cross-correlation (no non-linear mutual information), also when `normalize_affine` is given.
    * `ravetools` runs a greedy single-field `SyN` with a fixed-point inverse. Its linear stages use an automatic step size, random (`Bernoulli`) sampling and no histogram matching, and its affine registration starts with a rigid pass.
    * The update-field smoothing takes the `rpyANTs` values (3 and 3.5) as a standard deviation in voxels, where `ANTs` reads a variance; for templates with more voxels per brain it is multiplied by up to 2 (the 0.5 mm `09b` templates), since the 1 mm values folded the deformation in about 1.5% of the brain on `mni_icbm152_nlin_asym_09b`. The share of the brain where the final deformation folds is logged, with a warning above 1%. The initial registration, which places the template brain mask and the tissue priors, always goes to the 1 mm `mni_icbm152_nlin_asym_09a` (see `yael_brain_extract()` below), whatever the template of the normalization; the final registration stays on the grid of that template.
    * When the k-means segmentation that finds the white matter fails on a few extreme intensities, it is run again with the intensities clipped at their 99.9% (then 99%) quantile.
    * Brain extraction: the template brain mask mapped to the native image is adjusted to the subject, following `antsBrainExtraction.sh`, instead of being used as it is. A three-class segmentation of the mask grown by 2 mm finds the largest connected white and gray matter, an erosion, the largest component, a dilation, hole filling and a closing cut what hangs on the brain by a thread, and the mapped mask is added back. Then, as in the watershed of `FreeSurfer`, the gray and white matter that dark voxels (`CSF` or bone) cut off from the brain, such as skull marrow, `dura` or scalp, leaves the mask, also where the mapped mask held it (`antsBrainExtraction.sh` keeps all of the mapped mask). The thin strands this can leave (such as marrow as dark as gray matter) go too: the part of the mask that survives an erosion by 2 mm, grown back by 3 mm and kept inside the mask, holds the brain but not what is thinner than about 4 mm and reaches more than about 1 mm out. The distances are millimeters along each axis of the image, so they hold on thick slices. The brain mask comes from `yael_brain_extract()` with the template `mni_icbm152_nlin_asym_09a`, whose tissue priors the segmentation also takes, and replaces the mapped mask in the tissue segmentation, in the final registration and its `brainmask2.nii.gz` and `brain2.nii.gz` (which held the template mask pulled back), and in `construct_ants_folder_from_template()`, which reuses the files of the extraction from the stage folder of the normalization when all of them are there. The mapped mask is kept, with a warning, when the segmentation fails or the volume changes by more than 20%.
    * White-matter voxels whose ray cast away from the center of the white matter misses the smoothed outer surface of the gray matter and the cerebellum (`ravetools::mris_outer_surface()`), such as skull marrow that touches the brain, which the brain extraction keeps, are dilated by two voxels and left out of the white-matter channel; those labeled white matter also leave the brain mask and the brain image of the final registration. The cone below the center of the white matter, which holds the brain stem, is kept. The removed volume is logged. The tissue classes keep the order of their priors, with a warning (and nothing left out) when their median intensities are not in that order.
    * Additional images are resampled onto the native `T1w` grid and left out (with a warning) when they cover less than 95% of the brain; the template tissue priors are resampled onto the template `T1` grid; the outermost voxel layer of the stored warp fields has no displacement.
    * `transform_image_from_template()` and `transform_image_to_template()` return `ieegio_volume` objects, with nearest-neighbor or linear interpolation only, and `interpolator = "auto"` follows the documented rule.
    * Normalization results are replaced together once the new ones are complete, and stale `ants<K>` transforms of earlier layouts are removed; queries never create folders; `set_input_image()` reads the new image before removing the old one; nothing is written into the template folders.
    * Streamlines in `VTK` format are neither read nor written (they need `Python`), and `TT` files are saved as `TCK`.
    * Memory: with a 1 mm `T1w` image, the normalization to `mni_icbm152_nlin_asym_09a` peaked at about 8 GB of resident memory, and to `mni_icbm152_nlin_asym_09b` at about 15 GB (22 GB with compressed memory). The script of `cmd_run_yael_preprocess_native()` therefore switches from `09b` to `09a` below 32 GB of memory (12 GB for `cmd_run_yael_preprocess()`), and `map_to_template()` warns for such templates below 32 GB.
* Added `yael_brain_extract()`, the brain mask of a `T1`-weighted image without `Python`: the image is corrected with `N4` and registered (affine, then `SyN`, without masks; only `SyN` from a given `affine_transform`) to `mni_icbm152_nlin_asym_09a`, whose brain mask, brought back to the image, is adjusted to the subject by the brain extraction of the native engine (see above) on the image corrected by `N4` without intensity truncation (with the truncation of `abp_n4`, bright marrow looks like white matter). A given initial mask, such as a `FreeSurfer` `brainmask.mgz`, replaces the template mask and its registration. It returns an `ieegio_volume`; `work_path` keeps the intermediate files (`nu`, the `SyNabp` registration, the initial and the final mask). The native normalization uses it (`map_to_template()` passes its `affine_transform` for the `MNI152` templates).
* `get_available_morph_to_template()`, `generate_atlases_from_template()`, `generate_atlas_YBA()`, `transform_point_to_template()` and `transform_thinfilm_to_mni152()` read the normalization with the native engine, so they no longer need `Python`, whichever engine processed the subject. `get_available_morph_to_template()` runs in the current session instead of a separate job. Atlases written by the native engine are `float32`, as with `ANTs`. `yael_macaque()` stays on `rpyANTs`.
* Added `YAELProcess$write_talairach_xfm()` to write `ants/mri/transforms/talairach.xfm` from an existing template normalization.
* `YAELProcess$generate_atlas_from_template()` gains `smooth_method = "explicit"` to smooth atlas surfaces with `ravetools::mris_smooth()`, and `max_vertices` (500,000 by default) to reduce very large atlas surfaces after smoothing; both are passed to `ieegio::volume_to_surface()` when the installed `ieegio` accepts them.
* `RAVESubject` gains `format()`, which `print()` now shows: the subject's blocks, its electrodes by signal type with sample rates and which ones are `Notch`-filtered or have `Wavelet` power, its epoch and reference names (with the subject's defaults marked), and its imaging state (native `MRI`, `MNI` normalization templates, `CT`-`MRI` co-registration, `FreeSurfer`). Only metadata are read.

### Bug Fixes

* Without `Python`, `transform_point_to_template(volumetric_transform = "auto")` and `transform_thinfilm_to_mni152()` silently returned affine coordinates even when a non-linear deformation existed, because a failed lookup looked the same as a missing deformation. The deformation is now read without `Python`, and a failed lookup gives a warning before the affine is used. The `group_3d_viewer` module may have saved such coordinates in `electrodes_normalization_mni152.csv` as non-linear results; to regenerate this file for subjects viewed without `Python`, choose the non-linear volumetric mapping and enable "Clear existing cache and force re-calculating normalization" in the module.
* `transform_point_to_template()` treated the results of a normalization to `fsaverage` as `MNI152`. For subjects with no `MNI152` normalization, the `MNI152_*` columns held `MNI305` coordinates and the `MNI305_*` columns were converted a second time, which gave errors of about 2 mm. `transform_thinfilm_to_mni152()` used these columns.
* `generate_atlases_from_template()` ignored `extra_transform` when it was a matrix, or when `surfaces = FALSE`, without any message, and failed on a file path with `extra_transform_type = "native"`. All of these now create the `atlases_extra` folder. With `extra_transform_type = "ants"`, the default, a matrix is read as `LPS` to `LPS`, like an `ANTs` file; with `"native"`, a matrix or text file is read as `RAS` to `RAS`. Reading an `ANTs` file no longer needs `Python`.
* `cmd_run_yael_preprocess()` and `cmd_run_yael_preprocess_native()` gain `overwrite` (default `FALSE`). With `run_recon_all = TRUE`, their scripts always deleted the existing `FreeSurfer` folder; they now do so only with `overwrite = TRUE`, which also removes the `tmp` folder under the imaging folder and calculates the normalization again even if it exists. Code that relies on the old behavior needs `overwrite = TRUE`.
* `YAELProcess$generate_atlas_from_template()` reports each atlas surface it fails to generate as a warning naming the volume; failures were hidden by a silent `try()`. Generating the surface of a whole-brain `parcellation` such as `YBA690` (millions of vertices) crashed `R` inside `ravetools::vcg_smooth_implicit()`; with `ravetools` 0.3.3 the full-resolution surface is smoothed in about 20 seconds and then reduced to `max_vertices`.
* `YAELProcess$construct_ants_folder_from_template()` wrote `ants/mri/transforms/talairach.xfm` from the template-to-native `ANTs` affine without inverting it or converting `LPS` to `RAS`, and applied the `MNI152`-to-`MNI305` correction to every template. For subjects without `rave-imaging/fs`, `MNI305`/`MNI152` electrode coordinates derived from this file could be off by several centimeters. The transform is now inverted, converted to `RAS`, and corrected only for `MNI152` templates; failures are logged and the stale file is removed. To repair a subject, run `as_yael_process("<project>/<subject>")$write_talairach_xfm("<template>")`, then re-save the electrode localization, or re-import an electrode table without the `MNI305_*`/`MNI152_*` columns (or with them set to zero); existing `MNI` columns are kept as they are.
* `cmd_run_dcm2niix()` now remembers the imported image source under the `yael_preprocess` module (it used the old module ID `surface_reconstruction`), as a path relative to the subject's raw folder (or `BIDS` raw folder), so the module loader can select it (`#57c7684`).
* Subject pipeline listing is backward compatible with old pipeline paths, and listing all pipelines works again (`#f428fc0`, `#e9ebb8e`, `#7922564`).
* `filearray` partitions are created before parallel workers start during power baseline, preventing a rare write race (`#9dadfec`).
* `rave_slices` is no longer copied when normalization fails (`#2dd9c41`).
* Fixed a `read_mat2()` call signature in the per-channel `HDF5`/MATLAB importer (`#6872bfc`).
* Fixed a syntax issue in the YAEL process (`#61016f5`).

### Other Changes

* Added tests that check function-call signatures through `asNamespace()` (`#633cde9`).
* Added tests that run each function that moved to the native engine on both engines and compare the outputs; they skip on `CRAN` and without `Python`.
* Importing `NEV`/`NSx` data hashes each `NEV` file once per session; validation and import used to hash the whole file again for every channel type (up to six times per file). The hash still names the cache folder, so existing caches are reused.

---

# ravecore 0.1.1

### New Features

* Added `generate_atlases_from_template()` to generate brain atlases from a template; supports exporting STL meshes in LPS coordinates (`#d2b0f60`, `#c42dbb9`).
* Added `use_antspynet` option in the YAEL preprocessing pipeline (`#7795023`).
* Added snapshot report for projects and subjects (`#f91eb3b`).
* Added `install_openneuro()` helper to install OpenNeuro subjects (`#1cdffc2`).
* Added quick mode for faster subject preparation (`#70388b4`).
* Added spike diagnostic plots and graphic options for the spike visualizer (`#b53887d`, `#f1cc332`).
* Added debug message output for spike sorter workflows (`#fc25b2f`).
* Added utility functions for `SpikeInterface` integration (`#71db36d`).
* Spike analyzer now uses band-passed signal for improved accuracy (`#bb021d4`).
* Parallel workers are used to save spike data (`#b4c1b46`).
* Python environment is auto-installed when missing (`#5985f46`).
* Spike sorters are now cached to avoid redundant computation (`#2068ce0`).
* Baseline firing rate computation now supports sub-1 rates (`#292f2fe`, `#48f3760`).
* Spike histogram bins now cover the entire epoch range (`#d49f9db`, `#a9e94ea`).
* Spike train is filtered to ensure times are within the requested bin range (`#ca2c1e4`).
* Repository object format has been updated and improved (`#ea2513a`).

### Bug Fixes

* Fixed HDF5 links not being closed promptly, which could cause resource leaks (`#a5e95fe`).
* Fixed `LFP_reference` serialization error and added additional validation checks (`#93ad452`, `#f39f51a`).
* Fixed error message displayed when `rpymat` is not configured (`#ff5a6b2`).
* Fixed `ensure_py_package()` to install `spikeinterface` via pip correctly (`#1bcd358`).
* Fixed ordering issue when the electrode table is empty (`#37904c3`).
* Fixed syntax errors (`#fb14366`).
* Handle missing electrodes gracefully (`#3f6e381`).
* `threeBrain` template is now ensured without throwing an error (`#f91eb3b`).
* Invalid projects now display without broken links (`#048c91e`).
* Spike iteration now processes per channel instead of manually computing iteration chunks (`#1580465`).
* Only spike data (not raw signals) is stored during sorter runs (`#2aa1cff`).
* Reference path is now correctly prioritized (`#39c75bb`).

### Other Changes

* Lint and code style improvements (`#afae460`).
* Minor documentation fixes (`#990fc40`).
* CRAN submission comment fixes (`#004b775`, `#8804222`).

---

# ravecore 0.1.0

* Initial CRAN submission.
