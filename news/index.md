# Changelog

## ravecore 0.1.2

#### New Features

- Added
  [`yael_macaque()`](http://rave.wiki/ravecore/reference/yael_macaque.md)
  and macaque support in the YAEL preprocessing pipeline (`#32dcb87`,
  `#6c864e2`).
- Added
  [`generate_atlas_YBA()`](http://rave.wiki/ravecore/reference/generate_atlas_YBA.md)
  to morph `YBA690` / `YBA696` atlases from the `MNI152` symmetric
  template to the subject, with an atlas-overlay report (`#f45c574`).
- Added streamline support:
  [`rave_brain()`](http://rave.wiki/ravecore/reference/rave_brain.md)
  gains a `streamlines` argument, and the YAEL process can generate
  streamlines from a template (`#fbd913a`, `#f79c06e`, `#6adff48`).
- [`yael_preprocess()`](http://rave.wiki/ravecore/reference/cmd_run_yael_preprocess.md)
  accepts `additional_images` so that more image modalities can be used
  for co-registration (`#e6d97ba`).
- Added
  [`realign_trials()`](http://rave.wiki/ravecore/reference/realign_trials.md)
  to realign arrays by event; `event` can be an event string or a column
  name such as `"Time"` or `"Event_*"` (`#f4fc653`, `#db09e00`).
- Added
  [`validate_condition_groupings()`](http://rave.wiki/ravecore/reference/validate_condition_groupings.md)
  to validate and clean condition-group lists; the epoch table is
  ordered by trial number (`#923c913`, `#509570b`).
- Added `"db_zscore"` baseline method to
  [`power_baseline()`](http://rave.wiki/ravecore/reference/power_baseline.md)
  (`#ea95dc4`).
- Repositories gain `get_electrode_coordinate()` to subset the electrode
  table by channel numbers and types (`#9c7f5df`).
- `RAVEEpoch` supports an `ExcludedHint` column and an
  `exclude_trials()` method; saving produces a trimmed `_OutlierRemoved`
  epoch (`#33c828c`).
- Added
  [`reg3d_rigid()`](http://rave.wiki/ravecore/reference/reg3d_rigid.md)
  and
  [`cmd_run_reg3d_rigid()`](http://rave.wiki/ravecore/reference/reg3d_rigid.md)
  to rigidly co-register `CT` to `MRI` with the built-in
  [`ravetools::register_volume3d()`](https://dipterix.org/ravetools/reference/register_volume3d.html)
  (`YAEL-reg3d`; no external program needed), with `Rigid`, `DenseRigid`
  and `FastRigid` presets and mutual-information or cross-correlation
  cost. Requires `ravetools` 0.3.1 or later.
- Added `YAELNativeProcess`, `as_yael_process(engine = "native")`,
  [`yael_preprocess_native()`](http://rave.wiki/ravecore/reference/cmd_run_yael_preprocess_native.md)
  and
  [`cmd_run_yael_preprocess_native()`](http://rave.wiki/ravecore/reference/cmd_run_yael_preprocess_native.md):
  the YAEL co-registration, template normalization and atlas mapping
  without `Python`, built on `ravetools` (`N4`, tissue segmentation with
  template priors, affine and `SyN` registration). They take the same
  arguments and write the same files as `YAELProcess`, and either engine
  reads the results of the other (checked against `ANTs` on toy images);
  `YAELProcess` stays the default. Requires `ravetools` 0.3.3 or later;
  with an older version the native functions stop with an explanation.
  Differences from `YAELProcess` (details in
  [`?YAELNativeProcess`](http://rave.wiki/ravecore/reference/YAELNativeProcess.md)):
  - No `deep_atropos` (`use_antspynet` is ignored): the tissue
    segmentation with the template priors always runs. A tissue class
    whose prior is empty inside the brain is left out (with a warning)
    instead of failing, and the template brain mask is made binary.
  - Every image of the non-linear stage uses local cross-correlation (no
    non-linear mutual information), also when `normalize_affine` is
    given.
  - `ravetools` runs a greedy single-field `SyN` with a fixed-point
    inverse. Its linear stages use an automatic step size, random
    (`Bernoulli`) sampling and no histogram matching, and its affine
    registration starts with a rigid pass.
  - The update-field smoothing takes the `rpyANTs` values (3 and 3.5) as
    a standard deviation in voxels, where `ANTs` reads a variance; for
    templates with more voxels per brain it is multiplied by up to 2
    (the 0.5 mm `09b` templates), since the 1 mm values folded the
    deformation in about 1.5% of the brain on
    `mni_icbm152_nlin_asym_09b`. The share of the brain where the final
    deformation folds is logged, with a warning above 1%.
  - When the k-means segmentation that finds the white matter fails on a
    few extreme intensities, it is run again with the intensities
    clipped at their 99.9% (then 99%) quantile.
  - White-matter voxels near the edge of the brain mask that lie outside
    the gray-matter surface (at least a quarter of the rays cast from
    them miss it), such as a thick, bright skull marrow that the initial
    registration left inside the template brain mask, are dilated by two
    voxels and left out of the white-matter channel, the brain mask and
    the brain image of the final registration; the cone below the center
    of the white matter, which holds the brain stem, is kept. The
    removed volume is logged. The tissue classes keep the order of their
    priors, with a warning when their median intensities are not in that
    order.
  - Additional images are resampled onto the native `T1w` grid and left
    out (with a warning) when they cover less than 95% of the brain; the
    template tissue priors are resampled onto the template `T1` grid;
    the outermost voxel layer of the stored warp fields has no
    displacement.
  - `transform_image_from_template()` and
    `transform_image_to_template()` return `ieegio_volume` objects, with
    nearest-neighbor or linear interpolation only, and
    `interpolator = "auto"` follows the documented rule.
  - Normalization results are replaced together once the new ones are
    complete, and stale `ants<K>` transforms of earlier layouts are
    removed; queries never create folders; `set_input_image()` reads the
    new image before removing the old one; nothing is written into the
    template folders.
  - Streamlines in `VTK` format are neither read nor written (they need
    `Python`), and `TT` files are saved as `TCK`.
  - Memory: with a 1 mm `T1w` image, the normalization to
    `mni_icbm152_nlin_asym_09a` peaked at about 8 GB of resident memory,
    and to `mni_icbm152_nlin_asym_09b` at about 15 GB (22 GB with
    compressed memory). The script of
    [`cmd_run_yael_preprocess_native()`](http://rave.wiki/ravecore/reference/cmd_run_yael_preprocess_native.md)
    therefore switches from `09b` to `09a` below 32 GB of memory (12 GB
    for
    [`cmd_run_yael_preprocess()`](http://rave.wiki/ravecore/reference/cmd_run_yael_preprocess.md)),
    and `map_to_template()` warns for such templates below 32 GB.
- Added `YAELProcess$write_talairach_xfm()` to write
  `ants/mri/transforms/talairach.xfm` from an existing template
  normalization.
- `YAELProcess$generate_atlas_from_template()` gains
  `smooth_method = "explicit"` to smooth atlas surfaces with
  [`ravetools::mris_smooth()`](https://dipterix.org/ravetools/reference/mris_smooth.html),
  and `max_vertices` (500,000 by default) to reduce very large atlas
  surfaces after smoothing; both are passed to
  [`ieegio::volume_to_surface()`](http://dipterix.org/ieegio/reference/volume_to_surface.md)
  when the installed `ieegio` accepts them.
- `RAVESubject` gains [`format()`](https://rdrr.io/r/base/format.html),
  which [`print()`](https://rdrr.io/r/base/print.html) now shows: the
  subject’s blocks, its electrodes by signal type with sample rates and
  which ones are `Notch`-filtered or have `Wavelet` power, its epoch and
  reference names (with the subject’s defaults marked), and its imaging
  state (native `MRI`, `MNI` normalization templates, `CT`-`MRI`
  co-registration, `FreeSurfer`). Only metadata are read.

#### Bug Fixes

- `YAELProcess$generate_atlas_from_template()` reports each atlas
  surface it fails to generate as a warning naming the volume; failures
  were hidden by a silent [`try()`](https://rdrr.io/r/base/try.html).
  Generating the surface of a whole-brain `parcellation` such as
  `YBA690` (millions of vertices) crashed `R` inside
  [`ravetools::vcg_smooth_implicit()`](https://dipterix.org/ravetools/reference/vcg_smooth.html);
  with `ravetools` 0.3.3 the full-resolution surface is smoothed in
  about 20 seconds and then reduced to `max_vertices`.
- `YAELProcess$construct_ants_folder_from_template()` wrote
  `ants/mri/transforms/talairach.xfm` from the template-to-native `ANTs`
  affine without inverting it or converting `LPS` to `RAS`, and applied
  the `MNI152`-to-`MNI305` correction to every template. For subjects
  without `rave-imaging/fs`, `MNI305`/`MNI152` electrode coordinates
  derived from this file could be off by several centimeters. The
  transform is now inverted, converted to `RAS`, and corrected only for
  `MNI152` templates; failures are logged and the stale file is removed.
  To repair a subject, run
  `as_yael_process("<project>/<subject>")$write_talairach_xfm("<template>")`,
  then re-save the electrode localization, or re-import an electrode
  table without the `MNI305_*`/`MNI152_*` columns (or with them set to
  zero); existing `MNI` columns are kept as they are.
- [`cmd_run_dcm2niix()`](http://rave.wiki/ravecore/reference/cmd_run_dcm2niix.md)
  now remembers the imported image source under the `yael_preprocess`
  module (it used the old module ID `surface_reconstruction`), as a path
  relative to the subject’s raw folder (or `BIDS` raw folder), so the
  module loader can select it (`#57c7684`).
- Subject pipeline listing is backward compatible with old pipeline
  paths, and listing all pipelines works again (`#f428fc0`, `#e9ebb8e`,
  `#7922564`).
- `filearray` partitions are created before parallel workers start
  during power baseline, preventing a rare write race (`#9dadfec`).
- `rave_slices` is no longer copied when normalization fails
  (`#2dd9c41`).
- Fixed a `read_mat2()` call signature in the per-channel `HDF5`/MATLAB
  importer (`#6872bfc`).
- Fixed a syntax issue in the YAEL process (`#61016f5`).

#### Other Changes

- Added tests that check function-call signatures through
  [`asNamespace()`](https://rdrr.io/r/base/ns-internal.html)
  (`#633cde9`).
- Importing `NEV`/`NSx` data hashes each `NEV` file once per session;
  validation and import used to hash the whole file again for every
  channel type (up to six times per file). The hash still names the
  cache folder, so existing caches are reused.

------------------------------------------------------------------------

## ravecore 0.1.1

CRAN release: 2026-04-02

#### New Features

- Added
  [`generate_atlases_from_template()`](http://rave.wiki/ravecore/reference/generate_atlases_from_template.md)
  to generate brain atlases from a template; supports exporting STL
  meshes in LPS coordinates (`#d2b0f60`, `#c42dbb9`).
- Added `use_antspynet` option in the YAEL preprocessing pipeline
  (`#7795023`).
- Added snapshot report for projects and subjects (`#f91eb3b`).
- Added
  [`install_openneuro()`](http://rave.wiki/ravecore/reference/install_openneuro.md)
  helper to install OpenNeuro subjects (`#1cdffc2`).
- Added quick mode for faster subject preparation (`#70388b4`).
- Added spike diagnostic plots and graphic options for the spike
  visualizer (`#b53887d`, `#f1cc332`).
- Added debug message output for spike sorter workflows (`#fc25b2f`).
- Added utility functions for `SpikeInterface` integration (`#71db36d`).
- Spike analyzer now uses band-passed signal for improved accuracy
  (`#bb021d4`).
- Parallel workers are used to save spike data (`#b4c1b46`).
- Python environment is auto-installed when missing (`#5985f46`).
- Spike sorters are now cached to avoid redundant computation
  (`#2068ce0`).
- Baseline firing rate computation now supports sub-1 rates (`#292f2fe`,
  `#48f3760`).
- Spike histogram bins now cover the entire epoch range (`#d49f9db`,
  `#a9e94ea`).
- Spike train is filtered to ensure times are within the requested bin
  range (`#ca2c1e4`).
- Repository object format has been updated and improved (`#ea2513a`).

#### Bug Fixes

- Fixed HDF5 links not being closed promptly, which could cause resource
  leaks (`#a5e95fe`).
- Fixed `LFP_reference` serialization error and added additional
  validation checks (`#93ad452`, `#f39f51a`).
- Fixed error message displayed when `rpymat` is not configured
  (`#ff5a6b2`).
- Fixed `ensure_py_package()` to install `spikeinterface` via pip
  correctly (`#1bcd358`).
- Fixed ordering issue when the electrode table is empty (`#37904c3`).
- Fixed syntax errors (`#fb14366`).
- Handle missing electrodes gracefully (`#3f6e381`).
- `threeBrain` template is now ensured without throwing an error
  (`#f91eb3b`).
- Invalid projects now display without broken links (`#048c91e`).
- Spike iteration now processes per channel instead of manually
  computing iteration chunks (`#1580465`).
- Only spike data (not raw signals) is stored during sorter runs
  (`#2aa1cff`).
- Reference path is now correctly prioritized (`#39c75bb`).

#### Other Changes

- Lint and code style improvements (`#afae460`).
- Minor documentation fixes (`#990fc40`).
- CRAN submission comment fixes (`#004b775`, `#8804222`).

------------------------------------------------------------------------

## ravecore 0.1.0

CRAN release: 2025-09-23

- Initial CRAN submission.
