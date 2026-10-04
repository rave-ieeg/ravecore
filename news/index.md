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

#### Bug Fixes

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
