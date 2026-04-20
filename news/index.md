# Changelog

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
