# YAEL-Tweaked FreeSurfer Reconstruction for Macaque Brains

**Function:** `yael_macaque()` · **Source:** `R/workflow-freesurfer_macaque.R`

---

## Overview

Standard `recon-all` cannot be run on macaque T1w MRI without modification: the
human GCA atlas, Talairach seed placement, and voxel-size assumptions all break
for non-human primate data. `yael_macaque()` replaces `recon-all` with a
manually sequenced set of FreeSurfer binaries, substituting each failing step
with a species-aware equivalent derived from the NMT v2 template and YAEL
normalization. The result is a FreeSurfer subject directory that RAVE can
consume identically to a human reconstruction.

The pipeline has two stages that can be run together or independently:

| Stage | Function | Typical runtime |
|---|---|---|
| 1 – Normalization | `yael_macaque_step1()` | ~10 min |
| 2 – Reconstruction | `yael_macaque_step2()` | 1–3 hours |

---

## Prerequisites

- **Python environment** configured via `ravemanager::configure_python()`.
  Required packages: `rpyANTs`, `rpymat`.
- **FreeSurfer 6.x, 7.x, 8.x** installed and registered via
  `ravepipeline::raveio_setopt('freesurfer_path', '/path/to/freesurfer')`.
  FreeSurfer 8.0.0 has been explicitly tested.
- **NMT v2 template** (`nmt_v2_0_acpc_asym`). Downloaded automatically on
  first use via `call_rpyants("ensure_template", ...)`.
- **PREEMACS curvature atlas** (`lh/rh.PREEMACS_34_v1.tif`). Shipped alongside
  the NMT v2 template. Required for surface registration; if absent, the sphere
  is copied as-is with a warning.
- The T1w input must be **ACPC-aligned**.

---

## Stage 1 – Normalization (`yael_macaque_step1`)

### A. Subject initialization

The RAVE subject object is created and its imaging directory structure
initialized with `subject$initialize_paths(include_freesurfer = TRUE)`. If the
input image is not NIfTI it is converted and saved under
`<imaging>/inputs/MRI/MRI_for_fs.nii.gz`.

### B. YAEL normalization

`yael_preprocess()` is called with:

- `normalize_template = "nmt_v2_0_acpc_asym"` – NMT v2 ACPC asymmetric macaque
  template.
- `normalize_affine = <template>/I4.mat` – the identity matrix, which suppresses
  the affine pre-alignment that YAEL normally applies. This is intentional: the
  input is already ACPC-aligned, so an extra affine would misregister it.
- `use_antspynet = FALSE` – ANTsPyNet brain extraction is disabled because
  macaque brains fall outside the training distribution of the human-tuned
  network. The brain mask is instead derived from the NMT template mask (see
  step C).
- `add_surfaces = FALSE` – surface generation is deferred to stage 2.

The deformable SyN registration maps the subject T1w to NMT v2, producing
forward and inverse warp fields stored in the YAEL process directory.

### C. FreeSurfer folder scaffolding and raw file preparation

A fresh FreeSurfer folder is created at `<imaging>/<folder_name>/` with the
standard subdirectory tree: `mri/`, `mri/raw/`, `mri/orig/`, `mri/transforms/`,
`surf/`, `label/`, `scripts/`, `stats/`, `tmp/`, `touch/`, `trash/`.

Three images are written to `mri/raw/`:

| File | Source |
|---|---|
| `T1.nii.gz` | YAEL T1w input (native space) |
| `brainmask.nii.gz` | NMT template brain mask warped back to native space |
| `aparc+aseg.nii.gz` | NMT `aparc+aseg` atlas warped back to native space |

The inverse warp (template → native) is applied via
`yael_process$transform_image_from_template()`. For the atlas, nearest-neighbour
interpolation preserves integer label values.

The ANTs affine component of the template-to-native transform is extracted and
converted from ANTs LPS convention to RAS convention:

```
affine_RAS = diag(-1,-1,1,1) %*% affine_LPS %*% diag(-1,-1,1,1)
```

This is written as an MNI `.xfm` file (`affine.xfm`), and again with the
translation column scaled by `rescale = 2` (`affine_x2.xfm`), for use as the
Talairach transform in stage 2.

### D. Auto-crop and fake-space rescaling

**Auto-crop.** The native brain mask is eroded (morphological dilation with
radius 3, 26-connectivity threshold, then inverted) to remove isolated voxels,
and the tight bounding box of the remaining foreground is computed. An 8-voxel
margin is added on each side and the box is clamped to the volume dimensions.
All three images (T1, mask, atlas) are cropped to this box and saved as
`*_cropped.nii.gz`. The vox2ras affine is updated to reflect the new origin.

**Fake-space rescaling.** A macaque brain at typical 0.5 mm isotropic resolution
spans ~130 mm and ~260 voxels. FreeSurfer's atlas registration tools assume a
~256 mm field-of-view (human scale). Multiplying the vox2ras matrix diagonal by
`rescale = 2` makes each voxel appear 1 mm wide while keeping the same data,
effectively placing the brain in a human-scale coordinate system without
resampling any voxel. All three images are saved again as `*_x2.nii.gz` with
this inflated affine. The Talairach translation (`affine_x2.xfm`) is scaled
consistently so FreeSurfer header embedding remains coherent.

> **Why ×2?** A 0.5 mm macaque scan → ×2 → apparent 1 mm, matching the 1 mm
> human convention. The factor is hard-coded because it is tied to the
> typical NMT v2 resolution and FreeSurfer's 256 mm FOV expectation.

---

## Stage 2 – Reconstruction (`yael_macaque_step2`)

All FreeSurfer commands are executed via `rpymat::run_command()` after sourcing
`$FREESURFER_HOME/SetUpFreeSurfer.sh`. Key environment variables set for every
call:

| Variable | Value |
|---|---|
| `FREESURFER_HOME` | FreeSurfer installation root |
| `SUBJECTS_DIR` | Subject imaging directory |
| `YAEL_ROOT` | `<imaging>/<folder_name>` (the FS subject dir) |
| `YAEL_DNAME` | `folder_name` (used as the FS subject ID) |
| `DKT_GCA` | Path to `lh.DKaparc.*.gcs` (latest version found) |

If the imaging path contains characters outside `[a-zA-Z0-9./\\_-]` (spaces,
accents, etc.) a temporary symlink to a safe cache path is created, because
FreeSurfer binaries do not handle special characters in `$SUBJECTS_DIR`.

### Step 2-A. Volume conversion and bias correction

```bash
mri_convert -i mri/raw/T1_x2.nii.gz   -o mri/orig/001.mgz
mri_convert -c mri/orig/001.mgz            mri/rawavg.mgz   # conform to 256³, 1 mm
cp mri/rawavg.mgz  mri/orig.mgz
cp mri/rawavg.mgz  mri/T1.mgz
mri_convert -c mri/raw/brainmask_x2.nii.gz  mri/brainmask_x2.auto.mgz
mri_nu_correct.mni --i mri/orig.mgz --o mri/nu.mgz
```

The fake-space T1 is conformed to the standard FreeSurfer 256³ 1 mm grid and
N3 bias-field correction is applied.

### Step 2-B. Intensity rescaling of `nu.mgz`

**Problem.** GCA-based tools (`mri_em_register`, `mri_ca_normalize`) expect the
85th-percentile brain intensity (a WM proxy) to be approximately 110. A
skull-stripped macaque T1 stored as UCHAR typically has WM ≈ 220. In FreeSurfer
8 the histogram search in `mri_em_register` returns empty, producing a blank
`norm.mgz`.

**Fix.** The 85th percentile of all nonzero voxels in `nu.mgz` is computed in R.
If it exceeds 130 the entire volume is multiplied by `110 / wm_est`, clamped to
[0, 255], and written back. The rescaled volume is saved as `nu.nii.gz` then
re-conformed to `nu.mgz` via `mri_convert -c`.

`norm.mgz` is initialized as a copy of `nu.mgz` (WM ≈ 110 after rescaling);
`mri_ca_normalize` is not run.

### Step 2-C. Talairach transform

**Standard path (skipped).** `talairach_avi` registers `nu.mgz` to MNI305 to
produce `transforms/talairach.auto.xfm`. For macaque this registration is
unreliable.

**Bypass.** The affine transform from the YAEL normalization (`affine_x2.xfm`,
already in MNI `.xfm` format and already in fake-space) is copied directly to
`transforms/talairach.auto.xfm` and `transforms/talairach.xfm`. The transform
is embedded into the headers of `nu.mgz` and `brainmask.auto.mgz` with
`mri_add_xform_to_header`.

`talairach.lta` is generated via:

```bash
lta_convert --inxfm transforms/talairach.xfm \
            --outlta transforms/talairach.lta \
            --src mri/nu.mgz \
            --trg $FREESURFER_HOME/average/mni305.cor.mgz
```

This produces the LTA file expected by downstream tools without running the
unreliable human atlas registration.

### Step 2-D. Segmentation (no human GCA)

**Standard path (skipped).** `mri_em_register` + `mri_ca_normalize` +
`mri_ca_label` use the human GCA (`RB_all_*.gca`) to produce `norm.mgz` and
`aseg.auto_noCCseg.mgz`. These consistently fail on macaque: PREEMACS (the
previous macaque pipeline) required FreeSurfer 6 for its more permissive
histogram matching; FreeSurfer 8 is stricter.

**Bypass.** The NMT `aparc+aseg` atlas warped to native space (and into
fake-space via `*_x2.nii.gz`) is converted directly to
`aseg.auto_noCCseg.mgz`:

```bash
mri_convert -rt nearest -odt int -c \
    mri/raw/aparc+aseg_x2.nii.gz  mri/aseg.auto_noCCseg.mgz
```

Copies are made to `aseg.auto.mgz` and `aseg.presurf.mgz`.

Brain normalization avoids the `-mprage` flag (which triggers GCA-based WM
calibration and fails on macaque):

```bash
mri_normalize -aseg mri/aseg.presurf.mgz \
              -mask mri/brainmask.mgz \
              mri/norm.mgz  mri/brain.mgz
```

WM segmentation and editing proceed normally:

```bash
mri_segment    -mprage  mri/brain.mgz  mri/wm.seg.mgz
mri_edit_wm_with_aseg -keep-in mri/wm.seg.mgz mri/brain.mgz \
                        mri/aseg.presurf.mgz  mri/wm.asegedit.mgz
mri_pretess    mri/wm.asegedit.mgz wm  mri/norm.mgz  mri/wm.mgz
```

> Note: `-mprage` is retained in `mri_segment` because that flag controls a
> different behavior (MPRAGE acquisition model) that is beneficial; it does not
> invoke GCA-based calibration in this tool.

### Step 2-E. Hemisphere filling (bypass of `mri_fill`)

**Standard path (skipped).** `mri_fill` determines LH/RH seeds by
back-projecting fixed human Talairach coordinates through `talairach.lta`. For
macaques the registration is too distorted; seeds land outside the volume (e.g.
"rh seed out of bounds (94, -321, 133)").

**Bypass.** The NMT atlas label set already encodes hemisphere. WM voxels in
`wm.mgz` are labelled directly from `aseg.auto_noCCseg.mgz`:

| Condition | `filled.mgz` value |
|---|---|
| WM voxel AND aseg ∈ LH labels (1–6, 9–15, 17–31) | 255 |
| WM voxel AND aseg ∈ RH labels (40–45, 49–63) | 127 |
| Otherwise | 0 |

Cerebellum (labels 7–8, 46–47) and brainstem (label 16) are deliberately
excluded so they are not tessellated as part of the cortical surface.

The filled volume is written as `filled.nii.gz` then re-conformed to `filled.mgz`
via `mri_convert -odt uchar`.

### Step 2-F. Surface tessellation and topology correction

Standard FreeSurfer surface generation proceeds from the filled volume:

```bash
# LH (value 255)
mri_pretess filled.mgz 255 norm.mgz filled-pretess-255.mgz
mri_tessellate filled-pretess-255.mgz 255 surf/lh.orig.nofix
mris_extract_main_component surf/lh.orig.nofix surf/lh.orig.nofix

# RH (value 127) – symmetric
...

# Smooth, inflate, quick-sphere (topology pre-check)
mris_smooth  -nw -seed 1234  surf/lh.orig.nofix     surf/lh.smoothwm.nofix
mris_inflate -no-save-sulc   surf/lh.smoothwm.nofix surf/lh.inflated.nofix
mris_sphere  -q -seed 1234   surf/lh.inflated.nofix surf/lh.qsphere.nofix

# Fix topology
mris_fix_topology -mgz -sphere qsphere.nofix -ga -seed 1234 <folder_name> lh
mris_euler_number surf/lh.orig
mris_remove_intersection surf/lh.orig surf/lh.orig
```

### Step 2-G. White surface and spherical inflation

```bash
mris_make_surfaces -aseg aseg.presurf -white white.preaparc \
                   -noaparc -whiteonly -mgz -T1 brain.finalsurfs \
                   <folder_name> lh

mris_smooth -n 3 -nw -seed 1234  surf/lh.white.preaparc  surf/lh.smoothwm
mris_inflate                      surf/lh.smoothwm         surf/lh.inflated

# Curvature (required input for mris_register)
mris_curvature -w surf/lh.white.preaparc
mris_curvature -thresh .999 -n -a 5 -w -distances 10 10 surf/lh.inflated

mris_sphere -seed 1234  surf/lh.inflated  surf/lh.sphere
```

### Step 2-H. Surface registration to NMT template

`mris_register` aligns the subject sphere to the PREEMACS curvature atlas
(`lh/rh.PREEMACS_34_v1.tif`) bundled with the NMT v2 template. This is the
macaque-specific analogue of the human `fsaverage` registration.

```bash
mris_register -curv surf/lh.sphere  <lh.PREEMACS_34_v1.tif>  surf/lh.sphere.reg
mris_jacobian surf/lh.white.preaparc surf/lh.sphere.reg surf/lh.jacobian_white
mrisp_paint -a 5  <lh.PREEMACS_34_v1.tif>#6  surf/lh.sphere.reg  surf/lh.avg_curv
```

If the `.tif` file is absent (template incomplete), the sphere is copied as
`sphere.reg` with a warning and registration is skipped.

### Step 2-I. Cortical parcellation

The DK atlas GCS (`lh.DKaparc.*.gcs`, latest version discovered under
`$FREESURFER_HOME/average/`) is used with `mris_ca_label`. The human DK atlas
labels on macaque are anatomically inaccurate, but the coarse label boundaries
are sufficient to guide `mris_make_surfaces` pial placement.

```bash
mris_ca_label -l label/lh.cortex.label \
              -aseg mri/aseg.presurf.mgz \
              -seed 1234 \
              <folder_name> lh surf/lh.sphere.reg ${DKT_GCA} \
              label/lh.aparc.annot
```

### Step 2-J. Pial surface and ribbon

```bash
mris_make_surfaces -orig_white white.preaparc -orig_pial white.preaparc \
                   -max 10 -aseg aseg.presurf -mgz -T1 brain.finalsurfs \
                   <folder_name> lh

mris_volmask --aseg_name aseg.presurf \
             --label_left_white 2 --label_left_ribbon 3 \
             --label_right_white 41 --label_right_ribbon 42 \
             --save_ribbon <folder_name>

mris_smooth -nw -n 10 surf/lh.pial  surf/lh.pial-outer-smoothed
```

---

## Output directory layout

```
<imaging>/<folder_name>/
├── mri/
│   ├── raw/               intermediate NIfTI inputs (T1, mask, atlas; cropped & x2)
│   ├── orig/001.mgz       fake-space T1 (pre-conform)
│   ├── T1.mgz             conformed T1
│   ├── nu.mgz             bias-corrected, intensity-rescaled
│   ├── norm.mgz           = nu.mgz
│   ├── brain.mgz          normalized, skull-stripped
│   ├── brain.finalsurfs.mgz
│   ├── wm.mgz             white-matter mask
│   ├── filled.mgz         hemisphere-labelled WM (LH=255, RH=127)
│   ├── aseg.presurf.mgz   NMT atlas labels (fake space)
│   ├── brainmask.mgz
│   └── transforms/
│       ├── talairach.xfm  (from YAEL affine, scaled ×2)
│       └── talairach.lta
├── surf/
│   ├── lh.white.preaparc, lh.pial, lh.inflated, lh.sphere, lh.sphere.reg …
│   └── rh.*  (symmetric)
└── label/
    ├── lh.aparc.annot, lh.cortex.label …
    └── rh.*
```

---

## Key deviations from standard `recon-all`

| Standard step | Problem on macaque | Replacement |
|---|---|---|
| `talairach_avi` | Unreliable MNI registration | YAEL ANTs affine (×2 scaled) |
| `mri_em_register` + `mri_ca_normalize` | Human GCA WM histogram mismatch | Skipped; `nu.mgz` used as `norm.mgz` after intensity rescaling |
| `mri_ca_label` (GCA segmentation) | Human atlas | NMT `aparc+aseg` warped to native/fake space |
| `mri_fill` | Seed coordinates land outside volume | Direct aseg-based hemisphere labelling |
| Surface atlas registration to `fsaverage` | Human curvature atlas | PREEMACS `lh/rh.PREEMACS_34_v1.tif` |
| `mri_normalize -mprage` | GCA WM calibration fails | `mri_normalize` with `-aseg` only |
| Voxel-size assumption (1 mm / 256 FOV) | Macaque ~0.5 mm / ~130 mm | Fake-space ×2 rescaling of vox2ras |
| Brain extraction (ANTsPyNet) | Human-trained model | NMT brain mask morphed to native space |

---

## Citations

When using this pipeline, please cite:

1. Wang, Z., Magnotti, J.F., Zhang, X. and Beauchamp, M.S. (2023). YAEL: Your
   Advanced Electrode Localizer. *eNeuro*, 10(10).
2. Magnotti, J.F., Wang, Z. and Beauchamp, M.S. (2020). RAVE: Comprehensive
   open-source software for reproducible analysis and visualization of
   intracranial EEG data. *NeuroImage*, 223, 117341.
