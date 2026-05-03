# Apply Morlet-Wavelet to subject

Calculates time-frequency decomposition; not intended for direct use.
Please use 'RAVE' pipelines (see 'Examples').

## Usage

``` r
run_wavelet(
  subject,
  electrodes,
  freqs,
  cycles,
  target_sample_rate = 100,
  kernels_precision = "float",
  pre_downsample = 1,
  verbose = TRUE
)
```

## Arguments

- subject:

  'RAVE' subject or subject ID

- electrodes:

  electrode channels to apply, must be imported and `'LFP'` type

- freqs:

  numeric vector of frequencies to apply

- cycles:

  number of wavelet cycles at each `freqs`, integers

- target_sample_rate:

  the resulting 'spectrogram' sampling frequency

- kernels_precision:

  double or single (default) floating precision

- pre_downsample:

  down-sample (integer) priory to the decomposition; set to 1 (default)
  to avoid

- verbose:

  whether to verbose the progress

## Value

The decomposition results are stored in 'RAVE' subject data path; the
function only returns the wavelet parameters.

## Details

The channel signals are first down-sampled (optional) by a ratio of
`pre_downsample` via a 'FIR' filter. After the down-sample, 'Morlet'
wavelet kernels are applied to the signals to calculate the wavelet
coefficients (complex number) at each frequency in `freqs`. The number
of `cycles` at each frequency controls the number of sine and cosine
waves, allowing users to balance the time and power accuracy. After the
decomposition, the 'spectrogram' is further down-sampled to
`target_sample_rate`, assuming the brain power is a smooth function over
time. This down-sample is done via time-point sampling to preserve the
phase information (so the linear functions such as common-average or
bi-polar reference can be carried over to the complex coefficients).

## Examples

``` r

# Check https://rave.wiki for additional pipeline installation

if (FALSE) { # \dontrun{

# ---- Recommended usage --------------------------------------------


pipeline <- ravepipeline::pipeline("wavelet_module")
pipeline$set_settings(
  project_name = "demo",
  subject_code = "DemoSubject",
  precision = "float",
  pre_downsample = 4,
  kernel_table = ravetools::wavelet_cycles_suggest(
    freqs = seq(1, 200, by = 1)),
  target_sample_rate = 100
)

# Internally, the above pipeline includes this function call below

# ---- For demonstration use, do not call this function directly ----

# Original sample rate: 2000 Hz
# Downsample by 4 to 500 Hz first - 250 Hz Nyquist
# Wavelet at each 1, 2, ..., 200 Hz
#   The number of cycles log-linear from 2 to 20
#   The wavelet coefficient sample rate is 500 Hz
# Further down-sample to 100 Hz to save storage space

run_wavelet(
  subject = "demo/DemoSubject",
  electrodes = c(13:16, 2),
  pre_downsample = 4,
  freqs = seq(1, 200, by = 1),
  cycles = c(2, 20),
  target_sample_rate = 100
)

} # }

```
