# Definitions of electrode with local field potential signal type

Please use a safer
[`new_electrode`](http://rave.wiki/ravecore/reference/new_electrode.md)
function to create instances. This documentation is to describe the
member methods of the electrode class `LFP_electrode`

## Value

if the reference number if `NULL` or `'noref'`, then returns 0,
otherwise returns a
[`FileArray-class`](https://dipterix.org/filearray/reference/FileArray-class.html)

If `simplify` is enabled, and only one block is loaded, then the result
will be a vector (`type="voltage"`) or a matrix (others), otherwise the
result will be a named list where the names are the blocks.

## Super classes

[`ravepipeline::RAVESerializable`](http://dipterix.org/ravepipeline/reference/RAVESerializable.md)
-\>
[`ravecore::RAVEAbstarctElectrode`](http://rave.wiki/ravecore/reference/RAVEAbstarctElectrode.md)
-\> `LFP_electrode`

## Active bindings

- `h5_fname`:

  'HDF5' file name

- `valid`:

  whether current electrode is valid: subject exists and contains
  current electrode or reference; subject electrode type matches with
  current electrode type

- `raw_sample_rate`:

  voltage sample rate

- `power_sample_rate`:

  power/phase sample rate

- `preprocess_info`:

  preprocess information

- `power_file`:

  path to power 'HDF5' file

- `phase_file`:

  path to phase 'HDF5' file

- `voltage_file`:

  path to voltage 'HDF5' file

## Methods

### Public methods

- [`LFP_electrode$@marshal()`](#method-LFP_electrode-@marshal)

- [`LFP_electrode$@unmarshal()`](#method-LFP_electrode-@unmarshal)

- [`LFP_electrode$print()`](#method-LFP_electrode-print)

- [`LFP_electrode$set_reference()`](#method-LFP_electrode-set_reference)

- [`LFP_electrode$new()`](#method-LFP_electrode-new)

- [`LFP_electrode$.load_noref_wavelet()`](#method-LFP_electrode-.load_noref_wavelet)

- [`LFP_electrode$.load_noref_voltage()`](#method-LFP_electrode-.load_noref_voltage)

- [`LFP_electrode$.load_wavelet()`](#method-LFP_electrode-.load_wavelet)

- [`LFP_electrode$.load_voltage()`](#method-LFP_electrode-.load_voltage)

- [`LFP_electrode$.load_raw_voltage()`](#method-LFP_electrode-.load_raw_voltage)

- [`LFP_electrode$load_data_with_epochs()`](#method-LFP_electrode-load_data_with_epochs)

- [`LFP_electrode$load_dimnames_with_epochs()`](#method-LFP_electrode-load_dimnames_with_epochs)

- [`LFP_electrode$load_data_with_blocks()`](#method-LFP_electrode-load_data_with_blocks)

- [`LFP_electrode$load_dim_with_blocks()`](#method-LFP_electrode-load_dim_with_blocks)

- [`LFP_electrode$clear_cache()`](#method-LFP_electrode-clear_cache)

- [`LFP_electrode$clear_memory()`](#method-LFP_electrode-clear_memory)

- [`LFP_electrode$clone()`](#method-LFP_electrode-clone)

Inherited methods

- [`ravepipeline::RAVESerializable$@compare()`](http://dipterix.org/ravepipeline/reference/RAVESerializable.html#method-@compare)
- [`ravecore::RAVEAbstarctElectrode$load_blocks()`](http://rave.wiki/ravecore/reference/RAVEAbstarctElectrode.html#method-load_blocks)
- [`ravecore::RAVEAbstarctElectrode$load_data()`](http://rave.wiki/ravecore/reference/RAVEAbstarctElectrode.html#method-load_data)
- [`ravecore::RAVEAbstarctElectrode$set_epoch()`](http://rave.wiki/ravecore/reference/RAVEAbstarctElectrode.html#method-set_epoch)

------------------------------------------------------------------------

### Method `@marshal()`

Internal method

#### Usage

    LFP_electrode$@marshal(...)

#### Arguments

- `...`:

  internal arguments

------------------------------------------------------------------------

### Method `@unmarshal()`

Internal method

#### Usage

    LFP_electrode$@unmarshal(object)

#### Arguments

- `object, ...`:

  internal arguments

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

print electrode summary

#### Usage

    LFP_electrode$print()

------------------------------------------------------------------------

### Method `set_reference()`

set reference for current electrode

#### Usage

    LFP_electrode$set_reference(reference)

#### Arguments

- `reference`:

  either `NULL` or `LFP_electrode` instance

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    LFP_electrode$new(subject, number, quiet = FALSE)

#### Arguments

- `subject, number, quiet`:

  see constructor in
  [`RAVEAbstarctElectrode`](http://rave.wiki/ravecore/reference/RAVEAbstarctElectrode.md)

------------------------------------------------------------------------

### Method `.load_noref_wavelet()`

load non-referenced wavelet coefficients (internally used)

#### Usage

    LFP_electrode$.load_noref_wavelet(reload = FALSE)

#### Arguments

- `reload`:

  whether to reload cache

------------------------------------------------------------------------

### Method `.load_noref_voltage()`

load non-referenced voltage (internally used)

#### Usage

    LFP_electrode$.load_noref_voltage(reload = FALSE)

#### Arguments

- `reload`:

  whether to reload cache

- `srate`:

  voltage signal sample rate

------------------------------------------------------------------------

### Method `.load_wavelet()`

load referenced wavelet coefficients (internally used)

#### Usage

    LFP_electrode$.load_wavelet(
      type = c("power", "phase", "wavelet-coefficient"),
      reload = FALSE
    )

#### Arguments

- `type`:

  type of data to load

- `reload`:

  whether to reload cache

------------------------------------------------------------------------

### Method `.load_voltage()`

load referenced voltage (internally used)

#### Usage

    LFP_electrode$.load_voltage(reload = FALSE)

#### Arguments

- `reload`:

  whether to reload cache

------------------------------------------------------------------------

### Method `.load_raw_voltage()`

load raw voltage (no process)

#### Usage

    LFP_electrode$.load_raw_voltage(reload = FALSE)

#### Arguments

- `reload`:

  whether to reload cache

------------------------------------------------------------------------

### Method `load_data_with_epochs()`

method to load electrode data

#### Usage

    LFP_electrode$load_data_with_epochs(
      type = c("power", "phase", "voltage", "wavelet-coefficient", "raw-voltage")
    )

#### Arguments

- `type`:

  data type such as `"power"`, `"phase"`, `"voltage"`,
  `"wavelet-coefficient"`, and `"raw-voltage"`. For `"power"`,
  `"phase"`, and `"wavelet-coefficient"`, 'Wavelet' transforms are
  required. For `"voltage"`, 'Notch' filters must be applied. All these
  types except for `"raw-voltage"` will be referenced. For
  `"raw-voltage"`, no reference will be performed since the data will be
  the "raw" signal (no processing).

------------------------------------------------------------------------

### Method `load_dimnames_with_epochs()`

get expected dimension names

#### Usage

    LFP_electrode$load_dimnames_with_epochs(
      type = c("power", "phase", "voltage", "wavelet-coefficient", "raw-voltage")
    )

#### Arguments

- `type`:

  see `load_data_with_epochs`

------------------------------------------------------------------------

### Method `load_data_with_blocks()`

load electrode block-wise data (with no reference), useful when epoch is
absent

#### Usage

    LFP_electrode$load_data_with_blocks(
      blocks,
      type = c("power", "phase", "voltage", "wavelet-coefficient", "raw-voltage"),
      simplify = TRUE
    )

#### Arguments

- `blocks`:

  session blocks

- `type`:

  data type such as `"power"`, `"phase"`, `"voltage"`, `"raw-voltage"`
  (with no filters applied, as-is from imported),
  `"wavelet-coefficient"`. Note that if type is `"raw-voltage"`, then
  the data only needs to be imported; for `"voltage"` data, 'Notch'
  filters must be applied; for all other types, 'Wavelet' transforms are
  required.

- `simplify`:

  whether to simplify the result

------------------------------------------------------------------------

### Method `load_dim_with_blocks()`

get expected dimension information for block-based loader

#### Usage

    LFP_electrode$load_dim_with_blocks(
      blocks,
      type = c("power", "phase", "voltage", "wavelet-coefficient", "raw-voltage")
    )

#### Arguments

- `blocks, type`:

  see `load_data_with_blocks`

------------------------------------------------------------------------

### Method `clear_cache()`

method to clear cache on hard drive

#### Usage

    LFP_electrode$clear_cache(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `clear_memory()`

method to clear memory

#### Usage

    LFP_electrode$clear_memory(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LFP_electrode$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r

# Download subject demo/DemoSubject

if(has_rave_subject("demo/DemoSubject")) {

subject <- as_rave_subject("demo/DemoSubject", strict = FALSE)

# Electrode 14 in demo/DemoSubject
e <- new_electrode(subject = subject, number = 14, signal_type = "LFP")

# Load CAR reference "ref_13-16,24"
ref <- new_reference(subject = subject, number = "ref_13-16,24",
                     signal_type = "LFP")
e$set_reference(ref)

# Set epoch
e$set_epoch(epoch = 'auditory_onset')

# Set loading window
e$trial_intervals <- list(c(-1, 2))

# Preview
print(e)

# Now epoch power
power <- e$load_data_with_epochs("power")
names(dimnames(power))

# Subset power
subset(power, Time ~ Time < 0, Electrode ~ Electrode == 14)


# clear cache on hard disk
e$clear_cache()
ref$clear_cache()

}
```
