# Class definition for micro-wire spike channels

Class definition for micro-wire spike channels

Class definition for micro-wire spike channels

## Value

If `simplify` is enabled, and only one block is loaded, then the result
will be a vector (`type="voltage"`) or a matrix (others), otherwise the
result will be a named list where the names are the blocks.

## Super classes

[`ravepipeline::RAVESerializable`](http://dipterix.org/ravepipeline/reference/RAVESerializable.md)
-\>
[`ravecore::RAVEAbstarctElectrode`](http://rave.wiki/ravecore/reference/RAVEAbstarctElectrode.md)
-\> `Spike_electrode`

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

- `voltage_file`:

  path to voltage 'HDF5' file

## Methods

### Public methods

- [`Spike_electrode$@marshal()`](#method-Spike_electrode-@marshal)

- [`Spike_electrode$@unmarshal()`](#method-Spike_electrode-@unmarshal)

- [`Spike_electrode$print()`](#method-Spike_electrode-print)

- [`Spike_electrode$set_reference()`](#method-Spike_electrode-set_reference)

- [`Spike_electrode$new()`](#method-Spike_electrode-new)

- [`Spike_electrode$.load_noref_voltage()`](#method-Spike_electrode-.load_noref_voltage)

- [`Spike_electrode$.load_raw_voltage()`](#method-Spike_electrode-.load_raw_voltage)

- [`Spike_electrode$load_data_with_epochs()`](#method-Spike_electrode-load_data_with_epochs)

- [`Spike_electrode$load_dimnames_with_epochs()`](#method-Spike_electrode-load_dimnames_with_epochs)

- [`Spike_electrode$load_data_with_blocks()`](#method-Spike_electrode-load_data_with_blocks)

- [`Spike_electrode$load_dim_with_blocks()`](#method-Spike_electrode-load_dim_with_blocks)

- [`Spike_electrode$clear_cache()`](#method-Spike_electrode-clear_cache)

- [`Spike_electrode$clear_memory()`](#method-Spike_electrode-clear_memory)

- [`Spike_electrode$clone()`](#method-Spike_electrode-clone)

Inherited methods

- [`ravepipeline::RAVESerializable$@compare()`](http://dipterix.org/ravepipeline/reference/RAVESerializable.html#method-@compare)
- [`ravecore::RAVEAbstarctElectrode$load_blocks()`](http://rave.wiki/ravecore/reference/RAVEAbstarctElectrode.html#method-load_blocks)
- [`ravecore::RAVEAbstarctElectrode$load_data()`](http://rave.wiki/ravecore/reference/RAVEAbstarctElectrode.html#method-load_data)
- [`ravecore::RAVEAbstarctElectrode$set_epoch()`](http://rave.wiki/ravecore/reference/RAVEAbstarctElectrode.html#method-set_epoch)

------------------------------------------------------------------------

### Method `@marshal()`

Internal method

#### Usage

    Spike_electrode$@marshal(...)

#### Arguments

- `...`:

  internal arguments

------------------------------------------------------------------------

### Method `@unmarshal()`

Internal method

#### Usage

    Spike_electrode$@unmarshal(object)

#### Arguments

- `object, ...`:

  internal arguments

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

print electrode summary

#### Usage

    Spike_electrode$print()

------------------------------------------------------------------------

### Method `set_reference()`

set reference for current electrode

#### Usage

    Spike_electrode$set_reference(reference)

#### Arguments

- `reference`:

  either `NULL` or `LFP_electrode` instance

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    Spike_electrode$new(subject, number, quiet = FALSE)

#### Arguments

- `subject, number, quiet`:

  see constructor in
  [`RAVEAbstarctElectrode`](http://rave.wiki/ravecore/reference/RAVEAbstarctElectrode.md)

------------------------------------------------------------------------

### Method `.load_noref_voltage()`

load non-referenced voltage (internally used)

#### Usage

    Spike_electrode$.load_noref_voltage(reload = FALSE)

#### Arguments

- `reload`:

  whether to reload cache

- `srate`:

  voltage signal sample rate

------------------------------------------------------------------------

### Method `.load_raw_voltage()`

load raw voltage (no process)

#### Usage

    Spike_electrode$.load_raw_voltage(reload = FALSE)

#### Arguments

- `reload`:

  whether to reload cache

------------------------------------------------------------------------

### Method `load_data_with_epochs()`

method to load electrode data

#### Usage

    Spike_electrode$load_data_with_epochs(type = c("raw-voltage", "voltage"))

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

    Spike_electrode$load_dimnames_with_epochs(type = c("raw-voltage", "voltage"))

#### Arguments

- `type`:

  see `load_data_with_epochs`

------------------------------------------------------------------------

### Method `load_data_with_blocks()`

load electrode block-wise data (with no reference), useful when epoch is
absent

#### Usage

    Spike_electrode$load_data_with_blocks(
      blocks,
      type = c("raw-voltage", "voltage"),
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

    Spike_electrode$load_dim_with_blocks(
      blocks,
      type = c("raw-voltage", "voltage")
    )

#### Arguments

- `blocks, type`:

  see `load_data_with_blocks`

------------------------------------------------------------------------

### Method `clear_cache()`

method to clear cache on hard drive

#### Usage

    Spike_electrode$clear_cache(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `clear_memory()`

method to clear memory

#### Usage

    Spike_electrode$clear_memory(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Spike_electrode$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
