# Create new electrode channel instance or a reference signal instance

Create new electrode channel instance or a reference signal instance

Create new electrode channel instance or a reference signal instance

## Usage

``` r
new_electrode(subject, number, signal_type, ...)

new_reference(subject, number, signal_type, ...)

new_electrode(subject, number, signal_type, ...)

new_reference(subject, number, signal_type, ...)
```

## Arguments

- subject:

  characters, or a
  [`RAVESubject`](http://rave.wiki/ravecore/reference/RAVESubject.md)
  instance

- number:

  integer in `new_electrode`, or characters in `new_reference`; see
  'Details' and 'Examples'

- signal_type:

  signal type of the electrode or reference; can be automatically
  inferred, but it is highly recommended to specify a value; see
  [`SIGNAL_TYPES`](http://rave.wiki/ravecore/reference/ravecore-constants.md)

- ...:

  other parameters passed to class constructors, respectively

## Value

Electrode or reference instances that inherit
[`RAVEAbstarctElectrode`](http://rave.wiki/ravecore/reference/RAVEAbstarctElectrode.md)
class

Electrode or reference instances that inherit
[`RAVEAbstarctElectrode`](http://rave.wiki/ravecore/reference/RAVEAbstarctElectrode.md)
class

## Details

In `new_electrode`, `number` should be a positive valid integer
indicating the electrode number. In `new_reference`, `number` can be one
of the followings:

- `'noref'`, or `NULL`:

  no reference is needed

- `'ref_X'`:

  where `'X'` is a single number, then the reference is another existing
  electrode; this could occur in bipolar-reference cases

- `'ref_XXX'`:

  `'XXX'` is a combination of multiple electrodes. This could occur in
  common average reference, or white matter reference. One example is
  `'ref_13-16,24'`, meaning the reference signal is an average of
  electrode 13, 14, 15, 16, and 24.

In `new_electrode`, `number` should be a positive valid integer
indicating the electrode number. In `new_reference`, `number` can be one
of the followings:

- `'noref'`, or `NULL`:

  no reference is needed

- `'ref_X'`:

  where `'X'` is a single number, then the reference is another existing
  electrode; this could occur in bipolar-reference cases

- `'ref_XXX'`:

  `'XXX'` is a combination of multiple electrodes that can be parsed by
  [`parse_svec`](https://dipterix.org/dipsaus/reference/parse_svec.html).
  This could occur in common average reference, or white matter
  reference. One example is `'ref_13-16,24'`, meaning the reference
  signal is an average of electrode 13, 14, 15, 16, and 24.

## Examples

``` r
# Download subject demo/DemoSubject
if( has_rave_subject("demo/DemoSubject") ) {


# Electrode 14 in demo/DemoSubject
subject <- as_rave_subject("demo/DemoSubject")
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
power_array <- subset(power, Time ~ Time < 0,
                      Electrode ~ Electrode == 14)

# clear cache on hard disk
e$clear_cache()
ref$clear_cache()

}

# Download subject demo/DemoSubject
if( has_rave_subject("demo/DemoSubject") ) {


# Electrode 14 in demo/DemoSubject
subject <- as_rave_subject("demo/DemoSubject")
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
