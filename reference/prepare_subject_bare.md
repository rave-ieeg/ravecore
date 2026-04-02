# 'RAVE' repository: basic

'RAVE' repository: basic

## Usage

``` r
prepare_subject_bare(
  subject,
  electrodes = NULL,
  reference_name = NULL,
  ...,
  auto_exclude = FALSE,
  quiet = TRUE,
  repository_id = NULL
)

prepare_subject_bare0(
  subject,
  electrodes = NULL,
  reference_name = NULL,
  ...,
  auto_exclude = FALSE,
  quiet = TRUE,
  repository_id = NULL
)
```

## Arguments

- subject:

  'RAVE' subject

- electrodes:

  string or integers indicating electrodes to load

- reference_name:

  name of the reference table

- ...:

  passed to
  [`RAVESubjectBaseRepository`](http://rave.wiki/ravecore/reference/RAVESubjectBaseRepository.md)
  constructor

- auto_exclude:

  whether to automatically discard bad channels

- quiet:

  see field `quiet`

- repository_id:

  see field `repository_id`

## Value

A
[`RAVESubjectBaseRepository`](http://rave.wiki/ravecore/reference/RAVESubjectBaseRepository.md)
instance

## Examples

``` r
if ( has_rave_subject("demo/DemoSubject") ) {


repository <- prepare_subject_bare0("demo/DemoSubject",
                                    electrodes = 14:16,
                                    reference_name = "default")

print(repository)

repository$subject
repository$subject$raw_sample_rates

repository$electrode_table

repository$reference_table

electrodes <- repository$electrode_instances

# Channel 14
e <- electrodes$e_14

# referenced voltage
voltage <- e$load_data_with_blocks("008", "voltage")

ravetools::diagnose_channel(voltage, srate = 2000)


}
```
