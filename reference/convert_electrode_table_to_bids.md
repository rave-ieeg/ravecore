# Convert electrode table

Convert electrode table

## Usage

``` r
convert_electrode_table_to_bids(
  subject,
  space = c("ScanRAS", "MNI305", "fsnative")
)
```

## Arguments

- subject:

  'RAVE' subject

- space:

  suggested coordinate space, notice this argument might not be
  supported when `'FreeSurfer'` reconstruction is missing.

## Value

A list of table in data frame and a list of meta information

## Examples

``` r
# Run `install_subject("DemoSubject")` first!
if( has_rave_subject("demo/DemoSubject") ) {

convert_electrode_table_to_bids(
  "demo/DemoSubject",
  space = "ScanRAS"
)

}
```
