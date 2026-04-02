# Get names of available non-linear transforms to the templates

This function obtains existing transforms rather than creating new
transforms. Please check function
[`cmd_run_yael_preprocess`](http://rave.wiki/ravecore/reference/cmd_run_yael_preprocess.md)
about mapping native images to the templates. This function requires
additional 'Python' configuration.

## Usage

``` r
get_available_morph_to_template(subject)
```

## Arguments

- subject:

  'RAVE' subject instance or character; see
  [`as_rave_subject`](http://rave.wiki/ravecore/reference/new_rave_subject.md)

## Value

Template names with valid non-linear transforms.

## Examples

``` r
# Please check out https://rave.wiki to configure Python for RAVE
# or run ravemanager::configure_python()
if (FALSE) { # \dontrun{

get_available_morph_to_template("project_name/subject_code")

} # }

```
