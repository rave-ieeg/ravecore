# Get [`RAVESubject`](http://rave.wiki/ravecore/reference/RAVESubject.md) instance from character

Get [`RAVESubject`](http://rave.wiki/ravecore/reference/RAVESubject.md)
instance from character

## Usage

``` r
new_rave_subject(project_name, subject_code, strict = TRUE)

as_rave_subject(subject_id, strict = TRUE, reload = TRUE)

has_rave_subject(subject_id)
```

## Arguments

- project_name:

  character of 'RAVE' project name

- subject_code:

  character of 'RAVE' subject code

- strict:

  whether to check if subject directories exist or not

- subject_id:

  character in format `"project/subject"`

- reload:

  whether to reload (update) subject information, default is true

## Value

[`RAVESubject`](http://rave.wiki/ravecore/reference/RAVESubject.md)
instance

## See also

[`RAVESubject`](http://rave.wiki/ravecore/reference/RAVESubject.md)

## Examples

``` r

subject <- new_rave_subject(project_name = "demo@bids:ds04001",
                            subject_code = "DemoSubject",
                            strict = FALSE)

subject
#> RAVE subject <demo@bids:ds04001/DemoSubject>

subject$project$path
#> /home/runner/rave_data/bids_dir/ds04001/derivatives/rave/data_dir/demo
subject$imaging_path
#> /home/runner/rave_data/bids_dir/ds04001/derivatives/rave/raw_dir/sub-DemoSubject/rave-imaging
```
