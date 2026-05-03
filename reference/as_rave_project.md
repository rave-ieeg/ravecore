# Convert character to [`RAVEProject`](http://rave.wiki/ravecore/reference/RAVEProject.md) instance

Convert character to
[`RAVEProject`](http://rave.wiki/ravecore/reference/RAVEProject.md)
instance

## Usage

``` r
as_rave_project(x, ...)

# S3 method for class 'character'
as_rave_project(x, strict = TRUE, parent_path = NULL, ...)
```

## Arguments

- x:

  R object that can be converted to 'RAVE' project. When `x` is a
  character, see 'Details' on the rules.

- ...:

  passed to other methods, typically includes `strict` on whether to
  check existence of the project folder, and `parent_path`, specifying
  non-default project root

- strict:

  whether to check project path; if set to true and the project path is
  missing, the program will raise warnings

- parent_path:

  parent path in which the project is non-default, can be a path to the
  parent folder of the project, or a
  [`bids_project`](https://dipterix.org/bidsr/reference/bids_project.html)
  object. When the subject is from 'BIDS', the `parent_path` must be the
  root of 'BIDS' directory.

## Value

A [`RAVEProject`](http://rave.wiki/ravecore/reference/RAVEProject.md)
instance

## Details

A 'RAVE' project is an aggregation of subjects with the similar research
targets. For example, 'RAVE' comes with a demo subject set, and the
project 'demo' contains eight subjects undergoing same experiments.
Project `'YAEL'` contains subject whose electrodes are localized by
`'YAEL'` modules. The project can be "arbitrary": this is different to a
'BIDS' "project", often served as a data-set name or identifier. A
'BIDS' project may have multiple 'RAVE' projects. For example, an
audio-visual 'BIDS' data may have a 'RAVE' project `'McGurk'` to study
the `'McGurk'` effect and another `'synchrony'` to study the
audio-visual synchronization.

A valid 'RAVE' project name must only contain letters and digits;
underscores and dashes may be acceptable but might subject to future
change. For example `'demo'` is a valid project name, but `'my demo'` is
invalid.

RAVE supports storing the data in `'native'` or `'bids'`-compliant
formats. The native format is compatible with the 'RAVE' 1.0 and 2.0,
and requires no conversion to 'BIDS' format, while `'bids'` requires the
data to be stored and processed in 'BIDS'-complaint format, which is
better for data sharing and migration, but might be over-kill in some
cases.

If the project string contains `'@'`, the characters after the 'at' sign
will be interpreted as indication of the storage format. For instance
`'demo@native'` or `'demo@bids:ds0001'` are interpreted differently. The
previous one indicates that the project `'demo'` is stored with native
format, usually located at `'rave_data/data_dir'` under the home
directory (can be manually set to other locations). The latter one means
the 'RAVE' project `'demo'` is stored under 'BIDS' folder with a 'BIDS'
data-set name `'ds0001'`.

## See also

[`RAVEProject`](http://rave.wiki/ravecore/reference/RAVEProject.md)

## Examples

``` r


# ---- Native format (RAVE legacy) ------------------------
project <- as_rave_project("demo", strict = FALSE)

format(project)
#> [1] "demo"

project$path
#> /home/runner/rave_data/data_dir/demo

project$subjects()
#> character(0)

# Non-standard project locations (native format)
as_rave_project("demo", strict = FALSE,
                parent_path = "~/Downloads")
#> RAVE project <demo>
#>   Format standard: native 
#>   Directory: /home/runner/Downloads/demo 
#>   Subjects :  
#> Field/Method: @impl, format_standard, pipeline_path, name, path, subject_pipelines, group_path, has_subject, subjects, format, @compare 


# ---- BIDS format ----------------------------------------
project <- as_rave_project("demo@bids:ds001", strict = FALSE)

format(project)
#> [1] "demo@bids:ds001"

project$path
#> /home/runner/rave_data/bids_dir/ds001/derivatives/rave/data_dir/demo

# BIDS format, given the parent folder; this example requires
# 'bidsr' sample data. Run `bidsr::download_bids_examples()` first.

examples <- bidsr::download_bids_examples(test = TRUE)

if(!isFALSE(examples)) {

  project <- as_rave_project(
    "audiovisual@bids", strict = FALSE,
    parent_path = file.path(examples, "ieeg_epilepsy_ecog"))

  # RAVE processed data is under BIDS dirivative folder
  project$path

  # "audiovisual@bids:ieeg_epilepsy_ecog"
  format(project)
}


```
