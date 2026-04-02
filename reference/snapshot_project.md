# Create overview report for given project or subject

Create overview report for given project or subject

## Usage

``` r
snapshot_subject(x, target_path = NULL, quick = FALSE)

snapshot_project(
  x,
  target_path = NULL,
  template_subjects = NULL,
  quick = FALSE
)
```

## Arguments

- x:

  characters or a 'RAVE' project or subject instance

- target_path:

  directory where the snapshot data will be stored; default is under
  `'project_overview'` group data folder, see method `group_data` from
  [`RAVEProject`](http://rave.wiki/ravecore/reference/RAVEProject.md)
  class.

- quick:

  whether to skip certain validations and subjects if snapshot reports
  have already existed

- template_subjects:

  a vector of characters of template brain to be used for generating the
  group brain; see
  [`available_templates`](https://dipterix.org/threeBrain/reference/template_subject.html)
  for available templates.

## Value

`snapshot_subject` returns a list of subject summary and the calculated
target path; `snapshot_project` returns the path to the compiled project
report in 'HTML'.

## Examples

``` r
if (FALSE) { # \dontrun{

# Run in parallel
ravepipeline::with_rave_parallel({

  snapshot_project("demo")

})

} # }
```
