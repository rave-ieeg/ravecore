# Load or save meta data to 'RAVE' subject

Load or save meta data to 'RAVE' subject

## Usage

``` r
save_meta2(data, meta_type, project_name, subject_code)

load_meta2(
  meta_type = c("electrodes", "frequencies", "time_points", "epoch", "references",
    "time_excluded", "info"),
  project_name,
  subject_code,
  subject_id,
  meta_name
)
```

## Arguments

- data:

  data table

- meta_type:

  see load meta

- project_name:

  project name

- subject_code:

  subject code

- subject_id:

  subject identified, alternative way to specify the project and subject
  in one string

- meta_name:

  for epoch and reference only, the name the of the table

## Value

The corresponding metadata

## Examples

``` r

if(has_rave_subject("demo/DemoSubject")) {
  subject <- as_rave_subject("demo/DemoSubject", strict = FALSE)

  electrode_table <- subject$get_electrode_table()

  save_meta2(
    data = electrode_table,
    meta_type = "electrodes",
    project_name = subject$project_name,
    subject_code = subject$subject_code
  )

  load_meta2(meta_type = "electrodes", subject_id = subject)
}
```
