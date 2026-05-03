# Create a 'YAEL' imaging processing instance

Image registration across different modals. Normalize brain
'T1'-weighted 'MRI' to template brain and generate subject-level atlas
files. See
[`cmd_run_yael_preprocess`](http://rave.wiki/ravecore/reference/cmd_run_yael_preprocess.md)
to see how to run a built-in workflow

## Usage

``` r
as_yael_process(subject)
```

## Arguments

- subject:

  character (subject code, or project name with subject code), or
  [`RAVESubject`](http://rave.wiki/ravecore/reference/RAVESubject.md)
  instance.

## Value

A processing instance, see
[`YAELProcess`](http://rave.wiki/ravecore/reference/YAELProcess.md)

## Examples

``` r

process <- as_yael_process("YAEL/test_subject")

if (FALSE) { # \dontrun{

# Import and set original T1w MRI and CT
process$set_input_image("/path/to/T1w_MRI.nii", type = "T1w")
process$set_input_image("/path/to/CT.nii.gz", type = "CT")

# Co-register CT to MRI
process$register_to_T1w(image_type = "CT")

# Morph T1w MRI to 0.5 mm^3 MNI152 template
process$map_to_template(
  template_name = "mni_icbm152_nlin_asym_09b",
  native_type = "T1w"
)

} # }

```
