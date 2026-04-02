# External shell commands for 'RAVE'

These shell commands are only tested on 'MacOS' and 'Linux'. On
'Windows' machines, please use the 'WSL2' system.

## Usage

``` r
cmd_execute(
  script,
  script_path,
  command = "bash",
  dry_run = FALSE,
  backup = TRUE,
  args = NULL,
  ...
)

cmd_run_r(
  expr,
  quoted = FALSE,
  verbose = TRUE,
  dry_run = FALSE,
  log_file = tempfile(),
  script_path = tempfile(),
  ...
)
```

## Arguments

- script:

  the shell script

- script_path:

  path to run the script

- command:

  which command to invoke; default is `'bash'`

- dry_run:

  whether to run in dry-run mode; under such mode, the shell command
  will not execute. This is useful for debugging scripts; default is
  false

- backup:

  whether to back up the script file immediately; default is true

- args:

  further arguments in the shell command, especially the 'FreeSurfer'
  reconstruction command

- ...:

  passed to [`system2`](https://rdrr.io/r/base/system2.html), or
  additional arguments

- expr:

  expression to run as command

- quoted:

  whether `expr` is quoted; default is false

- verbose:

  whether to print out the command script; default is true under dry-run
  mode, and false otherwise

- log_file:

  where should log file be stored

## Value

A list of data containing the script details:

- `script`:

  script details

- `script_path`:

  where the script should/will be saved

- `dry_run`:

  whether dry-run mode is turned on

- `log_file`:

  path to the log file

- `execute`:

  a function to execute the script
