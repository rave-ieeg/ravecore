# Internal S7 class defining RAVE project

# In native implementation, RAVE project is stored at rave_data/data_dir/project
# However, this does not have to be the case. In BIDS, this can be in
# BIDS_DIR/derivatives/rave/project

RAVEProjectImpl <- S7::new_class(
  name = "RAVEProjectImpl",
  properties = list(
    name = S7::new_property(
      class = S7::class_character,
      setter = function(self, value) {
        value <- trimws(value)
        S7::prop(self, "name") <- value
        self
      },
      validator = function(value) {
        if (length(value) != 1) {
          return("RAVE project name must be a string")
        }
        if (!nzchar(value) ||
            !grepl("^[a-z]", value, ignore.case = TRUE)) {
          return("RAVE project name must not be empty and must starts with a character")
        }
        if (grepl("[^a-zA-Z0-9_-]", value)) {
          return(
            sprintf("RAVE project name can only contain letters (a-z, A-Z), digits (0-9), underscore (_) and/or dash (-). Invalid project name `%s`", paste(value, collapse = ""))
          )
        }
        return()
      }
    ),
    format_standard = S7::new_property(
      class = S7::class_character,
      setter = function(self, value) {
        value <- match.arg(value, choices = c("native", "bids"))
        S7::prop(self, "format_standard") <- value
        self
      }
    ),
    parent_path = S7::new_property(
      class = S7::new_union(S7::class_character, NULL, bidsr::BIDSProject),
      setter = function(self, value) {
        if(self@format_standard == "bids" && !S7::S7_inherits(value, bidsr::BIDSProject)) {
          value <- bidsr::bids_project(path = value)
        }
        S7::prop(self, "parent_path") <- value
        self
      },
      validator = function(value) {
        if(is.null(value)) { return() }
        if(S7::S7_inherits(value, bidsr::BIDSProject)) { return() }
        if(length(value) != 1 || is.na(value)) {
          return("RAVE project parent folder must be either a `bidsr::BIDSProject`, a `string` or `NULL`")
        }
      }
    ),
    path = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        parent_path <- self@parent_path
        switch (
          self@format_standard,
          "bids" = {
            if(S7::S7_inherits(parent_path, bidsr::BIDSProject)) {
              parent_path <- file_path(format(parent_path, storage = "derivative"), "rave", "data_dir")
            }
          },
          {
            if(is.null(parent_path)) {
              parent_path <- ravepipeline::raveio_getopt(key = "data_dir")
            }
          }
        )
        return(file_path(path_abs(path = parent_path), self@name))
      }
    )
  ),
  validator = function(self) {
    # For BIDS derivative, parent_path must be specified
    if(self@format_standard == "bids" && is.null(self@parent_path)) {
      return("RAVE project parent path must not be NULL for BIDS format")
    }
    if(S7::S7_inherits(self@parent_path, bidsr::BIDSProject) && self@format_standard == "native") {
      return("RAVE project parent path is specified as a BIDS derivative, but the format is `native`. Please specify either native format or BIDS, not both.")
    }
  }
)

S7::method(format.generic, RAVEProjectImpl) <- function(x, ...) {
  if(x@format_standard == "bids") {
    if(S7::S7_inherits(x@parent_path, bidsr::BIDSProject)) {
      re <- sprintf("%s@bids:%s", x@name, x@parent_path@name)
    } else {
      re <- sprintf("%s@bids", x@name)
    }
  } else {
    re <- x@name
  }
  re
}

# examples <- bidsr::download_bids_examples()
# project_path <- file.path(examples, "ieeg_epilepsy_ecog")
# if(!isFALSE(examples)) {
#
#
#   project <- BIDSProject(
#     path = project_path,
#     raw_data_relpath = ".",
#     derivative_data_relpath = "derivatives"
#   )
#
#   project
#
# }
# bidsr::bids_project()

#' Definition for 'RAVE' project class
#' @description See \code{\link{as_rave_project}} for creating 'RAVE' project
#' class
#' @export
RAVEProject <- R6::R6Class(
  classname = 'RAVEProject',
  class = TRUE,
  portable = TRUE,
  inherit = RAVESerializable,
  private = list(
    impl = "RAVEProjectImpl"
  ),
  public = list(

    #' @description Internal method
    #' @param ... internal arguments
    `@marshal` = function(...) {
      parent_path <- private$impl@parent_path
      if(length(parent_path)) {
        parent_path <- format(parent_path)
      }
      list(
        namespace = "ravecore",
        r6_generator = "RAVEProject",
        data = list(
          name = format(self),
          format_standard = private$impl@format_standard,
          parent_path = parent_path
        )
      )
    },

    #' @description Internal method
    #' @param object,... internal arguments
    `@unmarshal` = function(object, ...) {
      stopifnot(identical(object$namespace, "ravecore"))
      stopifnot(identical(object$r6_generator, "RAVEProject"))
      return(RAVEProject$new(
        project_name = object$data$name,
        strict = FALSE,
        parent_path = object$data$parent_path
      ))
    },

    #' @description override print method
    #' @param ... ignored
    print = function(...){
      cat('RAVE project <', self$name, '>\n', sep = '')
      cat('  Format standard:', self$format_standard, '\n')
      cat('  Directory:', self$path, '\n')
      cat('  Subjects :', paste(self$subjects(), collapse = ', '), '\n')
      nms <- names(self)
      nms <- nms[!nms %in% r6_reserved_fields]
      cat('Field/Method:', paste(nms, collapse = ', '), "\n")
    },

    #' @description override format method
    #' @param ... ignored
    format = function(...) {
      if(self$format_standard == "bids") {
        if(S7::S7_inherits(private$impl@parent_path, bidsr::BIDSProject)) {
          re <- sprintf("%s@bids:%s", self$name, private$impl@parent_path@name)
        } else {
          re <- sprintf("%s@bids", self$name)
        }
      } else {
        re <- self$name
      }
      re
    },

    #' @description constructor
    #' @param project_name character
    #' @param strict whether to check project path
    #' @param parent_path \code{NULL}, a path to the project parent folder
    #' for native projects, or the path to 'BIDS' root directory.
    initialize = function(project_name, strict = TRUE, parent_path = NULL){
      # project_name can be:
      # name, name@native, name@bids, name@bids:dataset_name
      format <- "native"
      if(grepl("@", project_name)) {
        project_name <- strsplit(project_name, "@", fixed = TRUE)[[1]]
        format <- project_name[[2]]
        project_name <- project_name[[1]]
      }
      if(length(parent_path) == 1 && is.character(parent_path)) {
        if(S7::S7_inherits(parent_path, bidsr::BIDSProject)) {
          format <- "bids"
        }
      }
      if(startsWith(tolower(format), "bids")) {

        if( length(parent_path) == 1 && !S7::S7_inherits(parent_path, bidsr::BIDSProject) ) {
          parent_path <- bidsr::bids_project(path = parent_path, strict = strict)
          strs <- strsplit(format, ":", fixed = TRUE)[[1]]
          if(length(strs) == 2 && !identical(tolower(parent_path@name), tolower(strs[[2]]))) {
            ravepipeline::logger(
              level = "warning",
              "Project `", project_name, "` indicates a BIDS dataset with name `",
              strs[[2]], "`. However, the `parent_path` (BIDS root directory) ",
              "is given and the directory name `", parent_path@name, "` does ",
              "not match with the indicated name. RAVE will use the BIDS dataset ",
              "name `", parent_path@name, "` and ignore the indicated name."
            )
          }
        } else {
          # name@bids:dataset_name
          format <- strsplit(format, ":", fixed = TRUE)[[1]]
          if(length(format) > 1) {
            bids_dataset <- format[[2]]
            # TODO: check if bids_dataset is legit
            if(!grepl("^[a-zA-Z0-9_-]+$", bids_dataset)) {
              stop("Invalid BIDS dataset name: `", bids_dataset, "`. The dataset name must only include letters and digits.")
            }
            parent_path <- bidsr::bids_project(
              file_path(ravepipeline::raveio_getopt("bids_data_dir"), format[[2]]),
              strict = strict)
          }
        }

        format <- "bids"
      } else {
        format <- "native"
      }
      private$impl <- RAVEProjectImpl(name = project_name, parent_path = parent_path, format_standard = format)

      if(strict && !dir.exists(private$impl@path)){
        ravepipeline::logger(sprintf("RAVE project does not exist:\n  %s", private$impl@path), level = "warning")
      }
    },

    #' @description get all imported subjects within project
    #' @returns character vector
    subjects = function(){
      re <- list.dirs(self$path, full.names = FALSE, recursive = FALSE)
      switch (
        self$format_standard,
        "bids" = {
          re <- re[grepl("^sub-", re, ignore.case = TRUE)]
          re <- gsub("^sub-", "", re, ignore.case = TRUE)
        },
        {
          re <- re[grepl('^[a-zA-Z]+', re)]
        }
      )
      return(re)
    },

    #' @description whether a specific subject exists in this project
    #' @param subject_code character, subject name
    #' @returns true or false whether subject is in the project
    has_subject = function(subject_code){
      parent_path <- self$path
      switch (
        self$format_standard,
        "bids" = {
          subject_code <- gsub("^sub-", "", subject_code, ignore.case = TRUE)
          subject_code <- sprintf("sub-%s", subject_code)
        }
      )
      dir_exists(file_path(parent_path, subject_code))
    },

    #' @description get group data path for 'rave' module
    #' @param module_id character, 'rave' module ID
    #' @param must_work whether the directory must exist; if not exists,
    #' should a new one be created?
    group_path = function(module_id, must_work = FALSE){
      if(!length(module_id)) {
        path <- file_path(self$path, "_project_data")
      } else {
        path <- file_path(self$path, "_project_data", module_id)
      }

      if(must_work){
        dir_create2(path, check = FALSE)
      }
      path_abs(path)
    },

    #' @description list saved pipelines
    #' @param pipeline_name name of the pipeline
    #' @param cache whether to use cached registry
    #' @param check whether to check if the pipelines exist as directories
    #' @param all whether to list all pipelines; default is false; pipelines
    #' with the same label but older time-stamps will be hidden
    #' @returns A data table of pipeline time-stamps and directories
    subject_pipelines = function(pipeline_name, cache = FALSE, check = TRUE, all = FALSE) {
      # pipeline_name <- "power_explorer"
      # self <- as_rave_project("demo")
      # check = FALSE
      subjects <- self$subjects()
      re <- lapply(subjects, function(subject_code) {
        subject <- RAVESubject$new(project_name = self$name, subject_code = subject_code, strict = FALSE)
        subject$list_pipelines(pipeline_name = pipeline_name, check = check, all = all)
      })
      data.table::rbindlist(re)
    }

  ),
  active = list(

    #' @field path project folder, absolute path
    path = function(){
      private$impl@path
    },

    #' @field name project name, character
    name = function(){
      private$impl@name
    },

    #' @field pipeline_path path to pipeline scripts under project's folder
    pipeline_path = function(){
      file_path(private$impl@path, "_project_pipeline")
    },

    #' @field format_standard storage format, can be either \code{'native'} or
    #' \code{'bids'}-compliant
    format_standard = function() {
      private$impl@format_standard
    },

    #' @field @impl the internal object
    `@impl` = function() {
      private$impl
    }
  )
)


#' Convert character to \code{\link{RAVEProject}} instance
#' @param x R object that can be converted to 'RAVE' project. When \code{x}
#' is a character, see 'Details' on the rules.
#' @param ... passed to other methods, typically includes \code{strict} on
#' whether to check existence of the project folder, and \code{parent_path},
#' specifying non-default project root
#' @param strict whether to check project path; if set to true and the project
#' path is missing, the program will raise warnings
#' @param parent_path parent path in which the project is non-default, can be
#' a path to the parent folder of the project, or a
#' \code{\link[bidsr]{bids_project}} object. When the subject is from 'BIDS',
#' the \code{parent_path} must be the root of 'BIDS' directory.
#' @returns A \code{\link{RAVEProject}} instance
#' @seealso \code{\link{RAVEProject}}
#'
#' @details
#'
#' A 'RAVE' project is an aggregation of subjects with the similar research
#' targets. For example, 'RAVE' comes with a demo subject set, and the project
#' 'demo' contains eight subjects undergoing same experiments. Project
#' \code{'YAEL'} contains subject whose electrodes are localized by
#' \code{'YAEL'} modules.
#' The project can be "arbitrary": this is different to a 'BIDS' "project",
#' often served as a data-set name or identifier. A 'BIDS' project may have
#' multiple 'RAVE' projects. For example, an audio-visual 'BIDS' data may have
#' a 'RAVE' project \code{'McGurk'} to study the \code{'McGurk'} effect and
#' another \code{'synchrony'} to study the audio-visual synchronization.
#'
#' A valid 'RAVE' project name must only contain letters and digits;
#' underscores and dashes may be acceptable but might subject to future change.
#' For example \code{'demo'} is a valid project name, but \code{'my demo'} is
#' invalid.
#'
#' RAVE supports storing the data in \code{'native'} or \code{'bids'}-compliant
#' formats. The native format is compatible with the 'RAVE' 1.0 and 2.0, and
#' requires no conversion to 'BIDS' format, while \code{'bids'} requires the
#' data to be stored and processed in 'BIDS'-complaint format, which is better
#' for data sharing and migration, but might be over-kill in some cases.
#'
#' If the project string contains \code{'@'}, the characters after the 'at'
#' sign will be interpreted as indication of the storage format. For instance
#' \code{'demo@native'} or \code{'demo@bids:ds0001'} are interpreted
#' differently. The previous one indicates that the project \code{'demo'} is
#' stored with native format, usually located at \code{'rave_data/data_dir'}
#' under the home directory (can be manually set to other locations). The
#' latter one means the 'RAVE' project \code{'demo'} is stored under 'BIDS'
#' folder with a 'BIDS' data-set name \code{'ds0001'}.
#'
#'
#' @examples
#'
#'
#' # ---- Native format (RAVE legacy) ------------------------
#' project <- as_rave_project("demo", strict = FALSE)
#'
#' format(project)
#'
#' project$path
#'
#' project$subjects()
#'
#' # Non-standard project locations (native format)
#' as_rave_project("demo", strict = FALSE,
#'                 parent_path = "~/Downloads")
#'
#'
#' # ---- BIDS format ----------------------------------------
#' project <- as_rave_project("demo@bids:ds001", strict = FALSE)
#'
#' format(project)
#'
#' project$path
#'
#' # BIDS format, given the parent folder; this example requires
#' # 'bidsr' sample data. Run `bidsr::download_bids_examples()` first.
#'
#' examples <- bidsr::download_bids_examples(test = TRUE)
#'
#' if(!isFALSE(examples)) {
#'
#'   project <- as_rave_project(
#'     "audiovisual@bids", strict = FALSE,
#'     parent_path = file.path(examples, "ieeg_epilepsy_ecog"))
#'
#'   # RAVE processed data is under BIDS dirivative folder
#'   project$path
#'
#'   # "audiovisual@bids:ieeg_epilepsy_ecog"
#'   format(project)
#' }
#'
#'
#'
#' @export
as_rave_project <- function(x, ...){
  # For compability
  if(missing(x)) {
    project <- list(...)$project
    if(length(project) == 1) {
      return(as_rave_project(x = project, ...))
    }
  }
  UseMethod(generic = "as_rave_project")
}

#' @export
as_rave_project.default <- function(x, ...) {
  stop("`as_rave_project` generic function not implemented for class ",
       paste(sQuote(class(x)), collapse = ", "))
}

#' @rdname as_rave_project
#' @export
as_rave_project.character <- function(x, strict = TRUE, parent_path = NULL, ...) {

  RAVEProject$new(project_name = x, strict = strict, parent_path = parent_path)

}

#' @export
as_rave_project.RAVEProject <- function(x, ...) {
  x
}

#' @export
`as_rave_project.ravecore::RAVEProjectImpl` <- function(x, ...) {

  RAVEProject$new(
    project_name = sprintf("%s@%s", x@name, x@format_standard),
    parent_path = x@parent_path,
    ...
  )

}


#' Get all possible projects in 'RAVE' default directory
#' @param refresh whether to refresh the cache; default is true
#' @returns characters of project names
#'
#' @examples
#'
#' get_projects()
#'
#'
#' @export
get_projects <- local({
  re <- NULL
  function(refresh = TRUE){
    if(refresh || !length(re)){
      native_dir <- ravepipeline::raveio_getopt('data_dir')
      bids_dir <- ravepipeline::raveio_getopt('bids_data_dir')

      if(file_exists(native_dir)) {
        native_projects <- list.dirs(native_dir, full.names = FALSE, recursive = FALSE)
        native_projects <- native_projects[grepl('^[a-zA-Z0-9]+', native_projects)]
      } else {
        native_projects <- NULL
      }

      if(file_exists(bids_dir)) {
        bids_datasets <- list.dirs(bids_dir, full.names = FALSE, recursive = FALSE)
        bids_rave_projects <- lapply(bids_datasets, function(dset) {
          rave_dir <- file_path(bids_dir, dset, "derivatives", "rave", "data_dir")
          if(!dir_exists(rave_dir)) { return() }
          rave_projects <- list.dirs(rave_dir, full.names = FALSE, recursive = FALSE)
          rave_projects <- rave_projects[grepl('^[a-zA-Z0-9]+', rave_projects)]
          sprintf("%s@bids:%s", rave_projects, dset)
        })
      } else {
        bids_rave_projects <- NULL
      }

      re <<- unname(c(unlist(native_projects), unlist(bids_rave_projects)))
    }
    re
  }
})
