project_root <- normalizePath(
  Sys.getenv("EF_LPA_PROJECT_ROOT", unset = "."),
  mustWork = TRUE
)

data_dir <- normalizePath(
  Sys.getenv("EF_LPA_DATA_DIR", unset = file.path(project_root, "data")),
  mustWork = TRUE
)

output_dir <- Sys.getenv(
  "EF_LPA_OUTPUT_DIR",
  unset = file.path(project_root, "results")
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, mustWork = TRUE)

require_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Missing R packages: ", paste(missing, collapse = ", "),
      ". Install them before running the analysis.",
      call. = FALSE
    )
  }
}

read_input <- function(filename) {
  path <- file.path(data_dir, filename)
  if (!file.exists(path)) stop("Input file not found: ", path, call. = FALSE)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

to_numeric <- function(x) suppressWarnings(as.numeric(as.character(x)))

assert_columns <- function(data, columns, label) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

assert_unique_ids <- function(data, label) {
  if (anyNA(data$subject_id) || anyDuplicated(data$subject_id)) {
    stop(label, " must contain nonmissing, unique subject_id values.", call. = FALSE)
  }
}

write_result <- function(data, filename) {
  path <- file.path(output_dir, filename)
  write.csv(data, path, row.names = FALSE, fileEncoding = "UTF-8")
  message("Saved: ", path)
  invisible(path)
}

format_p <- function(p) {
  ifelse(is.na(p), NA_character_, ifelse(p < .001, "< .001", sprintf("%.3f", p)))
}
