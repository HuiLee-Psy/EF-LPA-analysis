project_root <- normalizePath(
  Sys.getenv("EF_LPA_PROJECT_ROOT", unset = "."),
  mustWork = TRUE
)
Sys.setenv(EF_LPA_PROJECT_ROOT = project_root)

scripts <- c(
  "R/01_descriptives_correlations.R",
  "R/04_sdq_reliability_sample_check.R",
  "R/02_latent_profile_analysis.R",
  "R/03_profile_comparisons.R"
)

for (script in scripts) {
  message("\nRunning ", script)
  sys.source(file.path(project_root, script), envir = new.env(parent = globalenv()))
}

writeLines(capture.output(sessionInfo()), file.path(
  Sys.getenv("EF_LPA_OUTPUT_DIR", unset = file.path(project_root, "results")),
  "sessionInfo.txt"
))
message("\nAll analyses completed.")
