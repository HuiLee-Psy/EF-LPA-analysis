root <- normalizePath(Sys.getenv("EF_LPA_PROJECT_ROOT", unset = "."), mustWork = TRUE)
results <- Sys.getenv("EF_LPA_OUTPUT_DIR", unset = file.path(root, "results"))

read_result <- function(name) read.csv(file.path(results, name), check.names = FALSE)
expect_close <- function(actual, expected, tolerance, label) {
  mismatch <- length(actual) != length(expected) ||
    any(!is.finite(actual)) ||
    any(abs(actual - expected) > tolerance)
  if (mismatch) {
    stop(label, " did not match the validated reference values.", call. = FALSE)
  }
}

assignment <- read_result("Profile_ID_Assignment.csv")
if (!identical(as.integer(table(assignment$Class)), c(101L, 344L, 44L))) {
  stop("Profile sizes did not match 101, 344, and 44.")
}

avepp <- read_result("LPA_AvePP.csv")
expect_close(avepp$Mean_PP, c(.829, .912, .9747), .002, "Average posterior probabilities")

descriptives <- read_result("descriptive_statistics.csv")
expected_descriptives <- data.frame(
  Variable = c(
    "Sustained attention", "Interference inhibition", "Response inhibition",
    "Working memory", "Cognitive flexibility", "Chinese score", "English score",
    "Math score", "Hyperactivity/inattention", "Emotional symptoms",
    "Conduct problems", "Peer problems", "Prosocial behavior", "Total difficulties"
  ),
  N = c(489, 489, 489, 489, 489, 483, 483, 483, 255, 255, 255, 255, 255, 255),
  Mean = c(3.054, 64.300, 486.110, 3.812, 2.123, 60.342, 67.501,
           56.535, 3.349, 2.380, 1.694, 2.443, 7.584, 9.867)
)
descriptive_rows <- match(expected_descriptives$Variable, descriptives$Variable)
if (anyNA(descriptive_rows)) stop("One or more manuscript descriptive variables are missing.")
if (!identical(
  as.integer(descriptives$N[descriptive_rows]),
  as.integer(expected_descriptives$N)
)) {
  stop("Descriptive-statistic sample sizes did not match the source data.")
}
expect_close(
  descriptives$Mean[descriptive_rows], expected_descriptives$Mean, .001,
  "Descriptive-statistic means"
)

fit <- read_result("LPA_fit_statistics_manuscript.csv")
if (!identical(as.integer(fit$K), 2:5)) stop("The reported model set must contain K = 2 through 5.")
expect_close(fit$BIC, c(6735.496, 6729.007, 6752.633, 6768.490), .001, "BIC values")
expect_close(fit$Entropy, c(.980656, .782630, .627488, .696319), .00001, "Entropy values")
expect_close(fit$BLRT_p, c(0, .040, .154, .125), .00001, "BLRT p values")

r_values <- read_result("correlation_r_matrix.csv")
rownames(r_values) <- r_values$Variable
r_values$Variable <- NULL
actual_correlations <- c(
  r_values["Sustained attention", "Working memory"],
  r_values["Interference inhibition", "English score"],
  r_values["Response inhibition", "Chinese score"],
  r_values["Sustained attention", "Hyperactivity/inattention"],
  r_values["Working memory", "Peer problems"],
  r_values["Sustained attention", "Total difficulties"]
)
expected_correlations <- c(.1425458, .1092568, .1219206, -.1387128, -.1350527, -.1633371)
expect_close(
  actual_correlations, expected_correlations, 1e-06,
  "Selected Pearson correlations"
)

ef <- read_result("EF_profile_ANOVA.csv")
expected_ef <- c(z_AXCPT = 526.68, z_WM = 297.91, z_WCST = 21.25, z_Flanker = 7.70, z_SST = 3.82)
expect_close(ef$F_value[match(names(expected_ef), ef$Variable)], expected_ef, .02, "EF ANOVAs")

academic <- read_result("Academic_profile_ANOVA.csv")
expected_academic <- c(Math = 26.32, Chin = 16.04, Eng = 16.23)
expect_close(
  academic$F_value[match(names(expected_academic), academic$Variable)],
  expected_academic, .02, "Academic ANOVAs"
)

sdq <- read_result("SDQ_profile_ANOVA.csv")
expected_sdq <- c(
  SDQ_Attention_Deficit_Hyperactivity = 4.89,
  SDQ_emotion = .87,
  SDQ_conduct = 1.71,
  SDQ_companion = 7.55,
  SDQ_prosocial = 2.01,
  SDQ_Total_difficulty_score = 5.88
)
expect_close(sdq$F_value[match(names(expected_sdq), sdq$Variable)], expected_sdq, .02, "SDQ ANOVAs")

sdq_posthoc <- read_result("SDQ_profile_posthoc.csv")
peer_high_average <- subset(
  sdq_posthoc,
  Variable == "SDQ_companion" & Comparison == "Profile 1 - Profile 2"
)
expect_close(abs(peer_high_average$t_value), 1.83210, .0001, "Peer-problem post hoc t value")
expect_close(peer_high_average$p_bonferroni, .204352, .000001, "Peer-problem post hoc p value")

omega <- read_result("SDQ_reliability_omega.csv")
expected_omega <- c(.782325, .749037, .742831, .766306, .863895, .887558)
expect_close(omega$McDonalds_omega, expected_omega, .00001, "SDQ McDonald's omega")

availability <- read_result("SDQ_availability_comparisons.csv")
expect_close(availability$p_value, c(.384606, .066714), .00001, "SDQ availability tests")

message("All reference-result checks passed.")
