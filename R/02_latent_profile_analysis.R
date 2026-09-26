source(file.path(Sys.getenv("EF_LPA_PROJECT_ROOT", unset = "."), "R", "00_utils.R"))
require_packages(c("tidyLPA", "mclust"))

# tidyLPA 1.1.0 calls mclust functions from the attached package environment.
suppressPackageStartupMessages(library(mclust))

set.seed(1)
task <- read_input("new_task_performance_and_score-simple.csv")
required <- c(
  "subject_id", "AXCPT", "WM", "WCST", "Flanker_Raw", "SST_Raw",
  "Chin", "Eng", "Math"
)
assert_columns(task, required, "Task data")
assert_unique_ids(task, "Task data")

numeric_task <- data.frame(
  AXCPT = to_numeric(task$AXCPT),
  WM = to_numeric(task$WM),
  WCST = to_numeric(task$WCST),
  Flanker_Raw = to_numeric(task$Flanker_Raw),
  SST_Raw = to_numeric(task$SST_Raw)
)
if (anyNA(numeric_task)) {
  stop("All five EF indicators must be observed for the LPA sample.", call. = FALSE)
}

indicators <- data.frame(
  z_AXCPT = as.numeric(scale(numeric_task$AXCPT)),
  z_WM = as.numeric(scale(numeric_task$WM)),
  z_WCST = as.numeric(scale(numeric_task$WCST)),
  z_Flanker = as.numeric(scale(-numeric_task$Flanker_Raw)),
  z_SST = as.numeric(scale(-numeric_task$SST_Raw))
)

# K = 1 is fitted only as the baseline required for the K = 2 BLRT.
# The candidate profile solutions reported in the manuscript are K = 2 through 5.
lpa_fit <- tidyLPA::estimate_profiles(
  indicators,
  n_profiles = 1:5,
  models = 1
)
fit_raw <- tidyLPA::get_fit(lpa_fit)

first_column <- function(data, candidates) {
  hit <- candidates[candidates %in% names(data)][1]
  if (is.na(hit)) return(rep(NA_real_, nrow(data)))
  data[[hit]]
}

profile_sizes <- do.call(rbind, lapply(seq_along(lpa_fit), function(index) {
  fitted_data <- tidyLPA::get_data(lpa_fit[[index]])
  counts <- table(fitted_data$Class)
  data.frame(
    K = length(counts),
    Smallest_profile_n = min(counts),
    Smallest_profile_percent = 100 * min(counts) / sum(counts),
    Profile_sizes = paste(as.integer(counts), collapse = ", ")
  )
}))

fit_statistics <- data.frame(
  K = as.integer(first_column(fit_raw, c("Classes", "classes", "n_profiles", "Profile"))),
  LL = as.numeric(first_column(fit_raw, c("LogLik", "loglik", "LL"))),
  AIC = as.numeric(first_column(fit_raw, c("AIC", "aic"))),
  BIC = as.numeric(first_column(fit_raw, c("BIC", "bic"))),
  CAIC = as.numeric(first_column(fit_raw, c("CAIC", "caic"))),
  SABIC = as.numeric(first_column(
    fit_raw,
    c("SABIC", "aBIC", "ABIC", "A-BIC", "sample_size_adjusted_BIC")
  )),
  Entropy = as.numeric(first_column(fit_raw, c("Entropy", "entropy")))
)
fit_statistics <- merge(fit_statistics, profile_sizes, by = "K", sort = TRUE)

# Parametric bootstrap likelihood-ratio tests under Model 1 (mclust EEI).
n_boot <- as.integer(Sys.getenv("EF_LPA_NBOOT", unset = "1000"))
if (is.na(n_boot) || n_boot < 1) stop("EF_LPA_NBOOT must be a positive integer.")
matrix_data <- as.matrix(indicators)
blrt <- data.frame(K = 2:5, BLRT_value = NA_real_, BLRT_p = NA_real_)
set.seed(1)

for (k in 2:5) {
  null_fit <- mclust::Mclust(matrix_data, G = k - 1, modelNames = "EEI", verbose = FALSE)
  alternative_fit <- mclust::Mclust(matrix_data, G = k, modelNames = "EEI", verbose = FALSE)
  if (is.null(null_fit) || is.null(alternative_fit)) {
    stop("Model fitting failed for the ", k - 1, "- versus ", k, "-profile BLRT.")
  }

  observed_lrt <- 2 * (alternative_fit$loglik - null_fit$loglik)
  simulated_lrt <- rep(NA_real_, n_boot)
  for (b in seq_len(n_boot)) {
    simulated <- mclust::sim(
      modelName = "EEI",
      parameters = null_fit$parameters,
      n = nrow(matrix_data)
    )[, -1, drop = FALSE]
    simulated_null <- mclust::Mclust(
      simulated, G = k - 1, modelNames = "EEI", verbose = FALSE
    )
    simulated_alternative <- mclust::Mclust(
      simulated, G = k, modelNames = "EEI", verbose = FALSE
    )
    if (!is.null(simulated_null) && !is.null(simulated_alternative)) {
      simulated_lrt[b] <- 2 * (simulated_alternative$loglik - simulated_null$loglik)
    }
  }
  valid <- simulated_lrt[is.finite(simulated_lrt)]
  if (!length(valid)) stop("No valid bootstrap replications for K = ", k)
  blrt$BLRT_value[blrt$K == k] <- observed_lrt
  blrt$BLRT_p[blrt$K == k] <- mean(valid >= observed_lrt)
}

fit_statistics <- merge(fit_statistics, blrt, by = "K", all.x = TRUE, sort = TRUE)
write_result(subset(fit_statistics, K >= 2), "LPA_fit_statistics_manuscript.csv")

# Select the already-fitted three-profile solution rather than refitting it.
final_model <- lpa_fit[[3]]
classified <- tidyLPA::get_data(final_model)
if (nrow(classified) != nrow(task)) stop("LPA output row count does not match input data.")

for (variable in names(indicators)) {
  if (variable %in% names(classified) &&
      !isTRUE(all.equal(classified[[variable]], indicators[[variable]], tolerance = 1e-10))) {
    stop("LPA output order differs from the input order for ", variable, ".")
  }
}

probability_columns <- grep(
  "^(CPROB|Prob|probability_|posterior_)[._]?[0-9]+$",
  names(classified), value = TRUE, ignore.case = TRUE
)
probability_columns <- probability_columns[
  order(as.integer(gsub("[^0-9]", "", probability_columns)))
]
if (length(probability_columns) != 3) {
  stop(
    "Expected three class-specific posterior-probability columns; found: ",
    paste(probability_columns, collapse = ", "), call. = FALSE
  )
}

raw_class <- as.integer(as.character(classified$Class))
raw_levels <- sort(unique(raw_class))
if (!identical(raw_levels, 1:3)) stop("Unexpected raw class labels.")

raw_centroids <- aggregate(indicators, by = list(Raw_Class = raw_class), FUN = mean)
attention_raw <- raw_centroids$Raw_Class[which.min(raw_centroids$z_AXCPT)]
remaining <- setdiff(raw_levels, attention_raw)
remaining_rows <- match(remaining, raw_centroids$Raw_Class)
high_raw <- remaining[which.max(rowMeans(raw_centroids[remaining_rows, names(indicators)]))]
average_raw <- setdiff(remaining, high_raw)

raw_to_final <- setNames(rep(NA_integer_, 3), raw_levels)
raw_to_final[as.character(high_raw)] <- 1L
raw_to_final[as.character(average_raw)] <- 2L
raw_to_final[as.character(attention_raw)] <- 3L
final_class <- unname(raw_to_final[as.character(raw_class)])

probability_by_raw <- setNames(probability_columns, raw_levels)
posterior <- data.frame(
  Prob_1 = classified[[probability_by_raw[as.character(high_raw)]]],
  Prob_2 = classified[[probability_by_raw[as.character(average_raw)]]],
  Prob_3 = classified[[probability_by_raw[as.character(attention_raw)]]]
)

assignment <- data.frame(
  subject_id = as.character(task$subject_id),
  Class = final_class,
  posterior,
  max_probability = apply(posterior, 1, max),
  check.names = FALSE
)
write_result(assignment, "Profile_ID_Assignment.csv")

avepp <- do.call(rbind, lapply(1:3, function(k) {
  members <- assignment$Class == k
  data.frame(Profile = k, N = sum(members), Mean_PP = mean(assignment[[paste0("Prob_", k)]][members]))
}))
write_result(avepp, "LPA_AvePP.csv")

profile_plot <- do.call(rbind, lapply(1:3, function(k) {
  do.call(rbind, lapply(names(indicators), function(variable) {
    x <- indicators[[variable]][final_class == k]
    data.frame(Class = k, Variable = variable, N = length(x), Mean = mean(x), SE = sd(x) / sqrt(length(x)))
  }))
}))
write_result(profile_plot, "LPA_profile_plot_data.csv")

message("Selected profiles: ", paste(table(assignment$Class), collapse = ", "))
