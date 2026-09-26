source(file.path(Sys.getenv("EF_LPA_PROJECT_ROOT", unset = "."), "R", "00_utils.R"))

task <- read_input("new_task_performance_and_score-simple.csv")
sdq <- read_input("LPA_cluster_merged_selected_final.csv")

task_vars <- c(
  "subject_id", "AXCPT", "WM", "WCST", "Flanker_Raw", "SST_Raw",
  "Chin", "Eng", "Math"
)
sdq_vars <- c(
  "subject_id", "SDQ_Attention_Deficit_Hyperactivity", "SDQ_emotion",
  "SDQ_conduct", "SDQ_companion", "SDQ_prosocial",
  "SDQ_Total_difficulty_score"
)
assert_columns(task, task_vars, "Task data")
assert_columns(sdq, sdq_vars, "SDQ data")
assert_unique_ids(task, "Task data")
assert_unique_ids(sdq, "SDQ data")

task_selected <- data.frame(
  subject_id = as.character(task$subject_id),
  "Sustained attention" = to_numeric(task$AXCPT),
  "Working memory" = to_numeric(task$WM),
  "Cognitive flexibility" = to_numeric(task$WCST),
  "Interference inhibition" = -to_numeric(task$Flanker_Raw),
  "Response inhibition" = -to_numeric(task$SST_Raw),
  "Chinese score" = to_numeric(task$Chin),
  "English score" = to_numeric(task$Eng),
  "Math score" = to_numeric(task$Math),
  check.names = FALSE
)

# Table 1 reports the original task metrics. Flanker interference cost and
# SSRT are reversed only for analyses in which higher scores must mean better
# performance (correlations and LPA), not for the descriptive table.
task_descriptive <- data.frame(
  "Sustained attention" = to_numeric(task$AXCPT),
  "Interference inhibition" = to_numeric(task$Flanker_Raw),
  "Response inhibition" = to_numeric(task$SST_Raw),
  "Working memory" = to_numeric(task$WM),
  "Cognitive flexibility" = to_numeric(task$WCST),
  "Chinese score" = to_numeric(task$Chin),
  "English score" = to_numeric(task$Eng),
  "Math score" = to_numeric(task$Math),
  check.names = FALSE
)

sdq_selected <- data.frame(
  subject_id = as.character(sdq$subject_id),
  "Hyperactivity/inattention" = to_numeric(sdq$SDQ_Attention_Deficit_Hyperactivity),
  "Emotional symptoms" = to_numeric(sdq$SDQ_emotion),
  "Conduct problems" = to_numeric(sdq$SDQ_conduct),
  "Peer problems" = to_numeric(sdq$SDQ_companion),
  "Prosocial behavior" = to_numeric(sdq$SDQ_prosocial),
  "Total difficulties" = to_numeric(sdq$SDQ_Total_difficulty_score),
  check.names = FALSE
)

matched <- match(task_selected$subject_id, sdq_selected$subject_id)
analysis_data <- cbind(
  task_selected[-1],
  sdq_selected[matched, -1, drop = FALSE]
)

descriptive_data <- cbind(
  task_descriptive,
  sdq_selected[matched, -1, drop = FALSE]
)

descriptives <- do.call(rbind, lapply(names(descriptive_data), function(variable) {
  x <- descriptive_data[[variable]]
  data.frame(
    Variable = variable,
    N = sum(!is.na(x)),
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    check.names = FALSE
  )
}))
write_result(descriptives, "descriptive_statistics.csv")

variables <- names(analysis_data)
k <- length(variables)
r_matrix <- matrix(NA_real_, k, k, dimnames = list(variables, variables))
p_matrix <- matrix(NA_real_, k, k, dimnames = list(variables, variables))
n_matrix <- matrix(0L, k, k, dimnames = list(variables, variables))

for (i in seq_len(k)) {
  for (j in i:k) {
    complete <- complete.cases(analysis_data[[i]], analysis_data[[j]])
    n <- sum(complete)
    n_matrix[i, j] <- n_matrix[j, i] <- n
    if (i == j) {
      r_matrix[i, j] <- 1
    } else if (n >= 3 && sd(analysis_data[[i]][complete]) > 0 &&
               sd(analysis_data[[j]][complete]) > 0) {
      test <- cor.test(
        analysis_data[[i]][complete], analysis_data[[j]][complete],
        method = "pearson", alternative = "two.sided"
      )
      r_matrix[i, j] <- r_matrix[j, i] <- unname(test$estimate)
      p_matrix[i, j] <- p_matrix[j, i] <- test$p.value
    }
  }
}

matrix_to_frame <- function(x) {
  data.frame(Variable = rownames(x), x, check.names = FALSE, row.names = NULL)
}

pairs <- which(upper.tri(r_matrix), arr.ind = TRUE)
pairwise_results <- data.frame(
  Variable_1 = variables[pairs[, 1]],
  Variable_2 = variables[pairs[, 2]],
  r = r_matrix[pairs],
  p = p_matrix[pairs],
  n = n_matrix[pairs],
  stringsAsFactors = FALSE
)

write_result(matrix_to_frame(r_matrix), "correlation_r_matrix.csv")
write_result(matrix_to_frame(p_matrix), "correlation_p_values.csv")
write_result(matrix_to_frame(n_matrix), "correlation_n_values.csv")
write_result(pairwise_results, "correlation_pairwise_results.csv")

message("Task sample: ", nrow(task_selected))
message("Participants with SDQ data: ", sum(!is.na(matched)))
