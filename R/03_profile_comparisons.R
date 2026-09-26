source(file.path(Sys.getenv("EF_LPA_PROJECT_ROOT", unset = "."), "R", "00_utils.R"))

task <- read_input("new_task_performance_and_score-simple.csv")
sdq <- read_input("LPA_cluster_merged_selected_final.csv")
assignment_path <- file.path(output_dir, "Profile_ID_Assignment.csv")
if (!file.exists(assignment_path)) {
  stop("Run R/02_latent_profile_analysis.R before profile comparisons.", call. = FALSE)
}
assignment <- read.csv(assignment_path, stringsAsFactors = FALSE, check.names = FALSE)

assert_unique_ids(task, "Task data")
assert_unique_ids(sdq, "SDQ data")
assert_unique_ids(assignment, "Profile assignment")

task_analysis <- data.frame(
  subject_id = as.character(task$subject_id),
  Math = to_numeric(task$Math),
  Chin = to_numeric(task$Chin),
  Eng = to_numeric(task$Eng),
  z_AXCPT = as.numeric(scale(to_numeric(task$AXCPT))),
  z_WM = as.numeric(scale(to_numeric(task$WM))),
  z_WCST = as.numeric(scale(to_numeric(task$WCST))),
  z_Flanker = as.numeric(scale(-to_numeric(task$Flanker_Raw))),
  z_SST = as.numeric(scale(-to_numeric(task$SST_Raw)))
)
task_analysis <- merge(
  task_analysis,
  assignment[c("subject_id", "Class")],
  by = "subject_id", all.x = TRUE, sort = FALSE
)
if (anyNA(task_analysis$Class)) stop("Some task participants lack profile assignments.")
task_analysis$Class <- factor(task_analysis$Class, levels = 1:3)

sdq_outcomes <- c(
  "SDQ_Attention_Deficit_Hyperactivity", "SDQ_emotion", "SDQ_conduct",
  "SDQ_companion", "SDQ_prosocial", "SDQ_Total_difficulty_score"
)
assert_columns(sdq, c("subject_id", sdq_outcomes), "SDQ data")
sdq_analysis <- sdq[c("subject_id", sdq_outcomes)]
sdq_analysis[sdq_outcomes] <- lapply(sdq_analysis[sdq_outcomes], to_numeric)
sdq_analysis <- merge(
  sdq_analysis,
  assignment[c("subject_id", "Class")],
  by = "subject_id", all.x = TRUE, sort = FALSE
)
if (anyNA(sdq_analysis$Class)) stop("Some SDQ participants lack profile assignments.")
sdq_analysis$Class <- factor(sdq_analysis$Class, levels = 1:3)

run_comparisons <- function(data, outcomes, labels, prefix) {
  omnibus_rows <- list()
  descriptive_rows <- list()
  posthoc_rows <- list()
  pairs <- combn(levels(data$Class), 2, simplify = FALSE)

  for (outcome in outcomes) {
    complete <- complete.cases(data[c("Class", outcome)])
    current <- data[complete, c("Class", outcome)]
    names(current)[2] <- "value"
    model <- aov(value ~ Class, data = current)
    table <- summary(model)[[1]]
    mse <- table[2, "Mean Sq"]
    residual_df <- table[2, "Df"]

    group_stats <- do.call(rbind, lapply(levels(current$Class), function(group) {
      x <- current$value[current$Class == group]
      data.frame(
        Variable = outcome, Label = unname(labels[outcome]), Class = as.integer(group),
        N = length(x), Mean = mean(x), SD = sd(x)
      )
    }))
    descriptive_rows[[outcome]] <- group_stats

    omnibus_p <- table[1, "Pr(>F)"]
    omnibus_rows[[outcome]] <- data.frame(
      Variable = outcome,
      Label = unname(labels[outcome]),
      F_value = table[1, "F value"],
      df1 = table[1, "Df"],
      df2 = residual_df,
      p_value = omnibus_p,
      eta2 = table[1, "Sum Sq"] / sum(table[, "Sum Sq"]),
      p_formatted = format_p(omnibus_p)
    )

    if (!is.na(omnibus_p) && omnibus_p < .05) {
      for (pair in pairs) {
        x1 <- current$value[current$Class == pair[1]]
        x2 <- current$value[current$Class == pair[2]]
        estimate <- mean(x1) - mean(x2)
        standard_error <- sqrt(mse * (1 / length(x1) + 1 / length(x2)))
        t_value <- estimate / standard_error
        p_raw <- 2 * pt(-abs(t_value), df = residual_df)
        p_bonferroni <- min(1, p_raw * length(pairs))
        posthoc_rows[[length(posthoc_rows) + 1]] <- data.frame(
          Variable = outcome,
          Label = unname(labels[outcome]),
          Comparison = paste0("Profile ", pair[1], " - Profile ", pair[2]),
          Estimate = estimate,
          SE = standard_error,
          t_value = t_value,
          df = residual_df,
          p_raw = p_raw,
          p_bonferroni = p_bonferroni,
          cohen_d = abs(estimate) / sqrt(mse),
          p_formatted = format_p(p_bonferroni)
        )
      }
    }
  }

  omnibus <- do.call(rbind, omnibus_rows)
  descriptives <- do.call(rbind, descriptive_rows)
  posthoc <- if (length(posthoc_rows)) do.call(rbind, posthoc_rows) else data.frame()
  write_result(omnibus, paste0(prefix, "_ANOVA.csv"))
  write_result(descriptives, paste0(prefix, "_descriptives.csv"))
  write_result(posthoc, paste0(prefix, "_posthoc.csv"))
  invisible(list(omnibus = omnibus, descriptives = descriptives, posthoc = posthoc))
}

task_outcomes <- c("z_AXCPT", "z_WM", "z_WCST", "z_Flanker", "z_SST")
task_labels <- c(
  z_AXCPT = "Sustained attention", z_WM = "Working memory",
  z_WCST = "Cognitive flexibility", z_Flanker = "Interference inhibition",
  z_SST = "Response inhibition"
)
run_comparisons(task_analysis, task_outcomes, task_labels, "EF_profile")

academic_outcomes <- c("Math", "Chin", "Eng")
academic_labels <- c(Math = "Mathematics", Chin = "Chinese", Eng = "English")
run_comparisons(task_analysis, academic_outcomes, academic_labels, "Academic_profile")

sdq_labels <- c(
  SDQ_Attention_Deficit_Hyperactivity = "Hyperactivity/inattention",
  SDQ_emotion = "Emotional symptoms",
  SDQ_conduct = "Conduct problems",
  SDQ_companion = "Peer problems",
  SDQ_prosocial = "Prosocial behavior",
  SDQ_Total_difficulty_score = "Total difficulties"
)
run_comparisons(sdq_analysis, sdq_outcomes, sdq_labels, "SDQ_profile")
