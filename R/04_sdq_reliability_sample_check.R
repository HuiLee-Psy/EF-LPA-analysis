source(file.path(Sys.getenv("EF_LPA_PROJECT_ROOT", unset = "."), "R", "00_utils.R"))
require_packages("psych")

sdq_raw <- read_input("SDQ_final.csv")
demographics <- read_input("LPA_gender_age.csv")
assert_columns(sdq_raw, c("subject_id", as.character(1:25)), "Raw SDQ data")
assert_columns(demographics, c("subject_id", "gender", "age"), "Demographic data")
assert_unique_ids(sdq_raw, "Raw SDQ data")
assert_unique_ids(demographics, "Demographic data")

items <- as.character(1:25)
item_data <- sdq_raw[items]
item_data[] <- lapply(item_data, to_numeric)
invalid <- !is.na(as.matrix(item_data)) & !(as.matrix(item_data) %in% 1:3)
if (any(invalid)) stop("SDQ items must be coded 1, 2, or 3 before recoding.")
item_data[] <- lapply(item_data, function(x) x - 1)
reverse_items <- c("7", "11", "14", "21", "25")
item_data[reverse_items] <- lapply(item_data[reverse_items], function(x) 2 - x)

scales <- list(
  "Emotional symptoms" = c("3", "8", "13", "16", "24"),
  "Conduct problems" = c("5", "7", "12", "18", "22"),
  "Hyperactivity/inattention" = c("2", "10", "15", "21", "25"),
  "Peer problems" = c("6", "11", "14", "19", "23"),
  "Prosocial behavior" = c("1", "4", "9", "17", "20")
)
scales[["Total difficulties"]] <- unname(unlist(scales[1:4]))

omega_results <- do.call(rbind, lapply(names(scales), function(scale_name) {
  current <- item_data[scales[[scale_name]]]
  current <- current[complete.cases(current), , drop = FALSE]
  omega <- suppressMessages(suppressWarnings(psych::omega(
    current, nfactors = 1, poly = TRUE, plot = FALSE
  )))
  data.frame(
    Scale = scale_name,
    N_items = ncol(current),
    N_complete = nrow(current),
    McDonalds_omega = omega$omega.tot
  )
}))
write_result(omega_results, "SDQ_reliability_omega.csv")

demographics$subject_id <- as.character(demographics$subject_id)
demographics$gender <- to_numeric(demographics$gender)
demographics$age <- to_numeric(demographics$age)
demographics$age[demographics$age < 0] <- NA_real_
demographics$SDQ_available <- demographics$subject_id %in% as.character(sdq_raw$subject_id)

sample_summary <- do.call(rbind, lapply(c(FALSE, TRUE), function(available) {
  current <- demographics[demographics$SDQ_available == available, ]
  data.frame(
    SDQ_available = available,
    N = nrow(current),
    Age_N = sum(!is.na(current$age)),
    Age_M = mean(current$age, na.rm = TRUE),
    Age_SD = sd(current$age, na.rm = TRUE),
    Female_N = sum(current$gender == 1, na.rm = TRUE),
    Female_percent = 100 * mean(current$gender == 1, na.rm = TRUE)
  )
}))
write_result(sample_summary, "sample_by_SDQ_availability.csv")

age_test <- t.test(age ~ SDQ_available, data = demographics)
sex_table <- table(demographics$gender, demographics$SDQ_available)
sex_test <- chisq.test(sex_table, correct = FALSE)
availability_tests <- data.frame(
  Test = c("Welch t test for age", "Pearson chi-square test for sex"),
  Statistic = c(unname(age_test$statistic), unname(sex_test$statistic)),
  df = c(unname(age_test$parameter), unname(sex_test$parameter)),
  p_value = c(age_test$p.value, sex_test$p.value)
)
write_result(availability_tests, "SDQ_availability_comparisons.csv")
