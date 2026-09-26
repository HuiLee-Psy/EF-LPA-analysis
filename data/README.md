# Required data files

Participant-level data are not included because they contain confidential information. Place the following files in this directory before running the analyses:

- `new_task_performance_and_score-simple.csv`
- `LPA_cluster_merged_selected_final.csv`
- `SDQ_final.csv`
- `LPA_gender_age.csv`

The scripts validate required column names and unique participant identifiers before analysis. Missing academic scores may be represented as `NA` or `#N/A`; they are converted to numeric missing values only for analyses involving academic achievement. The LPA sample is determined exclusively from the five EF indicators.
