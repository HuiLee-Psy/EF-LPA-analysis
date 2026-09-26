# Executive Function Profiles in Chinese Early Adolescents

This repository contains the analysis code for the manuscript *Executive Function Profiles in Chinese Early Adolescents: Associations with Academic Achievement and Emotional and Behavioral Difficulties*.

The workflow reproduces the analyses reported in the manuscript:

1. descriptive statistics and two-tailed Pearson correlations;
2. latent profile models using five standardized EF indicators;
3. selection and characterization of the three-profile solution;
4. profile comparisons for EF indicators, academic achievement, and parent-reported SDQ scores;
5. SDQ McDonald's omega and comparisons of participants with and without parent-report data.

## Analysis decisions

- Sustained attention, working memory, cognitive flexibility, interference inhibition, and response inhibition are the LPA indicators.
- Flanker interference cost and SSRT are reversed so that higher scores indicate better performance.
- Table 1 descriptive statistics retain the original Flanker interference-cost and SSRT units; reversal is applied only to correlations and profile analyses.
- Model 1 in `tidyLPA` is used: variances are constrained equal across profiles and covariances are fixed to zero.
- Candidate solutions contain two to five profiles. A one-profile model is fitted internally only as the baseline for the two-profile bootstrap likelihood-ratio test.
- BLRT p values use 1,000 parametric bootstrap replications by default.
- The retained solution contains three profiles. Numeric labels are standardized as Profile 1 = High EF, Profile 2 = Average EF, and Profile 3 = Attention-vulnerable EF.
- Profile comparisons use modal class assignment, one-way ANOVA, Bonferroni-adjusted pairwise comparisons, eta squared, and Cohen's d based on the omnibus residual standard deviation.

Unreported exploratory analyses from the working code, including posterior-probability weighted regressions and an academic-*g* PCA, are intentionally excluded.

## Repository structure

- `R/01_descriptives_correlations.R`: descriptive statistics and correlations
- `R/02_latent_profile_analysis.R`: LPA, fit indices, BLRT, AvePP, and profile assignments
- `R/03_profile_comparisons.R`: EF, academic, and SDQ profile comparisons
- `R/04_sdq_reliability_sample_check.R`: SDQ omega and sample-availability checks
- `scripts/figure2_correlation_heatmap.py`: manuscript Figure 2
- `tests/validate_results.R`: checks key outputs against the reported analysis
- `run_all.R`: runs the complete workflow

## Data

Participant-level data are not included. See `data/README.md` for the four required filenames. By default, scripts read `data/` and write `results/`.

Alternative locations can be supplied without editing the scripts:

```bash
export EF_LPA_DATA_DIR=/path/to/private/data
export EF_LPA_OUTPUT_DIR=/path/to/results
```

## Software

The archived LPA run was conducted in R 4.5.2 with `tidyLPA` 1.1.0 and
`mclust` 6.1.2. The same versions are recommended for exact reproduction.
Required R packages are:

```r
install.packages(c("psych", "mix"))
install.packages(
  "https://cran.r-project.org/src/contrib/Archive/mclust/mclust_6.1.2.tar.gz",
  repos = NULL,
  type = "source"
)
install.packages(
  "https://cran.r-project.org/src/contrib/Archive/tidyLPA/tidyLPA_1.1.0.tar.gz",
  repos = NULL,
  type = "source"
)
```

Figure 2 additionally requires Python 3 and the packages listed in `requirements.txt`.

## Run the analyses

From the repository root:

```bash
Rscript run_all.R
Rscript tests/validate_results.R
python3 scripts/figure2_correlation_heatmap.py
```

The full analysis uses 1,000 BLRT bootstrap replications. For a non-reportable smoke test only, reduce this number with `EF_LPA_NBOOT`, for example:

```bash
EF_LPA_NBOOT=10 Rscript R/02_latent_profile_analysis.R
```

The complete run records R and package information in `results/sessionInfo.txt`.
