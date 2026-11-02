# Win-Loss Percentage in the NFL

**Question:** Which team statistics are associated with NFL win percentage (2003-2023)?

**Answer, with caveats:** See the paper PDF. Two cautions apply when reading any model here:
- `points_scored_per_game` and `points_opp_per_game` are season aggregates of the same games that determine win percentage, so they are near-tautological predictors. The `*_no_points` models drop them and are the ones to rely on; the full model is a comparison only.
- About 22 correlated predictors plus stepwise selection means individual coefficients are unstable.

## Data

`modified_nfl_team_stats_2003_2023.xlsx`: 672 team-seasons, 28 columns (`year`, `team`, `win_loss_perc`, per-game offensive/penalty stats, `score_pct`, `turnover_pct`). The variable names resemble Pro-Football-Reference team stats, but the source, URL, license and retrieval date were not recorded. Three column names in the workbook are misspelled (`points_opp_pe_rgame`, `turnsover_per_game`, `peanalties_per_game`); the scripts rename them on load.

## Scripts

- `nfl_team_preanalysis_script.R`: single- and few-variable regressions and scatterplots.
- `nfl_winning_percentage_analysis_script_MLR.R`: multiple regression and forward/backward stepwise selection, evaluated on a **year-based holdout** (train 2003-2018, test 2019-2023) using MAD, MSE and RMSE. An earlier version used a random split that mixed seasons of the same team across train and test, and printed the no-points equation with the full model's intercept; both are fixed.

Any results previously reported from the random split will differ from a re-run.

## Run

```r
# from this folder
install.packages(c("readxl", "caret"))
source("nfl_winning_percentage_analysis_script_MLR.R")
```
