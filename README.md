# Data Analytics Portfolio

Four coursework and capstone projects. This repo is mostly **R, Excel, PowerPoint and Stata**; it does not currently contain SQL, Python, or BI-dashboard (Tableau/Power BI) work. See [Gaps](#gaps).

| Project | Question | Tools | Run it |
|---|---|---|---|
| [Authorized Push Payment Fraud](Authorized%20Push%20Payment%20Fraud%20and%20Data%20Analytics/README.md) | Can tree, random forest and KNN models flag likely APP-fraud transactions? | R | Yes (R script, relative path) |
| [Retail Sales](Retail%20Sales%20Data%20Presentation/README.md) | What do a year of online-retail transactions show about revenue by month and country? | R, Excel | Yes (R script) |
| [NFL Win-Loss](Win-Loss%20Percentage%20in%20the%20NFL%20Understanding%20Variables%20Within%20the%20NFL/README.md) | Which team statistics are associated with win percentage? | R | Yes (R scripts) |
| [Renaissance Zones](Renaissance%20Zones%2C%20Economic%20Variables%2C%20and%20Property%20Crime%20in%20the%20Counties%20of%20Michigan/README.md) | Do Michigan tax-incentive zones relate to employment/income, and those to property crime? | Stata, Excel | Paper and data only; analysis code not included |

## Running the R projects

1. Install R (4.x) and the packages listed in each project README.
2. Set the working directory to the project folder (the scripts use relative paths).
3. `source()` the script. Outputs go to an `outputs/` folder, which is git-ignored.

No `renv` lockfile or CI exists yet, so package versions are unpinned.

## Data and licensing

The MIT license in this repo covers the **code**. Datasets are third-party or course-supplied and keep their own terms; each project README lists what is known about the source. Retrieval dates are not recorded for any dataset (not known from the repo history).

`Online Retail.xlsx` is ~24 MB and is committed directly; it should move to Releases or Git LFS.

## Gaps

- No SQL, Python, or Tableau/Power BI work is in this public repo.
- No data dictionaries beyond the per-project READMEs.
- No time-based validation for the fraud project (see its README).
- FedFlix analysis was proposed in PR #1 but closed unmerged and is not part of this portfolio.
