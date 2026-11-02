# Renaissance Zones, Economic Variables, and Property Crime in Michigan Counties

**Question:** Are Michigan Renaissance Zone tax incentives linked to changes in county employment and income, and are employment and income linked to property crime?

**Answer:** The paper finds no link in either case (see the PDF). Linear regression, county-level panel data.

## Contents

- Paper (PDF) and slides (PPTX).
- `final_combined_data.xlsx` (sheet `Sheet3`, 2,170 county-year rows, 34 columns): population, demographics, per-capita income (nominal and 2016 dollars), employment, property and violent crime counts and ratios, and Renaissance Zone indicator flags.
- `HolmquistData.dta`: Stata dataset.

## Limitations

- **No analysis code is included**, although the work used Stata. The regressions cannot be reproduced from this repo; adding the `.do` file is outstanding.
- Data sources (census, crime, and zone-designation records) are not listed in the abstract or here, and there is no retrieval date or codebook. Variable names are truncated by Stata/Excel limits (e.g. `changeinratiooftotalemployedtopo`).
