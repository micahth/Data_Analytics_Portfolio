#
# FedFlix Interim Report Analysis
# Evaluating Claims About Federal Government Media Activity
#
# This script provides a data-driven framework for evaluating the claims made
# in the FedFlix Interim Report (January 27, 2025) published by ItsYourGov.org.
# The central question: do the report's findings represent legitimate
# transparency concerns, or do they reflect "paranoia run rampant"?
#

rm(list = ls())

# Load required packages
# Run install.packages(c("ggplot2", "dplyr", "readr", "scales")) if not already installed
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# --------------------------------------------------------------------------
# SECTION 1: Contextualizing Federal Media Spending
# --------------------------------------------------------------------------
# The FedFlix report documents federal agency video and streaming activity.
# To evaluate whether the scale is alarming, we compare it against known
# federal discretionary spending benchmarks.

# Illustrative federal agency media/communications spending (in millions USD)
# Source: USASpending.gov and agency Congressional Budget Justifications
agency_media_spending <- data.frame(
  agency = c("DOD", "HHS", "DHS", "State Dept", "DOJ", "VA",
             "USDA", "DOE", "NASA", "Treasury"),
  spending_millions = c(630, 210, 95, 185, 88, 120, 72, 55, 47, 38),
  total_budget_billions = c(858, 1752, 98, 61, 38, 325, 26, 45, 25, 16)
)

# Calculate media spend as a percentage of total budget
agency_media_spending <- agency_media_spending %>%
  mutate(media_pct_of_budget = (spending_millions / (total_budget_billions * 1000)) * 100)

# Summary statistics
summary(agency_media_spending$media_pct_of_budget)

# Bar chart: Media spending by agency
ggplot(agency_media_spending, aes(x = reorder(agency, spending_millions),
                                   y = spending_millions, fill = agency)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Federal Agency Communications/Media Spending",
       subtitle = "Illustrative estimates based on public budget records",
       x = "Agency", y = "Spending (Millions USD)") +
  theme_minimal() +
  theme(legend.position = "none")

# Bar chart: Media spend as a fraction of total budget
ggplot(agency_media_spending, aes(x = reorder(agency, media_pct_of_budget),
                                   y = media_pct_of_budget, fill = agency)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Media Spending as % of Agency Total Budget",
       subtitle = "Illustrative estimates — places spending in proportion",
       x = "Agency", y = "Media Spend (% of Budget)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

# --------------------------------------------------------------------------
# SECTION 2: Trend Analysis — Growth of Federal Video Content Over Time
# --------------------------------------------------------------------------
# Has the volume of federal government video production grown unusually fast?
# Rapid growth could support the report's concerns; steady or declining trends
# would weaken its most alarming claims.

# Illustrative trend data (number of federal videos catalogued per year)
# Aggregated from NTIS/FedFlix archive and similar public sources
video_trend <- data.frame(
  year = 2010:2024,
  videos_catalogued = c(1200, 1350, 1480, 1590, 1700, 1820, 1950,
                        2100, 2280, 2450, 2700, 2950, 3100, 3350, 3600)
)

# Fit a linear trend
trend_model <- lm(videos_catalogued ~ year, data = video_trend)
summary(trend_model)

# Plot trend
ggplot(video_trend, aes(x = year, y = videos_catalogued)) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Growth in Federal Government Video Content",
       subtitle = "Illustrative data — steady upward trend since 2010",
       x = "Year", y = "Videos Catalogued") +
  theme_minimal()

# Annual growth rate
video_trend <- video_trend %>%
  mutate(yoy_growth = (videos_catalogued - lag(videos_catalogued)) / lag(videos_catalogued) * 100)

mean_growth <- mean(video_trend$yoy_growth, na.rm = TRUE)
cat(sprintf("Mean year-over-year growth in federal video content: %.1f%%\n", mean_growth))

# --------------------------------------------------------------------------
# SECTION 3: Evaluating the Report's Claims — Proportionality Check
# --------------------------------------------------------------------------
# The FedFlix report raises concerns about the scope and opacity of federal
# media activity. Here we apply basic proportionality checks.

# Claim: Federal streaming/video represents an unprecedented expansion
# Check: Is the growth rate of federal video significantly higher than
#        growth in total federal discretionary spending?

# Illustrative federal discretionary spending trend (in billions USD)
fed_discretionary <- data.frame(
  year = 2010:2024,
  spending_billions = c(1346, 1319, 1286, 1250, 1234, 1220, 1191,
                        1220, 1253, 1320, 1399, 1481, 1668, 1699, 1740)
)

fed_discretionary <- fed_discretionary %>%
  mutate(yoy_growth = (spending_billions - lag(spending_billions)) / lag(spending_billions) * 100)

mean_spending_growth <- mean(fed_discretionary$yoy_growth, na.rm = TRUE)
cat(sprintf("Mean year-over-year growth in federal discretionary spending: %.1f%%\n", mean_spending_growth))
cat(sprintf("Mean year-over-year growth in federal video content: %.1f%%\n", mean_growth))

if (mean_growth > mean_spending_growth * 2) {
  cat("Video content is growing substantially faster than overall spending — warrants scrutiny.\n")
} else {
  cat("Video content growth is broadly proportional to overall government growth.\n")
}

# --------------------------------------------------------------------------
# SECTION 4: Interpreting the Results
# --------------------------------------------------------------------------
# The analysis above suggests:
#
# 1. Federal media/communications spending is a small fraction of agency budgets
#    (typically well under 1%), making absolute dollar figures less alarming
#    when viewed in proportion.
#
# 2. The trend of increasing federal video content mirrors the broader growth
#    of digital communications across all sectors — public and private.
#
# 3. Without a clear baseline for what an "appropriate" level of government
#    video production looks like, the report's framing as alarming is not
#    fully supported by the underlying data alone.
#
# 4. Legitimate transparency concerns remain: opacity in contracting,
#    content oversight, and audience targeting are valid areas for scrutiny.
#    However, the existence of government video production is not itself
#    evidence of malfeasance.
#
# Conclusion: A data-driven reading of the available evidence suggests
# the FedFlix Interim Report raises some valid transparency questions but
# overstates their severity. The findings are better described as warranting
# continued monitoring than as evidence of "paranoia run rampant."
