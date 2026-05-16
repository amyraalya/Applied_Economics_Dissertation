# ============================================================
# The Effect of Brexit on Digital Advertising Intensity:
# A Comparative Case Study between the UK and the EU
#
# Amyra Alya Abdullah | 11309743 | May 2026
# ============================================================


# ============================================================
# PACKAGES & SETUP
# ============================================================

install.packages(c("lfe", "patchwork", "boot", "flextable"))

library(tidyverse)
library(ggplot2)
library(broom)
library(lfe)
library(fixest)
library(car)
library(modelsummary)
library(dplyr)
library(gt)
library(boot)
library(patchwork)
library(flextable)
library(sandwich)
library(lmtest)

dir.create("output",         showWarnings = FALSE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)


# ============================================================
# DATA LOADING & PREPARATION
# ============================================================

master_data          <- read.csv("Master Dataset.csv")
master_data_combined <- read.csv("Master Dataset Final.csv")

# Primary dataset: UK vs EU
master_data <- master_data |>
  arrange(Region, Year) |>
  group_by(Region) |>
  mutate(
    gdp_growth      = (GDP_EUR - lag(GDP_EUR, 1)) / lag(GDP_EUR, 1) * 100,
    gdp_growth_lag1 = lag(gdp_growth, 1),
    inflation_lag1  = lag(Inflation, 1)
  ) |>
  ungroup() |>
  mutate(
    treated             = as.integer(Region == "UK"),
    post_2016           = as.integer(Year >= 2016),
    post_2020           = as.integer(Year >= 2020),
    did_2016            = treated * post_2016,
    did_2020            = treated * post_2020,
    rel_year_binned     = pmax(pmin(Year - 2016, 7), -7),
    Time                = Year - min(Year),
    Time2               = Time^2,
    uk_trend            = treated * Year,
    Period              = ifelse(Year < 2016, "Pre-2016", "Post-2016"),
    Ad_Spend_Per_Capita = (Ad_Spend_EUR * 1e9) / Population
  )

# Robustness dataset: UK, EU, Netherlands, France
master_data_combined <- master_data_combined |>
  mutate(
    treated             = as.integer(Region == "UK"),
    post_2016           = as.integer(Year >= 2016),
    post_2020           = as.integer(Year >= 2020),
    did_2016            = treated * post_2016,
    did_2020            = treated * post_2020,
    rel_year_binned     = pmax(pmin(Year - 2016, 7), -7),
    Time                = Year - min(Year),
    Time2               = Time^2,
    Ad_Spend_Per_Capita = (Ad_Spend_EUR * 1e9) / Population
  )

# Country-pair subsets
df_uk_nl <- master_data_combined |>
  filter(Region %in% c("UK", "Netherlands")) |>
  mutate(
    treated   = as.integer(Region == "UK"),
    post_2020 = as.integer(Year >= 2020),
    Time      = Year - min(Year),
    Time2     = Time^2
  )

df_uk_fr <- master_data_combined |>
  filter(Region %in% c("UK", "France")) |>
  mutate(
    treated   = as.integer(Region == "UK"),
    post_2020 = as.integer(Year >= 2020),
    Time      = Year - min(Year),
    Time2     = Time^2
  )


# ============================================================
# DESCRIPTIVE STATISTICS
# ============================================================

datasummary(
  Percent_Ad_Spend + GDP_EUR + Ad_Spend_Nominal ~
    Region * (Mean + SD + Median + Min + Max + N),
  data   = master_data,
  title  = "Summary Statistics by Region (2008-2024)",
  output = "output/summary_statistics.png"
)


# ============================================================
# RAW TRENDS
# ============================================================

master_data |>
  ggplot(aes(x = Year, y = Percent_Ad_Spend,
             colour = Region, group = Region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2020, linetype = "dashed", colour = "red") +
  annotate("text", x = 2020.2,
           y = max(master_data$Percent_Ad_Spend) * 0.85,
           label = "Formal Exit (2020)", hjust = 0, size = 3) +
  scale_colour_manual(values = c("EU" = "#C0392B", "UK" = "#2C3E7A")) +
  labs(
    title    = "Digital Advertising Intensity: UK vs EU (2008-2024)",
    subtitle = "Vertical line indicates UK formal exit from EU (2020)",
    y        = "Digital Advertising Spending (% of GDP)",
    x        = "Year",
    colour   = "Region"
  ) +
  theme_minimal()

ggsave("output/figures/raw_trends.pdf", width = 8, height = 5)

# Multi-region raw trends
region_colours <- c(
  "UK"          = "#2C3E7A",
  "EU"          = "#C0392B",
  "Netherlands" = "#E67E22",
  "France"      = "#1A7A4A"
)

master_data_combined |>
  ggplot(aes(x = Year, y = Percent_Ad_Spend,
             colour = Region, group = Region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2016, linetype = "dashed", colour = "grey40") +
  geom_vline(xintercept = 2020, linetype = "dotted", colour = "grey40") +
  annotate("text", x = 2020.2,
           y = max(master_data_combined$Percent_Ad_Spend, na.rm = TRUE) * 0.9,
           label = "Formal Exit (2020)", hjust = 0, size = 3) +
  scale_colour_manual(values = region_colours) +
  labs(
    title  = "Digital Advertising Intensity: UK, EU, Netherlands & France (2008-2024)",
    y      = "Digital Ad Spend as % of GDP",
    x      = "Year",
    colour = "Region"
  ) +
  theme_minimal()

ggsave("output/figures/four_region_intensity.pdf", width = 9, height = 5)


# ============================================================
# PARALLEL TRENDS VALIDATION
# ============================================================

# Visual check: pre-treatment linear trends
master_data |>
  filter(Year < 2016) |>
  ggplot(aes(x = Year, y = Percent_Ad_Spend,
             colour = Region, group = Region)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("EU" = "#C0392B", "UK" = "#2C3E7A")) +
  labs(
    title  = "Pre-Treatment Linear Trends (2008-2015)",
    y      = "Digital Ad Spend as % of GDP",
    x      = NULL,
    colour = NULL
  ) +
  theme_minimal()

ggsave("output/figures/pre_treatment_trends.pdf", width = 8, height = 5)

# Event study (2016 reference year)
es_2016 <- feols(
  Percent_Ad_Spend ~ i(rel_year_binned, treated, ref = -1) | Region + Year,
  data = master_data
)

pdf("output/figures/event_study_2016.pdf", width = 8, height = 5)
iplot(es_2016,
      main = "Event Study: Parallel Trends Test",
      xlab = "Years Relative to 2016 Referendum",
      ylab = "Coefficient Estimate")
dev.off()

# Joint significance test on pre-treatment coefficients
pre_treat_coefs <- names(coef(es_2016))[
  str_detect(names(coef(es_2016)), "::-[1-9]")
]
linearHypothesis(es_2016, pre_treat_coefs)

# Pre-treatment linearity diagnostic: linear vs quadratic trend
df_pre <- master_data |> filter(Year < 2016)

m_lin_pre  <- lm(
  Percent_Ad_Spend ~ Time + treated + Time:treated,
  data = df_pre
)
m_quad_pre <- lm(
  Percent_Ad_Spend ~ Time + I(Time^2) + treated + Time:treated + I(Time^2):treated,
  data = df_pre
)

coeftest(m_quad_pre, vcov = vcovHC(m_quad_pre, type = "HC3"))

bic_lin  <- BIC(m_lin_pre)
bic_quad <- BIC(m_quad_pre)
cat("BIC linear:   ", round(bic_lin,  3),
    "\nBIC quadratic:", round(bic_quad, 3), "\n")


# ============================================================
# MAIN DIFFERENCE-IN-DIFFERENCES MODELS
# ============================================================

# Sequential model build (2020 treatment)
m1_2020 <- lm(
  Percent_Ad_Spend ~ Region + post_2020 + Region:post_2020,
  data = master_data
)
m2_2020 <- lm(
  Percent_Ad_Spend ~ Time + Region + post_2020 + Region:post_2020,
  data = master_data
)
m3_2020 <- lm(
  Percent_Ad_Spend ~ Time + Region + post_2020 + Region:Time + Region:post_2020,
  data = master_data
)
m4_2020 <- lm(
  Percent_Ad_Spend ~ Time + Region + post_2020 +
    Region:Time + Region:post_2020 + Time:post_2020,
  data = master_data
)
m5_2020 <- lm(
  Percent_Ad_Spend ~ Time + Region + post_2020 +
    Region:Time + Region:post_2020 + Time:post_2020 + Region:Time:post_2020,
  data = master_data
)

modelsummary(
  list(
    "Step 1"        = m1_2020,
    "Step 2"        = m2_2020,
    "Step 3"        = m3_2020,
    "Step 4"        = m4_2020,
    "Step 5 (Full)" = m5_2020
  ),
  coef_map = c("RegionUK:post_2020" = "DiD Estimator (beta5)"),
  stars    = TRUE,
  title    = "Sequential Model Build: 2020 Treatment"
)

# Preferred specification: augmented DiD with and without controls
m_augmented_2020 <- lm(
  Percent_Ad_Spend ~ Time + Region + post_2020 +
    Region:Time + Region:post_2020 + Time:post_2020,
  data = master_data
)
summary(m_augmented_2020)

m_2020_controls <- lm(
  Percent_Ad_Spend ~ Time + Region + post_2020 +
    Region:Time + Region:post_2020 + Time:post_2020 +
    gdp_growth_lag1 + inflation_lag1,
  data = master_data
)
summary(m_2020_controls)


# ============================================================
# TWFE COMPARISON
# ============================================================

m_2020_twfe <- feols(
  Percent_Ad_Spend ~ did_2020 | Region + Year,
  data = master_data
)

m_2020_twfe_controls <- feols(
  Percent_Ad_Spend ~ did_2020 + gdp_growth_lag1 + inflation_lag1 | Region + Year,
  data = master_data
)

modelsummary(
  list(
    "TWFE (Baseline)"     = m_2020_twfe,
    "TWFE (Controls)"     = m_2020_twfe_controls,
    "Aug. DiD (Baseline)" = m_augmented_2020,
    "Aug. DiD (Controls)" = m_2020_controls
  ),
  coef_map = c(
    "did_2020"           = "Brexit × Post",
    "RegionUK:post_2020" = "Brexit × Post",
    "gdp_growth_lag1"    = "GDP Growth (Lagged)",
    "inflation_lag1"     = "Inflation (Lagged)"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  stars   = TRUE,
  title   = "TWFE vs Augmented DiD: 2020 Treatment"
)


# ============================================================
# MACROECONOMIC CONTROL VARIABLE TRENDS
# ============================================================

p_gdp <- master_data |>
  ggplot(aes(x = Year, y = gdp_growth, colour = Region)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 2016, linetype = "dashed") +
  annotate("rect", xmin = 2016, xmax = 2024,
           ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.3) +
  labs(title = "GDP Growth Rate", y = "%", x = NULL) +
  scale_colour_manual(values = c("EU" = "#C0392B", "UK" = "#2C3E7A")) +
  theme_minimal()

p_inf <- master_data |>
  ggplot(aes(x = Year, y = inflation_lag1, colour = Region)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 2016, linetype = "dashed") +
  annotate("rect", xmin = 2016, xmax = 2024,
           ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.3) +
  labs(title = "Inflation Rate (Lagged)", y = "%", x = NULL) +
  scale_colour_manual(values = c("EU" = "#C0392B", "UK" = "#2C3E7A")) +
  theme_minimal()

ggsave("output/figures/gdp_growth_series.pdf",  plot = p_gdp, width = 8, height = 4.5)
ggsave("output/figures/inflation_series.pdf",   plot = p_inf, width = 8, height = 4.5)

p_gdp + p_inf + plot_layout(guides = "collect")


# ============================================================
# ROBUSTNESS CHECKS
# ============================================================

# Alternative control units
m_2020_uk_eu <- lm(
  Percent_Ad_Spend ~ Time + Region + post_2020 +
    Region:Time + Region:post_2020 + Time:post_2020,
  data = master_data |> filter(Region %in% c("UK", "EU"))
)

m_2020_uk_nl <- lm(
  Percent_Ad_Spend ~ Time + Region + post_2020 +
    Region:Time + Region:post_2020 + Time:post_2020,
  data = df_uk_nl
)

m_2020_uk_fr <- lm(
  Percent_Ad_Spend ~ Time + Region + post_2020 +
    Region:Time + Region:post_2020 + Time:post_2020,
  data = df_uk_fr
)

modelsummary(
  list(
    "UK vs EU"          = m_2020_uk_eu,
    "UK vs Netherlands" = m_2020_uk_nl,
    "UK vs France"      = m_2020_uk_fr
  ),
  coef_map = c("RegionUK:post_2020" = "DiD Estimator (beta5)"),
  stars    = TRUE,
  title    = "Augmented DiD: UK vs Individual Control Units (2020 Treatment)"
)

# Three-unit panel
m_augmented_robust <- lm(
  Percent_Ad_Spend ~ Time + Region + post_2020 +
    Region:Time + Region:post_2020 + Time:post_2020,
  data = master_data_combined
)
summary(m_augmented_robust)

# Per capita outcome variable
m_per_capita_2020 <- lm(
  Ad_Spend_Per_Capita ~ Time + Region + post_2020 +
    Region:Time + Region:post_2020 + Time:post_2020,
  data = master_data
)
summary(m_per_capita_2020)

modelsummary(
  list(
    "Log Intensity (Primary)" = m_augmented_2020,
    "Per Capita"              = m_per_capita_2020
  ),
  coef_map = c("RegionUK:post_2020" = "DiD Estimator (beta5)"),
  stars    = TRUE,
  title    = "Per Capita Robustness (2020 Treatment)"
)

# Placebo test: false treatment date (2013)
master_data_placebo <- master_data |>
  filter(Year <= 2015) |>
  mutate(
    post_2013   = as.integer(Year >= 2013),
    did_placebo = treated * post_2013
  )

m_placebo_date <- lm(
  Percent_Ad_Spend ~ Time + Region + post_2013 +
    Region:Time + Region:post_2013 + Time:post_2013,
  data = master_data_placebo
)
summary(m_placebo_date)


# ============================================================
# BOOTSTRAP INFERENCE
# ============================================================

boot_did <- function(data, indices) {
  d   <- data[indices, ]
  fit <- lm(
    Percent_Ad_Spend ~ Time + Region + post_2020 +
      Region:Time + Region:post_2020 + Time:post_2020,
    data = d
  )
  return(coef(fit)["RegionUK:post_2020"])
}

set.seed(123)
boot_uk_eu <- boot(
  data      = master_data |> filter(Region %in% c("UK", "EU")),
  statistic = boot_did,
  R         = 1000
)

set.seed(123)
boot_uk_nl <- boot(
  data      = df_uk_nl,
  statistic = boot_did,
  R         = 1000
)

boot.ci(boot_uk_eu, type = "perc")
boot.ci(boot_uk_nl, type = "perc")

# Bootstrap p-values (two-sided, H0: coefficient <= 0)
boot_coefs_eu <- boot_uk_eu$t[!is.na(boot_uk_eu$t[, 1]), 1]
boot_coefs_nl <- boot_uk_nl$t[!is.na(boot_uk_nl$t[, 1]), 1]

p_value_eu <- 2 * mean(boot_coefs_eu <= 0)
p_value_nl <- 2 * mean(boot_coefs_nl <= 0)

data.frame(
  Comparison        = c("UK vs EU", "UK vs Netherlands"),
  Coefficient       = c(0.146, 0.164),
  Classical_SE      = c(0.039, 0.052),
  Bootstrap_SE      = c(round(sd(boot_coefs_eu), 3), round(sd(boot_coefs_nl), 3)),
  CI_Lower          = c(0.0546, 0.0261),
  CI_Upper          = c(0.2641, 0.3092),
  Bootstrap_P_Value = c(round(p_value_eu, 4), round(p_value_nl, 4)),
  Observations      = c(34, 30)
) |>
  gt() |>
  tab_header(
    title    = "Bootstrap Inference Results",
    subtitle = "Augmented DiD — 2020 Treatment Year"
  ) |>
  cols_label(
    Comparison        = "Comparison",
    Coefficient       = "DiD Coefficient",
    Classical_SE      = "Classical SE",
    Bootstrap_SE      = "Bootstrap SE",
    CI_Lower          = "95% CI Lower",
    CI_Upper          = "95% CI Upper",
    Bootstrap_P_Value = "Bootstrap P-Value",
    Observations      = "Observations"
  ) |>
  tab_spanner(label = "Standard Errors",  columns = c(Classical_SE, Bootstrap_SE)) |>
  tab_spanner(label = "95% Bootstrap CI", columns = c(CI_Lower, CI_Upper)) |>
  fmt_number(
    columns  = c(Coefficient, Classical_SE, Bootstrap_SE,
                 CI_Lower, CI_Upper, Bootstrap_P_Value),
    decimals = 4
  ) |>
  tab_source_note(
    "Note: Bootstrap based on 1,000 replications. CI computed using percentile method.
     P-value computed as twice the proportion of bootstrap estimates falling below zero."
  ) |>
  gtsave("output/bootstrap_results.png")


# ============================================================
# ROBUSTNESS SUMMARY
# ============================================================

robustness_summary <- tibble(
  Specification = c(
    "Primary (UK vs EU)",
    "Three-Unit Panel",
    "UK vs Netherlands",
    "UK vs France",
    "Bootstrap (UK vs EU)",
    "Bootstrap (UK vs Netherlands)",
    "Per Capita (EUR per person)"
  ),
  Coefficient  = c(0.146, 0.146, 0.164, 0.153, 0.146, 0.164, 70.699),
  SE           = c("(0.039)", "(0.036)", "(0.052)", "(0.060)",
                   "(0.056)", "(0.077)", "(0.023)"),
  Significance = c("***", "***", "**", "*", "**", "*", "**"),
  Observations = c(34, 58, 30, 28, 34, 30, 34)
) |>
  mutate(Estimate = paste0(round(Coefficient, 3), Significance, " ", SE)) |>
  select(Specification, Estimate, Observations)

flextable(robustness_summary) |>
  set_header_labels(
    Specification = "Specification",
    Estimate      = "DiD Coefficient",
    Observations  = "Observations"
  ) |>
  autofit()

cat("\nAll outputs written to output/\n")