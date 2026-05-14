# ============================================================
# The Effect of Brexit on Digital Advertising Intensity:
# A Comparative Case Study between the UK and the EU

# Amyra Alya Abdullah
# 11309743
# May 2026
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


# ============================================================
# DATA LOADING & PREPARATION
# ============================================================

# Population is included directly in both CSVs (units: millions of persons)
master_data          <- read.csv("Master Dataset.csv")
master_data_combined <- read.csv("Master Dataset Final.csv")

# --- DiD variables: primary dataset (UK + EU) ---
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
    treated              = as.integer(Region == "UK"),
    post_2016            = as.integer(Year >= 2016),
    post_2020            = as.integer(Year >= 2020),
    did_2016             = treated * post_2016,
    did_2020             = treated * post_2020,
    rel_year_binned      = pmax(pmin(Year - 2016, 7), -7),
    rel_year_binned_2013 = pmax(pmin(Year - 2013, 2), -4),
    Time                 = Year - min(Year),
    Time2                = Time^2,
    uk_trend             = treated * Year,
    Period               = ifelse(Year < 2016, "Pre-2016", "Post-2016"),
    # Ad_Spend_EUR in billions EUR; Population in millions persons -> result in EUR per person
    Ad_Spend_Per_Capita  = (Ad_Spend_EUR * 1e9) / (Population * 1e6)
  )

# --- DiD variables: robustness dataset (UK + EU + Netherlands + France) ---
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
    # Ad_Spend_EUR in billions EUR; Population in millions persons -> result in EUR per person
    Ad_Spend_Per_Capita = (Ad_Spend_EUR * 1e9) / (Population * 1e6)
  )

# --- Country-pair subsets for robustness ---
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
# SUMMARY STATISTICS
# ============================================================

datasummary(
  Percent_Ad_Spend + GDP_EUR + Ad_Spend_Nominal ~
    Region * (Mean + SD + Median + Min + Max + N),
  data   = master_data,
  title  = "Summary Statistics by Region (2008-2024)",
  output = "summary_statistics.png"
)

# ============================================================
# RAW TRENDS PLOT 
# ============================================================

master_data |>
  ggplot(aes(x = Year, y = Percent_Ad_Spend, colour = Region, group = Region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2020, linetype = "dashed", colour = "red") +
  annotate("text", x = 2020.2, y = max(master_data$Percent_Ad_Spend) * 0.85,
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


# ============================================================
# PARALLEL TRENDS VALIDATION 
# ============================================================

# Pre-treatment linear trends (2008-2015)
master_data |>
  filter(Year < 2016) |>
  ggplot(aes(x = Year, y = Percent_Ad_Spend, colour = Region, group = Region)) +
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

# Event study (2016 reference year)
es_2016 <- feols(
  Percent_Ad_Spend ~ i(rel_year_binned, treated, ref = -1) | Region + Year,
  data = master_data
)

iplot(es_2016,
      main = "Event Study: Parallel Trends Test",
      xlab = "Years Relative to 2016 Referendum",
      ylab = "Coefficient Estimate")

# Joint significance test on pre-treatment coefficients
pre_treat_coefs <- names(coef(es_2016))[str_detect(names(coef(es_2016)), "::-[1-9]")]
linearHypothesis(es_2016, pre_treat_coefs)


# ============================================================
# MAIN DiD REGRESSIONS
# ============================================================

# Primary specification: augmented DiD with 2020 treatment
m_augmented_2020 <- lm(
  Percent_Ad_Spend ~ Time +
    Region +
    post_2020 +
    Region:Time +
    Region:post_2020 +    # β5: primary DiD estimator
    Time:post_2020 +
    Region:Time:post_2020,
  data = master_data
)
summary(m_augmented_2020)

# With macro controls
m_2020_controls <- lm(
  Percent_Ad_Spend ~ Time +
    Region +
    post_2020 +
    Region:Time +
    Region:post_2020 +
    Time:post_2020 +
    gdp_growth_lag1 +
    inflation_lag1,
  data = master_data
)
summary(m_2020_controls)


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
  coef_map = c("RegionUK:post_2020" = "DiD Estimator (β5)"),
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

# Bootstrap p-values
boot_coefs_eu <- boot_uk_eu$t[!is.na(boot_uk_eu$t[, 1]), 1]
boot_coefs_nl <- boot_uk_nl$t[!is.na(boot_uk_nl$t[, 1]), 1]

p_value_eu <- 2 * mean(boot_coefs_eu <= 0)
p_value_nl <- 2 * mean(boot_coefs_nl <= 0)

# Bootstrap results table
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
  gtsave("bootstrap_results.png")


# ============================================================
# PER CAPITA ROBUSTNESS
# ============================================================

# Ad_Spend_EUR in billions EUR
m_per_capita_2020 <- lm(
  Ad_Spend_Per_Capita ~ Time + Region + post_2020 +
    Region:Time + Region:post_2020 + Time:post_2020,
  data = master_data
)
summary(m_per_capita_2020)


# ============================================================
# PLACEBO TESTS
# ============================================================

# Placebo date test: fake treatment in 2013, restricted to pre-2016
master_data_placebo <- master_data |>
  filter(Year <= 2015) |>
  mutate(
    post_2013   = as.integer(Year >= 2013),
    did_placebo = treated * post_2013
  )

m_placebo_date <- feols(
  Percent_Ad_Spend ~ did_placebo | Region + Year,
  data = master_data_placebo
)
summary(m_placebo_date)


# Placebo unit test: Netherlands as fake treated unit, pre-2016
df_placebo_unit <- master_data_combined |>
  filter(Region %in% c("Netherlands", "EU"), Year <= 2015) |>
  mutate(
    treated_placebo = as.integer(Region == "Netherlands"),
    post_2013       = as.integer(Year >= 2013),
    did_placebo     = treated_placebo * post_2013
  )

m_placebo_unit <- feols(
  Percent_Ad_Spend ~ did_placebo | Region + Year,
  data = df_placebo_unit
)
summary(m_placebo_unit)


# ============================================================
# SUMMARY ROBUSTNESS
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
  Coefficient = c(0.146, 0.146, 0.164, 0.153, 0.146, 0.164, 70.699),
  SE = c("(0.039)", "(0.036)", "(0.052)", "(0.060)", "(0.056)", "(0.077)", "(0.023)"),
  Significance = c("***", "***", "**", "*", "**", "*", "**"),
  Observations = c(34, 58, 30, 28, 34, 30, 34)
) %>%
  mutate(Estimate = paste0(round(Coefficient, 3), Significance, " ", SE)) %>%
  select(Specification, Estimate, Observations)

flextable(robustness_summary) %>%
  set_header_labels(
    Specification = "Specification",
    Estimate = "DiD Coefficient",
    Observations = "Observations"
  ) %>%
  autofit()

# ============================================================
# GDP GROWTH & INFLATION CHARTS
# ============================================================

p_gdp <- master_data |>
  ggplot(aes(x = Year, y = gdp_growth, colour = Region)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 2016, linetype = "dashed") +
  labs(title = "GDP Growth Rate", y = "%", x = NULL) +
  scale_colour_manual(values = c("EU" = "#C0392B", "UK" = "#2C3E7A")) +
  theme_minimal()

p_inf <- master_data |>
  ggplot(aes(x = Year, y = inflation_lag1, colour = Region)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 2016, linetype = "dashed") +
  labs(title = "Inflation Rate (Lagged)", y = "%", x = NULL) +
  scale_colour_manual(values = c("EU" = "#C0392B", "UK" = "#2C3E7A")) +
  theme_minimal()

p_gdp + p_inf + plot_layout(guides = "collect")


# ============================================================
# TWFE COMPARISON
# ============================================================

m_2016_twfe <- feols(Percent_Ad_Spend ~ did_2016 | Region + Year, data = master_data)
m_2020_twfe <- feols(Percent_Ad_Spend ~ did_2020 | Region + Year, data = master_data)

etable(m_2016_twfe, m_2020_twfe)


# ============================================================
# SEQUENTIAL MODEL BUILD — 2020
# ============================================================

# 2020 treatment
m1_2020 <- lm(Percent_Ad_Spend ~ Region + post_2020 + Region:post_2020, data = master_data)
m2_2020 <- lm(Percent_Ad_Spend ~ Time + Region + post_2020 + Region:post_2020, data = master_data)
m3_2020 <- lm(Percent_Ad_Spend ~ Time + Region + post_2020 + Region:Time + Region:post_2020, data = master_data)
m4_2020 <- lm(Percent_Ad_Spend ~ Time + Region + post_2020 + Region:Time + Region:post_2020 + Time:post_2020, data = master_data)
m5_2020 <- lm(Percent_Ad_Spend ~ Time + Region + post_2020 + Region:Time + Region:post_2020 + Time:post_2020 + Region:Time:post_2020, data = master_data)

modelsummary(
  list("Step 1" = m1_2020, "Step 2" = m2_2020, "Step 3" = m3_2020, "Step 4" = m4_2020, "Step 5 (Full)" = m5_2020),
  coef_map = c("RegionUK:post_2020" = "DiD Estimator (β5)"),
  stars    = TRUE,
  title    = "Sequential Model Build: 2020 Treatment"
)


# ============================================================
# QUADRATIC TREND SPECIFICATIONS
# ============================================================

m_augmented_2 <- lm(
  Percent_Ad_Spend ~ Time + Time2 + Region + post_2016 +
    Region:Time + Region:Time2 + Region:post_2016 +
    Time:post_2016 + Region:Time:post_2016,
  data = master_data
)

m_augmented_3 <- lm(
  Percent_Ad_Spend ~ Time + Region + post_2016 +
    Region:Time + Region:Time2 +
    Region:post_2016 + Time:post_2016 + Region:Time:post_2016,
  data = master_data
)

modelsummary(
  list(
    "Linear (Primary)"    = m5,
    "Full Quadratic"      = m_augmented_2,
    "Selective Quadratic" = m_augmented_3
  ),
  stars = TRUE,
  title = "Augmented DiD Specifications: Linear vs Quadratic Trends"
)


# ============================================================
# THREE-UNIT PANEL RAW TRENDS
# ============================================================

region_colours <- c(
  "UK"          = "#2C3E7A",
  "EU"          = "#C0392B",
  "Netherlands" = "#E67E22",
  "France"      = "#1A7A4A"
)

master_data_combined |>
  ggplot(aes(x = Year, y = Percent_Ad_Spend, colour = Region, group = Region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2020, linetype = "dashed", colour = "red") +
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
