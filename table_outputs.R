# Load necessary libraries
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

#######################################
######        Unit Root
#######################################
r_results_IPS <- tibble(
  Variable = c("CO2", "Y", "E_kwh", "Y2", "MVA", "HDI", "KOF"),
  Statistic_ttest = round(results_IPS_L1$test_statistic, digits = 3),
  p_value_ttest = round(results_IPS_L1$p_value, digits = 3)
)


r_results_LLC <- tibble(
  Variable = c("CO2", "Y", "E_kwh", "Y2", "MVA", "HDI", "KOF"),
  Statistic_regression = round(results_LLC_Hall$test_statistic, digits =3),
  p_value_regression = round(results_LLC_Hall$p_value, digits = 3)
)

stata_results_breitung <- tibble(
  Variable = c("CO2", "Y", "E_kwh", "Y2", "MVA", "HDI", "KOF"),
  Statistic_regression = c(3.552,  6.203,  3.859, 6.267,-0.555,  9.454,  4.026),
  p_value_regression = c(0.999, 1.000, 0.999, 1.000, 0.289, 1.000, 1.000 )
)

# Combine all results into a single dataframe
combined_results_UR <- full_join(r_results_IPS, stata_results_breitung, by = "Variable") %>%
  full_join(r_results_LLC, by = "Variable") 

# Create a nicely formatted table with multi-level headers
combined_results_UR %>%
  kable(format = "html", col.names = c(
    "Variable", 
    "Statistic", "p-value", 
    "Statistic", "p-value", 
    "Statistic", "p-value"
  ), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "IPS" = 2, "LLC" = 2, "Breitung" = 2))

#######################################
######        CS dependence
#######################################

r_results_peseran_cd <- tibble(
  Model = c("CO2 = Y + Y2 + E_kwh + KOF + MVA + HDI"),
  Statistic_regression = c(1.404),
  p_value_regression = c(0.1602)
)

r_results_peseran_cd %>%
  kable(format = "html", col.names = c(
    "Model", 
    "Statistic", "p-value"
  ), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Pesaran CD" = 2))

#######################################
######        Co-integration
#######################################

r_results_pedroni <- tibble(
  Variable = c(rownames(results_Pedroni[["STATISTIC"]])),
  Statistic_regression = round(results_Pedroni_NA_rownames[["STATISTIC"]][,1] , digits =3),
  p_value_regression = round(results_Pedroni_NA_rownames[["STATISTIC"]][,2], digits = 3)
)

r_results_pedroni %>%
  kable(format = "html", col.names = c(
    "Type", 
    "Empirical", "Standardized"
  ), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Pedroni" = 2))

#######################################
######        Correlation
#######################################
corr_results <- round(cor(df_log[,-c(1:4)]), digits = 3) %>% 
  mutate(var = rownames(corr_results))

corr_results <- tibble(
  Variable = c("CO2", "Y", "E_kwh", "Y2", "MVA", "HDI", "KOF"),
  Correlation = corr_results
)

corr_results %>%
  kable(format = "html", col.names = c(
    " ","CO2", "Y", "E_kwh", "Y2", "MVA", "HDI", "KOF"
  ), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c("Correlation Table" = 8))


#######################################
######        Regression
#######################################

paper_results <- tibble(
  Variable = c("Intercept", "Y",  "Y2", "E_kwh", "KOF", "MVA", "HDI"),
  FMOLS = c(-6.216,  2.095, -0.101, 1.828, 0.216, 0.072,  1.782),
  FMOLS_p = c("***","***","***","***","***","***","***"),
  DOLS = c(-8.478, 2.081, -0.100, 1.812, 0.212, 0.069, 2.216),
  DOLS_p = c("***","***","***","***","***","***","***")
)

reg_results <- tibble(
  Variable = c("Intercept", "Y",  "Y2", "E_kwh", "KOF", "MVA", "HDI"),
  FMOLS = round(FMOLS$theta, digits = 3),
  FMOLS_p = c("***","***","***","***","***","***","***"),
  DOLS = round(DOLS$theta, digits = 3),
  DOLS_p = c("","","","***","","",""),
  Breitung = round(pooled_model_KOF$coefficients, digits = 3),
  Breitung_p = c("***","***","***","***","***","***","***"),
)

combined_results_reg <- full_join(paper_results, reg_results, by = "Variable")

combined_results_reg %>%
  kable(format = "html", col.names = c(
    "Variable", 
    "FMOLS", "sig. lvl", "DOLS", "sig. lvl", 
    "FMOLS", "sig. lvl", "DOLS", "sig. lvl",
    "Breitung", "sig. lvl"
  ), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Paper" = 4, "Our findings" = 6))


reg_results %>%
  kable(format = "html", col.names = c(
    "Variable", 
    #"FMOLS", "sig. lvl", "DOLS", "sig. lvl", 
    "FMOLS", "sig. lvl", "DOLS", "sig. lvl",
    "Breitung", "sig. lvl"
  ), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Our findings" = 6))


