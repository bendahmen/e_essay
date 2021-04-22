#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#FDZ_script.R

#The aim of this script is to assemble the data set needed for preliminary analysis
#WARNING: Scripts runs very long due to replacing missing values in line 62

# Setup -------------------------------------------------------------------

#PACKAGES
installation_needed  <- F
loading_needed <- T
package_list <- c("haven", "tidyverse", "matrixStats", "rdrobust", "rdd", "plm", "stargazer")
if(installation_needed){install.packages(package_list, repos='http://cran.us.r-project.org')}
if(loading_needed){lapply(package_list, require, character.only = T)}

#clear workspace
rm(list=ls()) 
#SET WD in console

# Data cleaning -----------------------------------------------------------

mz_data <- data.frame()
for (t in c(2014:2017)) {
  tmp_data <- read_dta(paste("../Data/Mikrozensus/DSF/DSF_MZ ", t, ".dta", sep = ""))
  tmp_data <- mutate(tmp_data, year = t)
  mz_data <- mz_data %>%
    bind_rows(tmp_data)
}
#keep relevant vars 
relevant_vars <- c("EF29", "EF38", "EF44", "EF131", "EF134",
                          "EF160", "EF309", "EF436", "EF438", "EF442", "EF2001", "EF2009", "year")
mz_data <- mz_data %>%
  select(all_of(relevant_vars)) %>%
  rename(
    age = EF44,
    weekly_hours = EF131,
    actual_weekly_hours = EF134
  ) %>%
  filter(age>14 & age< 30) %>%
  mutate(
    employed = ifelse(EF29==1,1,ifelse(is.na(EF29),NA,0)),
    unemployed = ifelse(EF38 %in% c(1:21),1,ifelse(is.na(EF29),NA,0)),
    labour_force = ifelse(employed == 1 | unemployed == 1,1,ifelse(is.na(EF29),NA,0)),
    not_lbr_force = ifelse(EF38 %in% c(22,25),1,ifelse(is.na(EF29),NA,0)),
    marginal_empl = ifelse(EF160 %in% c(1:3),1,ifelse(is.na(EF160),NA,0)),
    degree = ifelse(EF309 == 1,1,0),
    migrant_status = ifelse(EF2009 %in% c(2:6),1,ifelse(is.na(EF160),NA,0)),
    adult = factor(ifelse(age >= 18,"Adult", "Minor")),
    adult_dummy = ifelse(age >= 18,1,0),
    age_bins_large = cut(age, c(0,17,23,26,30), labels = c("<18", "18-23", "24-26", "27-29")),
    age_bins_small = cut(age, c(0,17,20,23,26,30), labels = c("<18", "18-20", "21-23", "24-26", "27-29")),
    year_fac = as.factor(year),
    age_fac = as.factor(age),
    age_sq = age*age,
    treatment_year = ifelse(year >= 2015,1,0),
    treatment_age = ifelse(age >= 18,1,0),
    treatment_interaction = ifelse(treatment_year == 1 & treatment_age == 1,1,0)
  ) %>%
  group_by(year, age_bins_large) %>%
  mutate(
    albin_mean_employed = mean(employed, na.rm = T),
    albin_mean_unemployed = mean(unemployed, na.rm = T),
    albin_mean_labour_force = mean(labour_force, na.rm = T),
    albin_mean_not_lbr_force = mean(not_lbr_force, na.rm = T),
    albin_mean_marginal_empl = mean(marginal_empl, na.rm = T),
    albin_mean_degree = mean(degree, na.rm = T),
    albin_observations = n(),
    treatment_x_1823 = ifelse(treatment_year == 1 & age_bins_large == "18-23",1,0),
    treatment_x_2426 = ifelse(treatment_year == 1 & age_bins_large == "24-26",1,0),
    treatment_x_2729 = ifelse(treatment_year == 1 & age_bins_large == "27-29",1,0),
    treatment_x_2729s = ifelse(treatment_year == 1 & age_bins_small == "27-29",1,0),
    treatment_x_2426s = ifelse(treatment_year == 1 & age_bins_small == "24-26",1,0),
    treatment_x_2123s = ifelse(treatment_year == 1 & age_bins_small == "21-23",1,0),
    treatment_x_1820s = ifelse(treatment_year == 1 & age_bins_small == "18-20",1,0)
  ) %>%
  ungroup() %>%
  group_by(year, age_bins_small) %>%
  mutate(
    asbin_mean_employed = mean(employed, na.rm = T),
    asbin_mean_unemployed = mean(unemployed, na.rm = T),
    asbin_mean_labour_force = mean(labour_force, na.rm = T),
    asbin_mean_not_lbr_force = mean(not_lbr_force, na.rm = T),
    asbin_mean_marginal_empl = mean(marginal_empl, na.rm = T),
    asbin_mean_degree = mean(degree, na.rm = T),
    asbin_observations = n()
  ) %>%
  ungroup() %>%
  group_by(age, year) %>%
  mutate(
    ybin_mean_employed = mean(employed, na.rm = T),
    ybin_mean_unemployed = mean(unemployed, na.rm = T),
    ybin_mean_labour_force = mean(labour_force, na.rm = T),
    ybin_mean_not_lbr_force = mean(not_lbr_force, na.rm = T),
    ybin_mean_marginal_empl = mean(marginal_empl, na.rm = T),
    ybin_mean_degree = mean(degree, na.rm = T),
    ybin_observations = n()
  ) %>%
  ungroup()

# Plots -------------------------------------------------------------------

varlist <- list("employed", "unemployed", "labour_force", "not_lbr_force", "marginal_empl")

age_bins_plots <- lapply(X = varlist, FUN = function(vars) {
  ggplot(data = mz_data, aes_(x=as.name("year"), y=as.name(paste("asbin_mean_",vars, sep = "")), group = as.name("age_bins_small"), color = as.name("age_bins_small"))) +
    geom_point() +
    geom_line()
})

age_binl_plots <- lapply(X = varlist, FUN = function(vars) {
  ggplot(data = mz_data, aes_(x=as.name("year"), y=as.name(paste("albin_mean_",vars, sep = "")), group = as.name("age_bins_large"), color = as.name("age_bins_large"))) +
    geom_point() +
    geom_line()
})

year_bin_plots <- lapply(X = varlist, FUN = function(vars) {
  ggplot(data = mz_data, aes_(x=as.name("age"), y=as.name(paste("ybin_mean_",vars, sep = "")), group = as.name("year"), color = as.name("year_fac"))) +
    geom_point() +
    geom_line()
})

# Estimations -------------------------------------------------------------


