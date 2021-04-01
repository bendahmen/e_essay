#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#rdd_plot.R

#The aim of this script is to obtain RDD estimates for the data

# SET UP ------------------------------------------------------------------

#PACKAGES
installation_needed  <- F
loading_needed <- T
package_list <- c("haven", "tidyverse", "rdrobust", "rdd", "plm")
if(installation_needed){install.packages(package_list, repos='http://cran.us.r-project.org')}
if(loading_needed){lapply(package_list, require, character.only = T)}

#clear workspace
rm(list=ls()) 
#SET WD in console

#load data
load(file = "../Data/SOEP/gen/csample_young_data.Rda")

#filter data
treatment_sample <- filter(csample_young_data, year_group == "2015+ Treatment")
attach(treatment_sample)

#covariates
covariates <- c("migback", "pgbilzeit", "pgpsbil")

# GLOBAL PARAMETRIC MODEL -------------------------------------------------

#F-test for optimal specification workforce
linear_spec <- lm(workforce ~ adult_dummy + dist_x_adult + month_distance, weights=phrf)
linear_spec_ind <- lm(workforce ~ adult_dummy + dist_x_adult + month_distance + as.factor(month_distance_bins), weights = phrf)
linear_spec_anova <- anova(linear_spec, linear_spec_in)[2,c(5,6)]

quadratic_spec <- lm(workforce ~ adult_dummy + dist_x_adult + month_distance + month_distance_sq + dist_sq_x_adult, weights=phrf)
quadratic_spec_ind <- lm(workforce ~ adult_dummy + dist_x_adult + month_distance + as.factor(month_distance_bins) + month_distance_sq + dist_sq_x_adult, weights = phrf)
quadratic_spec_anova <- anova(quadratic_spec, quadratic_spec_ind)[2,c(5,6)]

#run parametric quadratic model
quadratic_interaction_rdd <- lapply(X = list("workforce", "unemployed", "hrl_wage"), FUN = function(outcome) {
  f <- reformulate(c("month_distance", "adult_dummy", "dist_x_adult", "month_distance_sq", "dist_sq_x_adult"), response = outcome)
  lm(f, data=treatment_sample, weights = phrf)
})

quadratic_interaction_rdd_covariates <- lapply(X = list("workforce", "unemployed", "hrl_wage"), FUN = function(outcome) {
  f <- reformulate(c("month_distance", "adult_dummy", "dist_x_adult", "month_distance_sq", "dist_sq_x_adult", covariates), response = outcome)
  lm(f, data=treatment_sample, weights = phrf)
})

#run parametric linear model
linear_interaction_rdd <- lapply(X = list("workforce", "unemployed", "hrl_wage"), FUN = function(outcome) {
  f <- reformulate(c("month_distance", "adult_dummy", "dist_x_adult"), response = outcome)
  lm(f, data=treatment_sample, weights = phrf)
})
linear_interaction_rdd_covariates <- lapply(X = list("workforce", "unemployed", "hrl_wage"), FUN = function(outcome) {
  f <- reformulate(c("month_distance", "adult_dummy", "dist_x_adult", covariates), response = outcome)
  lm(f, data=treatment_sample, weights = phrf)
})

#check for robustness with trimmed data
trimmed_sample <-treatment_sample %>%
  filter(month_distance > quantile(month_distance, 0.05) & month_distance < quantile(month_distance, 0.95))
attach(trimmed_sample)
linear_interaction_rdd_no5 <- lapply(X = list("workforce", "unemployed", "hrl_wage"), FUN = function(outcome) {
    f <- reformulate(c("month_distance", "adult_dummy", "dist_x_adult", "month_distance_sq", "dist_sq_x_adult"), response = outcome)
    lm(f, data=treatment_sample, weights = phrf)
  })

trimmed_sample <-treatment_sample %>%
  filter(month_distance > quantile(month_distance, 0.1) & month_distance < quantile(month_distance, 0.9))
attach(trimmed_sample)
linear_interaction_rdd_no10 <- lapply(X = list("workforce", "unemployed", "hrl_wage"), FUN = function(outcome) {
    f <- reformulate(c("month_distance", "adult_dummy", "dist_x_adult", "month_distance_sq", "dist_sq_x_adult"), response = outcome)
    lm(f, data=treatment_sample, weights = phrf)
  })

attach(treatment_sample)

# LOCAL PARAMETRIC MODEL -------------------------------------------------

#optimal bandwidth
optimal_bw_workforce <- rdbwselect(workforce, month_distance, bwselect = "msetwo")
#truncate sample
trimmed_sample <- treatment_sample %>%
  filter(month_distance > -optimal_bw_workforce[[1]][1] | month_distance < optimal_bw_workforce[[1]][2])

#F-test for optimal specification workforce
linear_nonp_spec <- lm(workforce ~ adult_dummy + dist_x_adult + month_distance, data = trimmed_sample, weights=phrf)
linear_nonp_spec_ind <- lm(workforce ~ adult_dummy + dist_x_adult + month_distance + as.factor(month_distance_bins), data = trimmed_sample, weights = phrf)
linear_nonp_spec_anova <- anova(linear_nonp_spec, linear_nonp_spec_ind)[2,c(5,6)]

quadratic_nonp_spec <- lm(workforce ~ adult_dummy + dist_x_adult + month_distance + month_distance_sq + dist_sq_x_adult, data = trimmed_sample, weights=phrf)
quadratic_nonp_spec_ind <- lm(workforce ~ adult_dummy + dist_x_adult + month_distance + as.factor(month_distance_bins) + month_distance_sq + dist_sq_x_adult, data = trimmed_sample, weights = phrf)
quadratic_nonp_spec_anova <- anova(quadratic_nonp_spec, quadratic_nonp_spec_ind)[2,c(5,6)]

linear_interaction_rdd_nonp <- lapply(X = list("workforce", "unemployed", "hrl_wage"), FUN = function(outcome) {
  f <- reformulate(c("month_distance", "adult_dummy", "dist_x_adult"), response = outcome)
  lm(f, data=trimmed_sample, weights = phrf)
})


# CONTROL GROUP ANALYSIS --------------------------------------------------

control_sample_1014 <- filter(csample_young_data, year_group == "2010-14")
control_sample_0509 <- filter(csample_young_data, year_group == "2005-09")

#run parametric linear model for control group 2010-2014
linear_interaction_rdd_control_1014 <- lapply(X = list("workforce", "unemployed", "hrl_wage"), FUN = function(outcome) {
  f <- reformulate(c("month_distance", "adult_dummy", "dist_x_adult"), response = outcome)
  lm(f, data=control_sample_1014, weights = phrf)
})
linear_interaction_rdd_covariates_control_1014 <- lapply(X = list("workforce", "unemployed", "hrl_wage"), FUN = function(outcome) {
  f <- reformulate(c("month_distance", "adult_dummy", "dist_x_adult", covariates), response = outcome)
  lm(f, data=control_sample_1014, weights = phrf)
})

#run parametric linear model for control group 2005-2009
linear_interaction_rdd_control_0509 <- lapply(X = list("workforce", "unemployed", "hrl_wage"), FUN = function(outcome) {
  f <- reformulate(c("month_distance", "adult_dummy", "dist_x_adult"), response = outcome)
  lm(f, data=control_sample_0509, weights = phrf)
})
linear_interaction_rdd_covariates_control_0509 <- lapply(X = list("workforce", "unemployed", "hrl_wage"), FUN = function(outcome) {
  f <- reformulate(c("month_distance", "adult_dummy", "dist_x_adult", covariates), response = outcome)
  lm(f, data=control_sample_0509, weights = phrf)
})

