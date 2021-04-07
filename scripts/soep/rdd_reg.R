#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#rdd_reg.R

#The aim of this script is to obtain RDD estimates for the data

# SET UP ------------------------------------------------------------------

#PACKAGES
installation_needed  <- F
loading_needed <- T
package_list <- c("haven", "tidyverse", "rdrobust", "rdd", "plm", "stargazer")
if(installation_needed){install.packages(package_list, repos='http://cran.us.r-project.org')}
if(loading_needed){lapply(package_list, require, character.only = T)}

#clear workspace
rm(list=ls()) 
#SET WD in console

#load data
load(file = "../Data/SOEP/gen/rdd_data.Rda")

#filter data
treatment_sample <- filter(rdd_data, year_group == "2015+ Treatment")
attach(treatment_sample)

#covariates
covariates <- c("migback", "pgbilzeit", "pgpsbil")

# GLOBAL PARAMETRIC MODEL -------------------------------------------------

#F-test for optimal specification workforce
linear_spec <- lm(workforce ~ adult_dummy + dist_x_adult + month_distance, weights=phrf)
linear_spec_ind <- lm(workforce ~ adult_dummy + dist_x_adult + month_distance + as.factor(month_distance_bins), weights = phrf)
linear_spec_anova <- anova(linear_spec, linear_spec_ind)[2,c(5,6)]

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
    lm(f, data=trimmed_sample, weights = phrf)
  })

trimmed_sample <-treatment_sample %>%
  filter(month_distance > quantile(month_distance, 0.1) & month_distance < quantile(month_distance, 0.9))
attach(trimmed_sample)
linear_interaction_rdd_no10 <- lapply(X = list("workforce", "unemployed", "hrl_wage"), FUN = function(outcome) {
    f <- reformulate(c("month_distance", "adult_dummy", "dist_x_adult", "month_distance_sq", "dist_sq_x_adult"), response = outcome)
    lm(f, data=trimmed_sample, weights = phrf)
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

control_sample_1014 <- filter(rdd_data, year_group == "2010-14")
control_sample_0509 <- filter(rdd_data, year_group == "2005-09")

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



# OUTPUT ------------------------------------------------------------------

#parametric global
anova_matrix <- rbind(linear_spec_anova, quadratic_spec_anova)
stargazer(anova_matrix, title = "F-Test for R-squared", summary = F)

#global employment
stargazer(linear_interaction_rdd[[1]], linear_interaction_rdd_control_1014[[1]], linear_interaction_rdd_covariates[[1]], linear_interaction_rdd_covariates_control_1014[[1]], quadratic_interaction_rdd[[1]], quadratic_interaction_rdd_covariates[[1]], column.labels = c("linear interaction", "linear interaction control", "linear interaction covariates", "linear interaction covariates control", "quadratic interaction", "quadratic interaction covariates"))

stargazer(linear_interaction_rdd[[2]], linear_interaction_rdd_control_1014[[2]], linear_interaction_rdd_covariates[[2]], linear_interaction_rdd_covariates_control_1014[[2]], quadratic_interaction_rdd[[2]], quadratic_interaction_rdd_covariates[[2]], column.labels = c("linear interaction", "linear interaction control", "linear interaction covariates", "linear interaction covariates control", "quadratic interaction", "quadratic interaction covariates"))

stargazer(linear_interaction_rdd[[3]], linear_interaction_rdd_control_1014[[3]], linear_interaction_rdd_covariates[[3]], linear_interaction_rdd_covariates_control_1014[[3]], quadratic_interaction_rdd[[3]], quadratic_interaction_rdd_covariates[[3]], column.labels = c("linear interaction", "linear interaction control", "linear interaction covariates", "linear interaction covariates control", "quadratic interaction", "quadratic interaction covariates"))

stargazer(linear_interaction_rdd_nonp[[1]], linear_interaction_rdd_nonp[[2]], linear_interaction_rdd_nonp[[3]])

stargazer(linear_interaction_rdd_no5[[1]], linear_interaction_rdd_no10[[1]], linear_interaction_rdd_no5[[2]], linear_interaction_rdd_no10[[2]], linear_interaction_rdd_no5[[3]], linear_interaction_rdd_no10[[3]], column.labels = c("Employment excluding 10 extreme percentiles", "Employment excluding 20 extreme percentiles", "Unemployment excluding 10 extreme percentiles", "Unemployment excluding 20 extreme percentiles", "Hourly wage excluding 10 extreme percentiles", "Hourly wage excluding 20 extreme percentiles"))

