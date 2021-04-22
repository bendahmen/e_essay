#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#rdd_reg.R

#The aim of this script is to obtain RDD estimates for the data

# SET UP ------------------------------------------------------------------

source("scripts/soep/head.R")

#SET WD in console

#load data
load(file = "../Data/SOEP/gen/rdd_data.Rda")

#filter data
treatment_sample <- filter(rdd_data, year_group == "2015+ Treatment")
treatment_sample <- pdata.frame(treatment_sample, index = c("pid", "syear"))
attach(treatment_sample)

#covariates
outcomes <- c("workforce", "unemployed", "hrl_wage", "labour_force", "pglabgro")
covariate_s <- " + migback + pgbilzeit + pgpsbil"


# Regression Function -----------------------------------------------------

rdd_regression <- function(outcome, data, quadratic = F, covariates = F) {
  s_formula <- paste(outcome, " ~ month_distance + adult_dummy + dist_x_adult", sep ="")
  if (quadratic) {
    s_formula <- paste(s_formula, " + month_distance_sq + dist_sq_x_adult", sep = "")
  }
  if (covariates) {
    s_formula <- paste(s_formula, covariate_s, sep = "")
  }
  formula <- as.formula(s_formula)
  reg <- lm(data = data, formula = formula, weights = phrf)
  rob <- coeftest(reg, vcov=vcovHC(reg))
  output <- list(reg, rob)
}

# GLOBAL PARAMETRIC MODEL -------------------------------------------------

#F-test for optimal specification workforce
linear_spec <- lm(workforce ~ adult_dummy + dist_x_adult + month_distance, weights=phrf, data = treatment_sample)
linear_spec_ind <- lm(workforce ~ adult_dummy + dist_x_adult + month_distance + as.factor(month_distance_bins), weights = phrf, data = treatment_sample)
linear_spec_anova <- anova(linear_spec, linear_spec_ind)[2,c(5,6)]

quadratic_spec <- lm(workforce ~ adult_dummy + dist_x_adult + month_distance + month_distance_sq + dist_sq_x_adult, weights=phrf, data = treatment_sample)
quadratic_spec_ind <- lm(workforce ~ adult_dummy + dist_x_adult + month_distance + as.factor(month_distance_bins) + month_distance_sq + dist_sq_x_adult, weights = phrf, data = treatment_sample)
quadratic_spec_anova <- anova(quadratic_spec, quadratic_spec_ind)[2,c(5,6)]

#run parametric quadratic model
quadratic_interaction_rdd <- lapply(X = outcomes, FUN = rdd_regression, data = treatment_sample, quadratic = T)

quadratic_interaction_rdd_covariates <- lapply(X = outcomes, FUN = rdd_regression, data = treatment_sample, quadratic = T, covariates = T)

#run parametric linear model
linear_interaction_rdd <- lapply(X = outcomes, FUN = rdd_regression, data = treatment_sample)

linear_interaction_rdd_covariates <- lapply(X = outcomes, FUN = rdd_regression, data = treatment_sample, covariates = T)

#check for robustness with trimmed data
trimmed_sample <-treatment_sample %>%
  filter(month_distance > quantile(month_distance, 0.05) & month_distance < quantile(month_distance, 0.95))
linear_interaction_rdd_no5 <- lapply(X = outcomes, FUN = rdd_regression, data = trimmed_sample, quadratic = T)

trimmed_sample <-treatment_sample %>%
  filter(month_distance > quantile(month_distance, 0.1) & month_distance < quantile(month_distance, 0.9))
linear_interaction_rdd_no10 <- lapply(X = outcomes, FUN = rdd_regression, data = trimmed_sample, quadratic = T)

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

linear_interaction_rdd_nonp <- lapply(X = outcomes, FUN = rdd_regression, data = trimmed_sample)


# CONTROL GROUP ANALYSIS --------------------------------------------------

control_sample_1014 <- filter(rdd_data, year_group == "2010-14")
control_sample_0509 <- filter(rdd_data, year_group == "2005-09")

#run parametric linear model for control group 2010-2014
linear_interaction_rdd_control_1014 <- lapply(X = outcomes, FUN = rdd_regression, data = control_sample_1014)

linear_interaction_rdd_covariates_control_1014 <- lapply(X = outcomes, FUN = rdd_regression, data = control_sample_1014, covariates = T)

#run parametric linear model for control group 2005-2009
linear_interaction_rdd_control_0509 <- lapply(X = outcomes, FUN = rdd_regression, data = control_sample_0509)

linear_interaction_rdd_covariates_control_0509 <- lapply(X = outcomes, FUN = rdd_regression, data = control_sample_0509, covariates = T)



# OUTPUT ------------------------------------------------------------------

# #parametric global
# anova_matrix <- rbind(linear_spec_anova, quadratic_spec_anova)
# stargazer(anova_matrix, title = "F-Test for R-squared", summary = F)
# 
# #global employment
# stargazer(linear_interaction_rdd[[1]], linear_interaction_rdd_control_1014[[1]], linear_interaction_rdd_covariates[[1]], linear_interaction_rdd_covariates_control_1014[[1]], quadratic_interaction_rdd[[1]], quadratic_interaction_rdd_covariates[[1]], column.labels = c("linear interaction", "linear interaction control", "linear interaction covariates", "linear interaction covariates control", "quadratic interaction", "quadratic interaction covariates"))
# 
# stargazer(linear_interaction_rdd[[2]], linear_interaction_rdd_control_1014[[2]], linear_interaction_rdd_covariates[[2]], linear_interaction_rdd_covariates_control_1014[[2]], quadratic_interaction_rdd[[2]], quadratic_interaction_rdd_covariates[[2]], column.labels = c("linear interaction", "linear interaction control", "linear interaction covariates", "linear interaction covariates control", "quadratic interaction", "quadratic interaction covariates"))
# 
# stargazer(linear_interaction_rdd[[3]], linear_interaction_rdd_control_1014[[3]], linear_interaction_rdd_covariates[[3]], linear_interaction_rdd_covariates_control_1014[[3]], quadratic_interaction_rdd[[3]], quadratic_interaction_rdd_covariates[[3]], column.labels = c("linear interaction", "linear interaction control", "linear interaction covariates", "linear interaction covariates control", "quadratic interaction", "quadratic interaction covariates"))
# 
# stargazer(linear_interaction_rdd_nonp[[1]], linear_interaction_rdd_nonp[[2]], linear_interaction_rdd_nonp[[3]])
# 
# stargazer(linear_interaction_rdd_no5[[1]], linear_interaction_rdd_no10[[1]], linear_interaction_rdd_no5[[2]], linear_interaction_rdd_no10[[2]], linear_interaction_rdd_no5[[3]], linear_interaction_rdd_no10[[3]], column.labels = c("Employment excluding 10 extreme percentiles", "Employment excluding 20 extreme percentiles", "Unemployment excluding 10 extreme percentiles", "Unemployment excluding 20 extreme percentiles", "Hourly wage excluding 10 extreme percentiles", "Hourly wage excluding 20 extreme percentiles"))



