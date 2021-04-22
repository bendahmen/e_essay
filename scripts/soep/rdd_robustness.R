#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#rdd_robustness.R

#The aim of this script is to check the robustness of the RDD estimates

# SET UP ------------------------------------------------------------------

source("scripts/soep/head.R")

#SET WD in console

#load data
load(file = "../Data/SOEP/gen/rdd_data.Rda")

#filter data
treatment_sample <- filter(rdd_data, year_group == "2015+ Treatment")



#covariates
covariates <- c("migback", "pgbilzeit", "pgpsbil")


# Validity of the Threshold -----------------------------------------------

#McCrary Density Test
mcd_test <- DCdensity(treatment_sample$month_distance, bin=1)

#non-outcome discontinuities
#run parametric quadratic model
quadratic_nonoutcome_rdd <- lapply(X = list("degree"), FUN = function(outcome) {
  f <- reformulate(c("month_distance", "adult_dummy", "dist_x_adult", "month_distance_sq", "dist_sq_x_adult"), response = outcome)
  plm(f, data=treatment_sample, weights = phrf, model = "pooling")
})

linear_nonoutcome_rdd <- lapply(X = list("pgerwzeit"), FUN = function(outcome) {
  f <- reformulate(c("month_distance", "adult_dummy", "dist_x_adult"), response = outcome)
  plm(f, data=treatment_sample, weights = phrf, model = "pooling")
})
