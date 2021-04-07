#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#rdd_robustness.R

#The aim of this script is to check the robustness of the RDD estimates

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


# Validity of the Threshold -----------------------------------------------

#McCrary Density Test
mcd_test <- DCdensity(treatment_sample$month_distance, bin=1)
