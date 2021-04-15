#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#did_plot.R

#The aim of this script is to run the differences-in-differences analysis regressions


# Setup -------------------------------------------------------------------

#PACKAGES
installation_needed  <- F
loading_needed <- T
package_list <- c("haven", "tidyverse")
if(installation_needed){install.packages(package_list, repos='http://cran.us.r-project.org')}
if(loading_needed){lapply(package_list, require, character.only = T)}

#clear workspace
rm(list=ls()) 
#SET WD in console

#load data
load(file = "../Data/SOEP/gen/did_data.Rda")

# Supply response regression ----------------------------------------------------------

supply_reg_bbins <- lm(data = did_data, labour_force ~ age_bins + year_bins + treatment_x_1823 + treatment_x_2427 + treatment_x_28)
supply_reg_yearfe_agebin <- lm(data = did_data, labour_force ~ age_bins + year_fac + treatment_x_1823 + treatment_x_2427 + treatment_x_28)
supply_reg_yearagefe <- lm(data = did_data, labour_force ~ age_fac + year_fac + treatment_x_1823 + treatment_x_2427 + treatment_x_28)


# Parallel trends - Supply ------------------------------------------------

#generate interactions
did_data <- mutate(did_data, pt_interactions = "0control")
for (age_bin in c("18-23", "24-27", "28+")) {
  for (year in c(2011:max(did_data$syear))) {
    did_data <- mutate(did_data, pt_interactions = ifelse(age_bins == age_bin & syear >= year,paste(age_bin, "_x_", year),pt_interactions))
  }
}

did_data <- mutate(did_data, pt_interactions_fac = as.factor(pt_interactions))
pt_reg <- lm(data = did_data, labour_force ~ age_fac + year_fac + pt_interactions_fac)
