#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#did_reg.R

#The aim of this script is to run the differences-in-differences analysis regressions

# Setup -------------------------------------------------------------------

source("scripts/soep/head.R")

#SET WD in console

#load data
load(file = "../Data/SOEP/gen/did_data.Rda")

# Supply response regression ----------------------------------------------------------

supply_reg_bbins <- lm(data = did_data, labour_force ~ age_bins + year_bins + treatment_x_1823 + treatment_x_2427 + treatment_x_28)
supply_reg_yearfe_agebin <- lm(data = did_data, labour_force ~ age_bins + year_fac + treatment_x_1823 + treatment_x_2427 + treatment_x_28)
supply_reg_yearagefe <- lm(data = did_data, labour_force ~ age_fac + year_fac + treatment_x_1823 + treatment_x_2427 + treatment_x_28)

# Parallel trends - Supply ------------------------------------------------

#generate interactions
pt_data <- mutate(did_data, pt_interactions = "0control")
for (age_bin in c("18-23", "24-26", "27-29")) {
  for (year in c(2011:max(pt_data$syear))) {
    pt_data <- mutate(pt_data, pt_interactions = ifelse(age_bins == age_bin & syear >= year,paste(age_bin, "_x_", year),pt_interactions))
  }
}
pt_data <- mutate(pt_data, pt_interactions_fac = as.factor(pt_interactions))
#parallel trends specification
pt_supply_reg <- lm(data = pt_data, labour_force ~ age_bins + year_fac + pt_interactions_fac)

# Demand response regression ----------------------------------------------
#given no supply response in 24+ groups can compare their employment levels to 18-

demand_did_data <- did_data %>%
  filter(age_bins != "18-23")

demand_reg_bbins <- lm(data = demand_did_data, workforce ~ age_bins + year_bins + treatment_x_2427 + treatment_x_28)
demand_reg_yearfe_agebin <- lm(data = demand_did_data, workforce ~ age_bins + year_fac + treatment_x_2427 + treatment_x_28)
demand_reg_yearagefe <- lm(data = demand_did_data, workforce ~ age_fac + year_fac + treatment_x_2427 + treatment_x_28)

#parallel trends
pt_demand_reg <- lm(data = pt_data, labour_force ~ age_bins + year_fac + pt_interactions_fac)
