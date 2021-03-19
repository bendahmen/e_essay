#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#merge.R

#The aim of this script is to assemble the data set needed for preliminary analysis

#PACKAGES
installation_needed  <- FALSE
loading_needed <- TRUE
package_list <- c('haven', "tidyr", "dplyr")
if(installation_needed){install.packages(package_list, repos='http://cran.us.r-project.org')}
if(loading_needed){lapply(package_list, require, character.only = TRUE)}

rm(list=ls()) #clear workspace
#SET WD

#import individual tracking data (long)
p_tracking_data <- read_dta("../Data/SOEP/cs-transfer/soep.v35.international.stata_dta/ppathl.dta")
#keep relevant vars from individual tracking data
p_tracking_data_vars <- c("pid", "syear", "sex", "gebjahr", "todjahr",
                          "germborn", "corigin", "gebmonat", "migback", "phrf", "piyear")
p_tracking_data <- p_tracking_data %>%
  select(p_tracking_data_vars)

