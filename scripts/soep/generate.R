#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#generate.R

#The aim of this script is to clean the data and generate additional variables needed in the different stages of analysis

#PACKAGES
installation_needed  <- F
loading_needed <- T
package_list <- c("haven", "tidyverse", "naniar")
if(installation_needed){install.packages(package_list, repos='http://cran.us.r-project.org')}
if(loading_needed){lapply(package_list, require, character.only = T)}

#clear workspace
rm(list=ls()) 
#SET WD in console

#load data
load(file = "../Data/SOEP/gen/sample_16_20_data.Rda")

#clean and generate values
sample_16_20_data <- sample_16_20_data %>%
  #recode missing values
  mutate(across(everything(), ~ifelse(. %in% c(-1:-8), NA, .))) %>%
  mutate(
    #generate distance from 18's birthday in months
    month_distance = ifelse(is.na(gebmonat) | is.na(pgmonth),(age-18)*12+6,(syear - gebjahr -18)*12 + (pgmonth - gebmonat)),
    #generate hourly wages
    hrl_wage = pglabgro/pgvebzeit
  )

