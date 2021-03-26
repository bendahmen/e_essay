#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#generate.R

#The aim of this script is to clean the data and generate additional variables needed in the different stages of analysis

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
load(file = "../Data/SOEP/gen/sample_16_20_data.Rda")

#clean and generate values
sample_16_20_data <- sample_16_20_data %>%
  #recode missing values
  mutate(across(everything(), ~ifelse(. %in% c(-1:-8), NA, .))) %>%
  mutate(
    #generate distance from 18's birthday in months
    month_distance = ifelse(is.na(gebmonat) | is.na(pgmonth),(age-18)*12+5,(syear - gebjahr -18)*12 + (pgmonth - gebmonat - 1)),
    #generate hourly wages
    hrl_wage = pglabgro/(pgvebzeit*4.35)
  )

#generate sample by month_distance for graphs
sample_16_20_md_data <- sample_16_20_data %>%
  mutate(
    #create dummies for being in the workforce or registered as unemployed
    workforce = ifelse(pgemplst %in% c(1, 2, 4), 1, 0),
    unemployed_status = ifelse(pglfs == 6, 1, 0)
  ) %>%
  group_by(month_distance) %>%
  #calculate monthly share of working and unemployed as well as mean hourly wage
  summarise(
    mean_hrl_wage = mean(hrl_wage),
    share_working = sum(workforce)/n(),
    share_unemp_status = sum(unemployed_status)/n()
  )
