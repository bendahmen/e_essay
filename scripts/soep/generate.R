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
load(file = "../Data/SOEP/gen/sample_young_data.Rda")

#clean and generate values
csample_young_data <- sample_young_data %>%
  #recode missing values
  mutate(across(everything(), ~ifelse(. %in% c(-1:-8), NA, .))) %>%
  mutate(
    #generate distance from 18's birthday in months
    month_distance = ifelse(is.na(gebmonat) | is.na(pgmonth),(age-18)*12+5,(syear - gebjahr -18)*12 + (pgmonth - gebmonat - 1)),
    #generate hourly wages
    hrl_wage = pglabgro/(pgvebzeit*4.35),
  ) %>%
  filter(
    syear>1984,
    month_distance > -26
  ) %>%
  mutate(
    #create dummies for being in the workforce or registered as unemployed
    workforce = ifelse(pgemplst %in% c(1, 2, 4), 1, 0),
    unemployed_status = ifelse(pglfs == 6, 1, 0),
    #dummy for being adult (eligible for MW)
    adult = factor(ifelse(month_distance<0,"Minor","Adult")),
    adult_dummy = ifelse(month_distance<0, 0, 1),
    #interaction bewteen adult dummy and running variable month distance
    dist_x_adult = month_distance*adult_dummy
  )

#create breaks for binning month distance
month_distance_bin_break <- seq(min(csample_young_data$month_distance),max(csample_young_data$month_distance)+3,4) #+3 ensures two highest values are not cut off
#create breaks for binning survey years
syear_bin_break <- c(seq(1984,2015,5),2020)


#create bins and (binned) means
csample_young_data <- csample_young_data %>%
  mutate(
    month_distance_bins = cut(csample_young_data$month_distance, month_distance_bin_break, labels = c(as.character(-6:17)), include.lowest = T),
    year_group = cut(csample_young_data$syear, syear_bin_break, include.lowest = F, labels = c("1985-89","1990-94","1995-99","2000-04","2005-09","2010-14","2015+ Treatment"))
  ) %>%
  group_by(year_group, month_distance) %>%
  mutate(
    mean_workforce = mean(workforce, na.rm = T),
    mean_unemp = mean(unemployed_status, na.rm = T),
    mean_hrl_wage = mean(hrl_wage, na.rm = T)
  ) %>%
  ungroup() %>%
  group_by(year_group, month_distance_bins) %>%
  mutate(
    binned_mean_hrl_wage = mean(hrl_wage, na.rm = T),
    binned_obversations = n()
  ) %>%
  ungroup()

#generate collapsed sample by month_distance for graphs
sample_young_md_data <- csample_young_data %>%
  group_by(year_group, month_distance_bins) %>%
  #calculate monthly share of working and unemployed as well as mean hourly wage
  summarise(
    mean_hrl_wage = mean(hrl_wage, na.rm = T),
    share_working = sum(workforce, na.rm = T)/n(),
    share_unemp_status = sum(unemployed_status, na.rm = T)/n(),
    n_employed = sum(workforce),
    observations = n()
  ) %>%
  ungroup()

#save datasets
save(csample_young_data, file="../Data/SOEP/gen/csample_young_data.Rda")
save(sample_young_md_data, file="../Data/SOEP/gen/sample_young_md_data.Rda")
