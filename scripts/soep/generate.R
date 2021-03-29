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
    hrl_wage = pglabgro/(pgvebzeit*4.35),
  )

#create breaks for binning month distance
month_distance_bin_break <- seq(min(sample_16_20_data$month_distance),max(sample_16_20_data$month_distance)+1,5) #+1 ensures two highest values are not cut off
#create breaks for binning survey years
syear_bin_break <- c(seq(1984,2015,5),2020)


#create bins
sample_16_20_data <- mutate(
  sample_16_20_data,
  month_distance_bins = cut(sample_16_20_data$month_distance, month_distance_bin_break, include.lowest = T),
  year_group = cut(sample_16_20_data$syear, syear_bin_break, include.lowest = F, labels = c(as.character(1:6),"Treatment"))
)

#generate sample by month_distance for graphs
sample_16_20_md_data <- sample_16_20_data %>%
  filter(syear>1984) %>%
  mutate(
    #create dummies for being in the workforce or registered as unemployed
    workforce = ifelse(pgemplst %in% c(1, 2, 4), 1, 0),
    unemployed_status = ifelse(pglfs == 6, 1, 0)
  ) %>%
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

ggplot(sample_16_20_md_data) + geom_line(aes(x=month_distance_bins, y=share_unemp_status, group=1)) +
  geom_line(aes(x=month_distance_bins, y=share_working, group=1)) +
  facet_wrap(~year_group, ncol=3)
