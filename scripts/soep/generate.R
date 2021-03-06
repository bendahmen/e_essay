#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#generate.R

#The aim of this script is to clean the data and generate additional variables needed in the different stages of analysis

source("scripts/soep/head.R")

#SET WD in console

#load data
load(file = "../Data/SOEP/gen/sample_young_data.Rda")

#clean and generate values
set.seed(41063)
csample_young_data <- sample_young_data %>%
  #recode missing values
  mutate(across(everything(), ~ifelse(. %in% c(-1:-8), NA, .))) %>%
  mutate(
    #generate distance from 18's birthday in months
    month_distance = ifelse(is.na(gebmonat) | is.na(pgmonth), ifelse(age==18,NA, (age-18)*12+round(runif(n=n(),min=-6, max=5))),(syear - gebjahr -18)*12 + (pgmonth - gebmonat - 1)),
    month_distance_sq = month_distance*month_distance,
    #generate hourly wages
    hrl_wage = pglabgro/(pgvebzeit*4.35),
  ) %>%
  filter(
    syear>1984,
    month_distance > -25
  ) %>%
  mutate(
    #create dummies for binary outcomes
    workforce = ifelse(pgemplst %in% c(1, 2, 4), 1, ifelse(is.na(pgemplst),NA,0)),
    unemployed = ifelse(pglfs == 6, 1, ifelse(is.na(pglfs),NA,0)),
    labour_force = ifelse(pglfs %in% c(6:12), 1, ifelse(is.na(pglfs),NA,0)), #ifelse(is.na(pglfs),NA,0)
    #dummy for being adult (eligible for MW)
    adult = factor(ifelse(month_distance<0,"Minor","Adult")),
    adult_dummy = ifelse(month_distance<0, 0, 1),
    #interaction between adult dummy and running variable month distance
    dist_x_adult = month_distance*adult_dummy,
    dist_sq_x_adult = month_distance_sq*adult_dummy,
    #dummies for other vars
    degree = ifelse(pgpsbil == 1, 1, ifelse(is.na(pgpsbil), NA,0))
  )

#create breaks for binning month distance
month_distance_bin_break <- seq(min(csample_young_data$month_distance),max(csample_young_data$month_distance)+3,4) #+3 ensures two highest values are not cut off
#create breaks for binning survey years
syear_bin_break <- c(seq(1984,2015,5),2020)


#create bins and (binned) means for RDD
rdd_data <- csample_young_data %>%
  mutate(
    month_distance_bins = cut(csample_young_data$month_distance, month_distance_bin_break, labels = c(as.character(-6:35)), include.lowest = T),
    year_group = cut(csample_young_data$syear, syear_bin_break, include.lowest = F, labels = c("1985-89","1990-94","1995-99","2000-04","2005-09","2010-14","2015+ Treatment"))
  ) %>%
  group_by(year_group, month_distance) %>%
  mutate(
    mean_workforce = weighted.mean(workforce, w = phrf, na.rm = T),
    mean_unemployed = weighted.mean(unemployed, phrf, na.rm = T),
    mean_hrl_wage = weighted.mean(hrl_wage, phrf, na.rm = T),
    mean_pglabgro = weighted.mean(pglabgro, phrf, na.rm = T),
    mean_labour_force = weighted.mean(labour_force, w = phrf, na.rm = T),
    median_hrl_wage = weightedMedian(hrl_wage, w = phrf, na.rm = T),
    mean_degree = weighted.mean(degree, w = phrf, na.rm = T),
    mean_pgbetr = weighted.mean(pgbetr, w = phrf, na.rm = T),
    mean_pgerwzeit = weighted.mean(pgerwzeit, w =phrf, na.rm = T),
    mean_pgvebzeit = weighted.mean(pgvebzeit, w = phrf, na.rm = T),
    mean_pgtatzeit = weighted.mean(pgtatzeit, w = phrf, na.rm = T),
    observations = n()
  ) %>%
  ungroup() %>%
  group_by(year_group, month_distance_bins) %>%
  mutate(
    binned_mean_hrl_wage = weighted.mean(hrl_wage, phrf, na.rm = T),
    binned_median_hrl_wage = weightedMedian(hrl_wage, phrf, na.rm = T),
    binned_mean_pglabgro = weighted.mean(pglabgro, phrf, na.rm = T),
    binned_mean_workforce = weighted.mean(workforce, phrf, na.rm = T),
    binned_mean_labour_force = weighted.mean(labour_force, phrf, na.rm = T),
    binned_mean_unemployed = weighted.mean(unemployed, phrf, na.rm = T),
    binned_mean_pgvebzeit = weighted.mean(pgvebzeit, phrf, na.rm = T),
    binned_mean_pgtatzeit = weighted.mean(pgtatzeit, phrf, na.rm = T),
    binned_obversations = n()
  ) %>%
  ungroup() %>%
  filter(
    !year_group %in% c("1985-89", "1990-94", "1995-99", "2000-04"),
    month_distance > -22,
    age < 26
  )

#generate bins and (binned) means for DID
did_data <- csample_young_data %>%
  filter(
    syear > 2005 #2010
  ) %>%
  mutate(
    #basic dif-in-dif factors and interactions
    syear_num = as.numeric(syear),
    year_fac = as.factor(syear),
    treatment_group = ifelse(syear >= 2015, 1,0),
    age_fac = as.factor(age),
    age_sq = age*age,
    treatment_interaction = ifelse(age >= 18 & syear_num >= 2015, 1,0)
  ) %>%
  mutate(
    #create age and year bins
    age_bins = cut(age, c(0,17,23,26,30), labels = c("<18", "18-23", "24-26", "27-29")),
    year_bins = cut(syear, c(2000, 2012, 2014, 2016, 2019)) #labels = c("2000-04", "2005-09", "2010-14", "2015+ - Treatment")
  ) %>%
  #create interactions for group analysis
  mutate(
    treatment_x_1823 = ifelse(treatment_group == 1 & age_bins == "18-23",1,0),
    treatment_x_2427 = ifelse(treatment_group == 1 & age_bins == "24-26",1,0),
    treatment_x_28 = ifelse(treatment_group == 1 & age_bins == "27-29",1,0)
  ) %>%
  group_by(syear, age_bins) %>%
  mutate(
    agebin_mean_workforce = weighted.mean(workforce, w = phrf, na.rm = T),
    agebin_mean_unemployed = weighted.mean(unemployed, phrf, na.rm = T),
    agebin_mean_hrl_wage = weighted.mean(hrl_wage, phrf, na.rm = T),
    agebin_mean_labour_force = weighted.mean(labour_force, w = phrf, na.rm = T),
    agebin_median_hrl_wage = weightedMedian(hrl_wage, w = phrf, na.rm = T),
    agebin_mean_degree = weighted.mean(degree, w = phrf, na.rm = T),
    agebin_mean_pgbetr = weighted.mean(pgbetr, w = phrf, na.rm = T),
    agebin_mean_pgerwzeit = weighted.mean(pgerwzeit, w =phrf, na.rm = T),
    agebin_mean_pgvebzeit = weighted.mean(pgvebzeit, w = phrf, na.rm = T),
    agebin_mean_pgtatzeit = weighted.mean(pgtatzeit, w = phrf, na.rm = T),
    agebin_observations = n()
  ) %>%
  ungroup() %>%
  group_by(age, year_bins) %>%
  mutate(
    yearbin_mean_workforce = weighted.mean(workforce, w = phrf, na.rm = T),
    yearbin_mean_unemployed = weighted.mean(unemployed, phrf, na.rm = T),
    yearbin_mean_hrl_wage = weighted.mean(hrl_wage, phrf, na.rm = T),
    yearbin_mean_labour_force = weighted.mean(labour_force, w = phrf, na.rm = T),
    yearbin_median_hrl_wage = weightedMedian(hrl_wage, w = phrf, na.rm = T),
    yearbin_mean_degree = weighted.mean(degree, w = phrf, na.rm = T),
    yearbin_mean_pgbetr = weighted.mean(pgbetr, w = phrf, na.rm = T),
    yearbin_mean_pgerwzeit = weighted.mean(pgerwzeit, w =phrf, na.rm = T),
    yearbin_mean_pgvebzeit = weighted.mean(pgvebzeit, w = phrf, na.rm = T),
    yearbin_mean_pgtatzeit = weighted.mean(pgtatzeit, w = phrf, na.rm = T),
    yearbin_observations = n()
  ) %>%
  ungroup()

# #generate collapsed sample by month_distance for graphs
# sample_young_md_data <- csample_young_data %>%
#   group_by(year_group, month_distance) %>%
#   #calculate monthly share of working and unemployed as well as mean hourly wage
#   summarise(
#     mean_hrl_wage = mean(hrl_wage, na.rm = T),
#     share_workforce = sum(workforce, na.rm = T)/n(),
#     share_unemployed = sum(unemployed, na.rm = T)/n(),
#     n_employed = sum(workforce, na.rm = T),
#     observations = n()
#   ) %>%
#   ungroup()

#save datasets
save(csample_young_data, file="../Data/SOEP/gen/csample_young_data.Rda")
# save(sample_young_md_data, file="../Data/SOEP/gen/sample_young_md_data.Rda")
save(rdd_data, file="../Data/SOEP/gen/rdd_data.Rda")
save(did_data, file="../Data/SOEP/gen/did_data.Rda")
