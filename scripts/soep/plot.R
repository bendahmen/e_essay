#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#plot.R

#The aim of this script is to create plots for the regression discontinuity design analysis

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
load(file = "../Data/SOEP/gen/csample_young_data.Rda")
load(file = "../Data/SOEP/gen/sample_young_md_data.Rda")

csample_young_data <- filter(csample_young_data, !year_group %in% c("1985-89"))

#plot employment share
ggplot(csample_young_data, aes(x=month_distance, y=workforce, group=adult, color=adult)) +
  geom_point(aes(y=mean_workforce), alpha=0.7) +
  geom_smooth(method="lm") +
  facet_wrap(~year_group, ncol=3)

#plot unemployment share
ggplot(csample_young_data, aes(x=month_distance, y=unemployed_status, group=adult, color=adult)) +
  geom_point(aes(y=mean_unemp), alpha=0.7) +
  geom_smooth(method="lm") +
  facet_wrap(~year_group, ncol=3)

#plot hourly wages
plot_data <- csample_young_data %>%
  group_by(month_distance_bins) %>%
  filter(!duplicated(binned_mean_hrl_wage))
ggplot(csample_young_data, aes(x=month_distance, y=hrl_wage, group=adult, color=adult)) +
  geom_point(data=plot_data, aes(x=month_distance, y=binned_mean_hrl_wage), alpha=0.6) +
  geom_smooth(method="lm") +
  ylim(0,15) +
  facet_wrap(~year_group, ncol=3)

