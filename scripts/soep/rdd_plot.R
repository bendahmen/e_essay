#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#rdd_plot.R

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

csample_young_data <- filter(csample_young_data, !year_group %in% c("1985-89", "1990-94", "1995-99"))
#create treatment sample
treatment_sample <- csample_young_data %>%
  filter(year_group == "2015+ Treatment")

#define basic plot
b_plot <- ggplot(treatment_sample, aes(x=month_distance, weight=phrf, group=adult, color=adult))

#scatter plot of all observations binary data
binary_outcomes <- list("workforce", "unemployed")
binary_scatter_plots <- lapply(X = binary_outcomes, FUN = function(vars) { ggplot(treatment_sample, aes(x=month_distance, weight=phrf, group=adult, color=adult)) + geom_point(aes_(y=as.name(paste("mean_",vars, sep="")))) } )

#binned scatter plot binary data
binned_emp_scatter_plot <- 
  binned_hrl_wage_scatter_plot <- ggplot(treatment_sample, aes(x=month_distance_bins, weight=phrf, group=adult, color=adult)) +
  geom_point(aes(y=binned_mean_workforce, size=binned_obversations))
binned_unemp_scatter_plot <- 
  binned_hrl_wage_scatter_plot <- ggplot(treatment_sample, aes(x=month_distance_bins, weight=phrf, group=adult, color=adult)) +
  geom_point(aes(y=binned_mean_unemployed, size=binned_obversations))

#scatter plot of all observations cont data
hrl_wage_scatter_plot <- ggplot(treatment_sample, aes(x=month_distance, weight=phrf, group=adult, color=adult)) + 
  geom_point(aes(y=hrl_wage))

#binned scatter plot hrl_wage
binned_hrl_wage_scatter_plot <- ggplot(treatment_sample, aes(x=month_distance_bins, weight=phrf, group=adult, color=adult)) +
  geom_point(aes(y=binned_median_hrl_wage, size=binned_obversations)) + expand_limits(y=0)
#smaller bins
sbinned_hrl_wage_scatter_plot <- ggplot(treatment_sample, aes(x=month_distance, weight=phrf, group=adult, color=adult)) +
  geom_point(aes(y=median_hrl_wage, size=observations)) + expand_limits(y=0)

#plot employment share
employment_scatter_linear_plot <- ggplot(csample_young_data, aes(x=month_distance, y=workforce, group=adult, color=adult, weight=phrf)) +
  geom_point(aes(y=mean_workforce), alpha=0.7) +
  geom_smooth(method="lm") +
  facet_wrap(~year_group, ncol=2)
employment_scatter_quadratic_plot <- ggplot(csample_young_data, aes(x=month_distance, y=workforce, group=adult, color=adult, weight=phrf)) +
  geom_point(aes(y=mean_workforce), alpha=0.7) +
  geom_smooth(method="lm", formula = y ~ x + I(x^2)) +
  facet_wrap(~year_group, ncol=2)

#plot unemployment share
unemployment_scatter_linear_plot <- ggplot(csample_young_data, aes(x=month_distance, y=unemployed, group=adult, color=adult, weight=phrf)) +
  geom_point(aes(y=mean_unemployed), alpha=0.7) +
  geom_smooth(method="lm") +
  facet_wrap(~year_group, ncol=2)
unemployment_scatter_quadratic_plot <- ggplot(csample_young_data, aes(x=month_distance, y=unemployed, group=adult, color=adult, weight=phrf)) +
  geom_point(aes(y=mean_unemployed), alpha=0.7) +
  geom_smooth(method="lm", formula = y ~ x + I(x^2)) +
  facet_wrap(~year_group, ncol=2)

#plot hourly wages
plot_data <- csample_young_data %>%
  group_by(month_distance_bins) %>%
  filter(!duplicated(binned_mean_hrl_wage))
hrl_wage_scatter_linear_plot <- ggplot(csample_young_data, aes(x=month_distance, y=hrl_wage, group=adult, color=adult, weight=phrf)) +
  geom_point(data=plot_data, aes(x=month_distance, y=binned_mean_hrl_wage), alpha=0.6) +
  geom_smooth(method="lm") +
  ylim(0,15) +
  facet_wrap(~year_group, ncol=2)
hrl_wage_scatter_quadratic_plot <- ggplot(csample_young_data, aes(x=month_distance, y=hrl_wage, group=adult, color=adult, weight=phrf)) +
  geom_point(data=plot_data, aes(x=month_distance, y=binned_mean_hrl_wage), alpha=0.6) +
  geom_smooth(method="lm", formula = y ~ x + I(x^2)) +
  ylim(0,15) +
  facet_wrap(~year_group, ncol=2)

