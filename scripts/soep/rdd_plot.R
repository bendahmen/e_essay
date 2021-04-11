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
load(file = "../Data/SOEP/gen/rdd_data.Rda")
load(file = "../Data/SOEP/gen/sample_young_md_data.Rda")

#create treatment sample
treatment_sample <- rdd_data %>%
  filter(year_group == "2015+ Treatment")


# RDD fitted scatter plots ------------------------------------------------


#define basic plot
b_plot <- ggplot(treatment_sample, aes(x=month_distance, weight=phrf, group=adult, color=adult))

#scatter plot of all observations binary data
binary_outcomes <- list("workforce", "unemployed")
binary_scatter_plots <- lapply(X = binary_outcomes, FUN = function(vars) { ggplot(rdd_data, aes(x=month_distance, weight=phrf, group=adult, color=adult)) + geom_point(aes_(y=as.name(paste("mean_",vars, sep="")))) + facet_wrap(~year_group, ncol = 2) } )
ggsave(plot=binary_scatter_plots[[1]], "plots/scatter_emp.png")
ggsave(plot=binary_scatter_plots[[2]], "plots/scatter_unemp.png")


#binned scatter plot binary data
binned_emp_scatter_plot <- ggplot(rdd_data, aes(x=month_distance_bins, weight=phrf, group=adult, color=adult)) +
  geom_point(aes(y=binned_mean_workforce, size=binned_obversations)) + facet_wrap(~year_group, ncol = 2)
ggsave("plots/binned_emp_scatter.png")

binned_unemp_scatter_plot <- ggplot(rdd_data, aes(x=month_distance_bins, weight=phrf, group=adult, color=adult)) +
  geom_point(aes(y=binned_mean_unemployed, size=binned_obversations)) + facet_wrap(~year_group, ncol = 2)
ggsave("plots/binned_unemp_scatter.png")

#scatter plot of all observations cont data
hrl_wage_scatter_plot <- ggplot(rdd_data, aes(x=month_distance, weight=phrf, group=adult, color=adult)) + 
  geom_point(aes(y=hrl_wage)) + facet_wrap(~year_group, ncol = 2) + ylim(0,40)
ggsave("plots/scatter_hrl_wage.png")

#binned scatter plot hrl_wage
binned_hrl_wage_scatter_plot <- ggplot(rdd_data, aes(x=month_distance_bins, weight=phrf, group=adult, color=adult)) +
  geom_point(aes(y=binned_median_hrl_wage, size=binned_obversations)) + expand_limits(y=0) + facet_wrap(~year_group, ncol = 2)
ggsave("plots/binned_hrl_wage_scatter.png")
#smaller bins
sbinned_hrl_wage_scatter_plot <- ggplot(rdd_data, aes(x=month_distance, weight=phrf, group=adult, color=adult)) +
  geom_point(aes(y=median_hrl_wage, size=observations)) + expand_limits(y=0) + facet_wrap(~year_group, ncol = 2)
ggsave("plots/sbinned_hrl_wage_scatter.png")

#plot employment share
employment_scatter_linear_plot <- ggplot(rdd_data, aes(x=month_distance, y=workforce, group=adult, color=adult, weight=phrf)) +
  geom_point(aes(y=mean_workforce), alpha=0.7) +
  geom_smooth(method="lm") +
  facet_wrap(~year_group, ncol=2)
ggsave("plots/employment_linear.png")
employment_scatter_quadratic_plot <- ggplot(rdd_data, aes(x=month_distance, y=workforce, group=adult, color=adult, weight=phrf)) +
  geom_point(aes(y=mean_workforce), alpha=0.7) +
  geom_smooth(method="lm", formula = y ~ x + I(x^2)) +
  facet_wrap(~year_group, ncol=2)
ggsave("plots/employment_quadratic.png")

#plot unemployment share
unemployment_scatter_linear_plot <- ggplot(rdd_data, aes(x=month_distance, y=unemployed, group=adult, color=adult, weight=phrf)) +
  geom_point(aes(y=mean_unemployed), alpha=0.7) +
  geom_smooth(method="lm") +
  facet_wrap(~year_group, ncol=2)
ggsave("plots/unemp_linear.png")
unemployment_scatter_quadratic_plot <- ggplot(rdd_data, aes(x=month_distance, y=unemployed, group=adult, color=adult, weight=phrf)) +
  geom_point(aes(y=mean_unemployed), alpha=0.7) +
  geom_smooth(method="lm", formula = y ~ x + I(x^2)) +
  facet_wrap(~year_group, ncol=2)
ggsave("plots/unemp_quadratic.png")

#plot hourly wages
plot_data <- rdd_data %>%
  group_by(month_distance_bins) %>%
  filter(!duplicated(binned_mean_hrl_wage))
hrl_wage_scatter_linear_plot <- ggplot(rdd_data, aes(x=month_distance, y=hrl_wage, group=adult, color=adult, weight=phrf)) +
  geom_point(data=plot_data, aes(x=month_distance, y=binned_mean_hrl_wage), alpha=0.6) +
  geom_smooth(method="lm") +
  ylim(0,15) +
  facet_wrap(~year_group, ncol=2)
ggsave("plots/hrl_wage_linear.png")
hrl_wage_scatter_quadratic_plot <- ggplot(rdd_data, aes(x=month_distance, y=hrl_wage, group=adult, color=adult, weight=phrf)) +
  geom_point(data=plot_data, aes(x=month_distance, y=binned_mean_hrl_wage), alpha=0.6) +
  geom_smooth(method="lm", formula = y ~ x + I(x^2)) +
  ylim(0,15) +
  facet_wrap(~year_group, ncol=2)
ggsave("plots/hrl_wage_quadratic.png")




# RDD robustness plots ----------------------------------------------------

#density in running variable
mdist_density_plot <- treatment_sample %>%
  group_by(month_distance) %>%
  mutate(amount = n()) %>%
  ggplot() + 
    geom_histogram(aes(x=month_distance, fill=adult), binwidth = 1, alpha = 0.6) +
    geom_smooth(aes(x=month_distance, y=amount, group=adult_dummy, color=adult), method="lm", formula = y ~ x + I(x^2) + I(x^3) + I(x^4))

#jumps at cutoffs

#school leaving degree
sch_leaving_disc_plot <- ggplot(rdd_data, aes(x=month_distance, y=degree, group=adult, color=adult, weight=phrf)) +
  geom_point(aes(y=mean_degree), alpha=0.7) +
  geom_smooth(method="lm", formula = y ~ x + I(x^2)) +
  facet_wrap(~year_group, ncol=2)
ggsave("plots/employment_linear.png")

#time within firm
firm_time_disc_plot <- ggplot(rdd_data, aes(x=month_distance, y=pgerwzeit, group=adult, color=adult, weight=phrf)) +
  geom_point(aes(y=mean_pgerwzeit), alpha=0.7) +
  geom_smooth(method="lm", formula = y ~ x) +
  facet_wrap(~year_group, ncol=2)
ggsave("plots/employment_linear.png")


# OUTPUT ------------------------------------------------------------------


