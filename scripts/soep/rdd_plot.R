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

binary_outcomes <- list("workforce", "unemployed", "labour_force")
cont_outcomes <- list("hrl_wage", "pgvebzeit", "pgtatzeit")

# RDD fitted scatter plots ------------------------------------------------


#define basic plots
plot_x_month_wgc_wrap <- ggplot(rdd_data, aes(x=month_distance, weight=phrf, group=adult, color=adult)) + facet_wrap(~year_group, ncol = 2)

plot_x_monthbins_wgc_wrap <- ggplot(rdd_data, aes(x=month_distance_bins, weight=phrf, group=adult, color=adult)) + facet_wrap(~year_group, ncol = 2)

#scatter plot of all observations binary data
binary_scatter_plots <- lapply(X = binary_outcomes, FUN = function(vars) { plot_x_month_wgc_wrap + geom_point(aes_(y=as.name(paste("mean_",vars, sep="")))) } )

#binned scatter plot binary data
binary_binned_scatter_plots <- lapply(X = binary_outcomes, FUN = function(vars) { plot_x_monthbins_wgc_wrap + geom_point(aes_(y=as.name(paste("binned_mean_",vars, sep="")), size = as.name("binned_obversations"))) } )

#scatter plot of all observations cont data
cont_scatter_plots <- lapply(X = cont_outcomes, FUN = function(vars) { plot_x_month_wgc_wrap + geom_point(aes_(y=as.name(vars))) } )

#binned scatter plot hrl_wage
cont_binned_scatter_plots <- lapply(X = cont_outcomes, FUN = function(vars) { plot_x_monthbins_wgc_wrap + 
    geom_point(aes_(y=as.name(paste("binned_median_",vars, sep="")), size = as.name("binned_obversations"))) +
    expand_limits(y=0)
    } )

#smaller bins
cont_binned_scatter_plots <- lapply(X = cont_outcomes, FUN = function(vars) { plot_x_month_wgc_wrap + 
    geom_point(aes_(y=as.name(paste("mean_",vars, sep="")), size = as.name("binned_obversations"))) +
    expand_limits(y=0)
    } )

#plot binary fitted plots
binary_linear_scatter_plots <- lapply(X = binary_outcomes, FUN = function(vars) { plot_x_month_wgc_wrap + 
    geom_point(aes_(y=as.name(paste("mean_",vars, sep=""))), alpha = 0.7) +
    geom_smooth(aes_(y=as.name(vars)), method = "lm")
    } )

binary_quadratic_scatter_plots <- lapply(X = binary_outcomes, FUN = function(vars) { plot_x_month_wgc_wrap + 
    geom_point(aes_(y=as.name(paste("mean_",vars, sep=""))), alpha = 0.7) +
    geom_smooth(aes_(y=as.name(vars)), method = "lm", formula = y ~ x + I(x^2))
    } )

#plot cont fitted plots
cont_linear_scatter_plots <- lapply(X = cont_outcomes, FUN = function(vars) { plot_x_month_wgc_wrap + 
    geom_point(
      data = rdd_data,
      aes_(y=as.name(paste("mean_",vars, sep=""))), alpha = 0.6) +
    geom_smooth(data = filter(rdd_data, hrl_wage<200), aes_(y=as.name(vars)), method = "lm")
    } )

cont_quadratic_scatter_plots <- lapply(X = cont_outcomes, FUN = function(vars) { plot_x_month_wgc_wrap + 
    geom_point(
      data = rdd_data %>%
        group_by(month_distance_bins) %>%
        filter(!duplicated(binned_mean_hrl_wage)),
      aes_(y=as.name(paste("binned_mean_",vars, sep=""))), alpha = 0.6) +
    geom_smooth(data = filter(rdd_data, hrl_wage<200), aes_(y=as.name(vars)), method = "lm", formula = y ~ x + I(x^2))
} )


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
sch_leaving_disc_plot <- plot_x_month_wgc_wrap +
  geom_point(aes(y=mean_degree), alpha=0.7) +
  geom_smooth(aes(y = degree), method="lm", formula = y ~ x + I(x^2))

#time within firm
firm_time_disc_plot <- plot_x_month_wgc_wrap +
  geom_point(aes(y=mean_pgerwzeit), alpha=0.7) +
  geom_smooth(aes(y = degree), method="lm", formula = y ~ x)


# OUTPUT ------------------------------------------------------------------


