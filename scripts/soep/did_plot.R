#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#did_plot.R

#The aim of this script is to create plots for the differences-in-differences analysis


# Setup -------------------------------------------------------------------

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
load(file = "../Data/SOEP/gen/did_data.Rda")


# Plots -------------------------------------------------------------------

ggplot(data = did_data, aes(x=syear, y=workforce, weight = phrf, group = age_bins, color= age_bins)) +
  geom_point(aes(y=mean_workforce)) +
  geom_smooth(method = "lm")
