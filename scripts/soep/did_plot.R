#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#did_plot.R

#The aim of this script is to create plots for the differences-in-differences analysis


# Setup -------------------------------------------------------------------

source("scripts/soep/head.R")

#SET WD in console

#load data
load(file = "../Data/SOEP/gen/did_data.Rda")


# Plots -------------------------------------------------------------------

ggplot(data = did_data[did_data$syear>2010,], aes(x=syear, y=labour_force, weight = phrf, group = age_bins, color= age_bins)) +
  geom_point(aes(y=agebin_mean_labour_force)) +
  geom_line(aes(y=agebin_mean_labour_force))

ggplot(data = did_data, aes(x=age, y = labour_force, weight = phrf, group= year_bins, color = year_bins)) +
  geom_point(aes(y = yearbin_mean_labour_force)) +
  geom_line(aes(y=yearbin_mean_labour_force))


