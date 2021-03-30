#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#rdd_plot.R

#The aim of this script is to obtain RDD estimates for the data

#PACKAGES
installation_needed  <- F
loading_needed <- T
package_list <- c("haven", "tidyverse", "rdrobust")
if(installation_needed){install.packages(package_list, repos='http://cran.us.r-project.org')}
if(loading_needed){lapply(package_list, require, character.only = T)}

#clear workspace
rm(list=ls()) 
#SET WD in console

#load data
load(file = "../Data/SOEP/gen/csample_young_data.Rda")

# #calculate optimal bandwidth
# csample_young_data$cutoff<-NULL
# csample_young_data$bandwidth<-NULL
# csample_young_data$kw<-NULL
# bws <- rdbwselect(csample_young_data$workforce, csample_young_data$month_distance, p=1, c = 0,
#                   kernel = "tri", bwselect="msetwo")

model <- rdrobust(csample_young_data$hrl_wage, csample_young_data$month_distance, bwselect = "msetwo", weights = csample_young_data$phrf)
