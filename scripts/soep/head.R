#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#head.R

#This is the header R script that is sourced by all other scripts

#PACKAGES
installation_needed  <- F
loading_needed <- T
package_list <- c("haven", "tidyverse", "matrixStats", "rdrobust", "rdd", "plm", "stargazer")
if(installation_needed){install.packages(package_list, repos='http://cran.us.r-project.org')}
if(loading_needed){lapply(package_list, require, character.only = T)}

#clear workspace
rm(list=ls()) 
#SET WD in console