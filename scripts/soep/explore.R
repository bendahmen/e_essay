#PACKAGES
installation_needed  <- FALSE
loading_needed <- TRUE
package_list <- c('have', "tidyr", "dplyr")
if(installation_needed){install.packages(package_list, repos='http://cran.us.r-project.org')}
if(loading_needed){lapply(package_list, require, character.only = TRUE)}

rm(list=ls()) #clear workspace
#SET WD

