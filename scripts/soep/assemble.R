#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#merge.R

#The aim of this script is to assemble the data set needed for preliminary analysis

#PACKAGES
installation_needed  <- FALSE
loading_needed <- TRUE
package_list <- c('haven', "tidyr", "dplyr")
if(installation_needed){install.packages(package_list, repos='http://cran.us.r-project.org')}
if(loading_needed){lapply(package_list, require, character.only = TRUE)}

rm(list=ls()) #clear workspace
#SET WD

#import individual tracking data (long)
p_tracking_data <- read_dta("../Data/SOEP/cs-transfer/soep.v35.international.stata_dta/ppathl.dta")
#keep relevant vars from individual tracking data
p_tracking_data_vars <- c("pid", "syear", "sex", "gebjahr", "todjahr",
                          "germborn", "corigin", "gebmonat", "migback", "phrf", "piyear")
p_tracking_data <- p_tracking_data %>%
  select(all_of(p_tracking_data_vars))

#keep relevant population
p_tracking_data <- p_tracking_data %>%
  mutate(approx_age = syear - gebjahr) %>%
  filter(approx_age < 22 & approx_age > 14,
         syear > 2000)

#import generated biographical data
p_bio_data <- read_dta("../Data/SOEP/cs-transfer/soep.v35.international.stata_dta/pgen.dta")
#keep relevant vars from individual generated biographical data
p_bio_data_vars <- c("pid", "syear", "pgnation", "pglabgro", "pgimpgro", "pglabnet", 
                     "pgsndjob", "pgimpsnd", "pgstib", "pgemplst", "pgjobch", 
                     "pgerwzeit", "pgtatzeit", "pgvebzeit", "pguebstd", "pgbetr", 
                     "pgallbet", "pgbilzeit", "pgpsbil", "pgpbbil01", "pgpbbil02", 
                     "pgpbbil03", "pgsndjob2", "pgsndjob3", "pgmonth", "pgpiyear")
p_bio_data <- p_bio_data %>%
  select(all_of(p_bio_data_vars))

#import individual extended income data
p_ext_income_data <- read_dta("../Data/SOEP/cs-transfer/soep.v35.international.stata_dta/pequiv.dta")
#keep relevant vars from individual extended income data
p_ext_income_vars <- c("pid", "syear", "d11102ll", "i11110", "ijob1", "ijob2", "iself", "iunby", "iunay", "iunay")
p_ext_income_data <- p_ext_income_data %>%
  select(all_of(p_ext_income_vars))

final_data <- p_tracking_data %>%
  left_join(p_bio_data, by = c("pid", "syear")) %>%
  left_join(p_ext_income_data, by = c("pid", "syear"))

#save generated data
save(final_data, file="../Data/SOEP/gen/assembled_data.Rda")
