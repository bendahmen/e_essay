#EXTENDED ESSAY LSE
#Author: Benjamin Dahmen
#assemble.R

#The aim of this script is to assemble the data set needed for preliminary analysis
#WARNING: Scripts runs very long due to replacing missing values in line 62

#PACKAGES
installation_needed  <- F
loading_needed <- T
package_list <- c('haven', "tidyr", "dplyr")
if(installation_needed){install.packages(package_list, repos='http://cran.us.r-project.org')}
if(loading_needed){lapply(package_list, require, character.only = T)}

#clear workspace
rm(list=ls()) 
#SET WD in console

#import individual tracking data (long)
p_tracking_data <- read_dta("../Data/SOEP/cs-transfer/soep.v35.international.stata_dta/ppathl.dta")
#keep relevant vars from individual tracking data
p_tracking_data_vars <- c("pid", "syear", "sex", "gebjahr", "todjahr",
                          "germborn", "corigin", "gebmonat", "migback", "phrf")
p_tracking_data <- p_tracking_data %>%
  select(all_of(p_tracking_data_vars))

#import generated biographical data
p_bio_data <- read_dta("../Data/SOEP/cs-transfer/soep.v35.international.stata_dta/pgen.dta")
#keep relevant vars from individual generated biographical data
p_bio_data_vars <- c("pid", "syear", "pgnation", "pglabgro", "pgimpgro", "pglabnet", 
                     "pgsndjob", "pgimpsnd", "pgstib", "pgemplst", "pglfs", "pgjobch", 
                     "pgerwzeit", "pgtatzeit", "pgvebzeit", "pguebstd", "pgbetr", 
                     "pgallbet", "pgbilzeit", "pgpsbil", "pgpbbil01", "pgpbbil02", 
                     "pgpbbil03", "pgmonth")
p_bio_data <- p_bio_data %>%
  select(all_of(p_bio_data_vars))

#import individual extended income data
p_ext_income_data <- read_dta("../Data/SOEP/cs-transfer/soep.v35.international.stata_dta/pequiv.dta")
#keep relevant vars from individual extended income data
p_ext_income_vars <- c("pid", "syear", "d11102ll", "i11110", "ijob1", "ijob2", "iself", "iunby", "iunay")
p_ext_income_data <- p_ext_income_data %>%
  select(all_of(p_ext_income_vars))

assembled_data <- p_tracking_data %>%
  left_join(p_bio_data, by = c("pid", "syear")) %>%
  left_join(p_ext_income_data, by = c("pid", "syear"))

#generate age last month, if month vars missing generate approx age; could extend to drop 17/18 year olds with approx age
assembled_data <- assembled_data %>%
  mutate(age = ifelse(gebmonat > 0 & pgmonth > 0, ifelse(gebmonat < pgmonth, (syear - gebjahr), (syear - gebjahr - 1)),syear - gebjahr))
  
#save assembled data
save(assembled_data, file="../Data/SOEP/gen/assembled_data.Rda")

#create young people sample
sample_young_data <- assembled_data %>%
  filter(age < 30 & age > 13)

#save sample
save(sample_young_data, file="../Data/SOEP/gen/sample_young_data.Rda")
