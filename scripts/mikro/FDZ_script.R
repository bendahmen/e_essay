# Setup -------------------------------------------------------------------

#PACKAGES
installation_needed  <- F
loading_needed <- T
package_list <- c("haven", "tidyverse", "matrixStats", "rdrobust", "rdd", "plm", "stargazer")
if(installation_needed){install.packages(package_list, repos='http://cran.us.r-project.org')}
if(loading_needed){lapply(package_list, require, character.only = T)}

#clear workspace
rm(list=ls()) 

#set Working Directory
#setwd("")

#Datenpfade
datenpfad <- ""
outputpfad <- ""

#logfile
file.create(paste(outputpfad,"log.txt", sep = ""))
sink(file = log.txt)

####################################################################################################################
####################################################################################################################
####################################################################################################################
  
  
####################################################################################################################
####################################################################################################################
### Titel des Projekts: 			<Effects of the German Minimum Wage on Youth Unemployment>
### Datengrundlage: 				<Mikrozensus 2014-2017>
###
### Dateiname des Programmcodes: 	<FDZ_script.R>
### erstellt: 						        <27.4.2021> 
### von: 							            <Benjamin Dahmen> 
### E-Mail: 						          <b.a.dahmen@lse.ac.uk> 
### Tel.: 							          <+49 15904345780> 
### 
### Dateiname des Output-Files: 	<log.txt> 
### 
### 
### Grundriss des Programms: 
###			<Dieses Programme erstellt verschiedene Graphen und Regressionsmodelle für eine Difference-in-Difference Analyse> 
###
### 
### Verwendete Variablen: 
### Originalvariablen: 	
###			 <EF29:   	Erwerbstyp 
### 			EF38:   	Nichterwerbstyp 
### 			EF44:  	  Alter
### 			EF131:  	Normale Arbeitszeit 
### 			EF134:  	Tatsächl. Arbeitszeit
### 			EF160:  	Erwerbstätigk. (Berichtsw.): Geringfügige Beschäftigung
### 			EF309:  	Allgemeiner Schulabschluss
### 			EF436:  	Nettoeinkommen 
### 			EF438:  	Nettoeinkommen (Hilfswert) 
###				EF442:  	Monatsnettoverdienst  
### 			EF952:  	Standardhochrechnungsfaktor 
###       EF2001:   Migrationsstatus 
###       EF2009:   Migrationsstatus (EF2001 recodiert)>
### 
###
### wesentliche neu angelegte Variablen in dieser Syntax <FDZ_script.R>:  
###			  <year:            Jahr des MZ
###       employed:         Dummy: Arbeit haben
### 			unemployed:  	    Dummy: arbeitssuchend 
### 			labour_force:   	Dummy: auf dem Arbeitsmarkt
### 			not_lbr_force:  	Dummy: nicht auf dem Arbeitsmarkt 
### 			marginal_empl: 	  Dummy: Minijobber
###       degree:           Dummy: allgemeiner Schulabschluss
###       migrant_status:   Dummy: Migrationsstatus
###       age_bins_large:   Faktor: Altersgruppen
###       age_bins_small:   Faktor: Altersgruppen
###       
###       Die meisten anderen Variablen sind selbstverständlich. Bei vielen handelt es sich um
###       einfache Faktorvarriablen einer numerischen Variable oder um Durschnittswerte verschiedener
###       Gruppen
###
### Gewichtungsvariable: 	EF952
###
###
####################################################################################################################
####################################################################################################################
  

# Data cleaning -----------------------------------------------------------

#mergin all years together and adding a dummy that determines the original year
mz_data <- data.frame()
for (t in c(2014:2017)) {
  #read MZ data from 2014-2017
  tmp_data <- read_dta(paste("../Data/Mikrozensus/DSF/DSF_MZ ", t, ".dta", sep = ""))
  tmp_data <- mutate(tmp_data, year = t)
  mz_data <- mz_data %>%
    bind_rows(tmp_data)
}
#keep relevant vars and rename
relevant_vars <- c("EF29", "EF38", "EF44", "EF131", "EF134", "EF952", "EF160", "EF309", "EF436", "EF438", "EF442", "EF2001", "EF2009", "year")
mz_data <- mz_data %>%
  select(all_of(relevant_vars)) %>%
  rename(
    age = EF44,
    weekly_hours = EF131,
    actual_weekly_hours = EF134
  ) %>%
  filter(age>14 & age< 30) %>%
  #create dummies for labour_force statuses and other outcomes as well as factors of age and years, square of age and a treatment indicator and its interaction with age (for Dif-in-Dif regression)
  mutate(
    employed = ifelse(EF29==1,1,ifelse(is.na(EF29),NA,0)),
    unemployed = ifelse(EF38 %in% c(1:21),1,ifelse(is.na(EF29),NA,0)),
    labour_force = ifelse(employed == 1 | unemployed == 1,1,ifelse(is.na(EF29),NA,0)),
    not_lbr_force = ifelse(EF38 %in% c(22,25),1,ifelse(is.na(EF29),NA,0)),
    marginal_empl = ifelse(EF160 %in% c(1:3),1,ifelse(is.na(EF160),NA,0)),
    degree = ifelse(EF309 == 1,1,0),
    migrant_status = ifelse(EF2009 %in% c(2:6),1,ifelse(is.na(EF160),NA,0)),
    age_bins_large = cut(age, c(0,17,23,26,30), labels = c("<18", "18-23", "24-26", "27-29")),
    age_bins_small = cut(age, c(0,17,20,23,26,30), labels = c("<18", "18-20", "21-23", "24-26", "27-29")),
    year_fac = as.factor(year),
    age_fac = as.factor(age),
    age_sq = age,#age,
    year_fac = as.factor(year),
    treatment_year = ifelse(year >= 2015,1,0),
    treatment_age = ifelse(age >= 18,1,0),
    treatment_interaction = ifelse(treatment_year == 1 & treatment_age == 1,1,0)
  ) %>%
  #groupy by different groupy and consequently calculate grouped means for the plots
  group_by(year, age_bins_large) %>%
  mutate(
    albin_mean_employed = weighted.mean(employed, weight = EF952, na.rm = T),
    albin_mean_unemployed = weighted.mean(unemployed, weight = EF952, na.rm = T),
    albin_mean_labour_force = weighted.mean(labour_force, weight = EF952, na.rm = T),
    albin_mean_not_lbr_force = weighted.mean(not_lbr_force, weight = EF952, na.rm = T),
    albin_mean_marginal_empl = weighted.mean(marginal_empl, weight = EF952, na.rm = T),
    albin_mean_degree = weighted.mean(degree, weight = EF952, na.rm = T),
    albin_observations = n(),
    treatment_x_1823 = ifelse(treatment_year == 1 & age_bins_large == "18-23",1,0),
    treatment_x_2426 = ifelse(treatment_year == 1 & age_bins_large == "24-26",1,0),
    treatment_x_2729 = ifelse(treatment_year == 1 & age_bins_large == "27-29",1,0),
    treatment_x_2729s = ifelse(treatment_year == 1 & age_bins_small == "27-29",1,0),
    treatment_x_2426s = ifelse(treatment_year == 1 & age_bins_small == "24-26",1,0),
    treatment_x_2123s = ifelse(treatment_year == 1 & age_bins_small == "21-23",1,0),
    treatment_x_1820s = ifelse(treatment_year == 1 & age_bins_small == "18-20",1,0),
    albin_observations = n()
  ) %>%
  ungroup() %>%
  group_by(year, age_bins_small) %>%
  mutate(
    asbin_mean_employed = weighted.mean(employed, weight = EF952, na.rm = T),
    asbin_mean_unemployed = weighted.mean(unemployed, weight = EF952, na.rm = T),
    asbin_mean_labour_force = weighted.mean(labour_force, weight = EF952, na.rm = T),
    asbin_mean_not_lbr_force = weighted.mean(not_lbr_force, weight = EF952, na.rm = T),
    asbin_mean_marginal_empl = weighted.mean(marginal_empl, weight = EF952, na.rm = T),
    asbin_mean_degree = weighted.mean(degree, weight = EF952, na.rm = T),
    asbin_observations = n()
  ) %>%
  ungroup() %>%
  group_by(age, year) %>%
  mutate(
    ybin_mean_employed = weighted.mean(employed, weight = EF952, na.rm = T),
    ybin_mean_unemployed = weighted.mean(unemployed, weight = EF952, na.rm = T),
    ybin_mean_labour_force = weighted.mean(labour_force, weight = EF952, na.rm = T),
    ybin_mean_not_lbr_force = weighted.mean(not_lbr_force, weight = EF952, na.rm = T),
    ybin_mean_marginal_empl = weighted.mean(marginal_empl, weight = EF952, na.rm = T),
    ybin_mean_degree = weighted.mean(degree, weight = EF952, na.rm = T),
    ybin_observations = n()
  ) %>%
  ungroup()

#generate tables with observations sizes for graphs
#first group data by groups that are displayed in plots and then keep only variables that are never missing to get the minimum no. of observations
#IMPORTANT: as I delete all observations that are missing in any of the outcome variables the generated no. of observations are the minimum number of observations that could be used for each point in each graph
observations_data <- mz_data %>%
  filter(
    !is.na(employed) & !is.na(unemployed) & !is.na(labour_force) & !is.na(not_lbr_force) & !is.na(marginal_empl)
  ) %>%
  group_by(age, year) %>%
  mutate(observations_age_year = n()) %>%
  ungroup() %>%
  group_by(year, age_bins_large) %>%
  mutate(observations_agel_year = n()) %>%
  ungroup() %>%
  group_by(year, age_bins_small) %>%
  mutate(observations_ages_year = n()) %>%
  ungroup() %>%
  select(c("age", "year", "age_bins_large", "age_bins_small", "observations_age_year", "observations_ages_year", "observations_agel_year"))

#for each grouping type I collect the minimum observations in a vector and export to excel
observations_year_age_vec <- c()
for (i in c(min(observations_data$age): max(observations_data$age))) {
  for (j in c(2014:2017)) {
    observations_year_age_vec <- append(observations_year_age_vec, values = paste("age",i,"year",j,"no:",nrow(observations_data[observations_data$age == i & observations_data$year == j,"observations_age_year"])))
  }
}
write.csv(observations_year_age_vec, file = "obs_plots_by_age.csv") #NICHT FÜR ANALYSE BENÖTIGT, beinhaltet minimale observationen für alle graphen die nach alter und jahr gruppiert sind

observations_year_ages_vec <- c()
for (i in c("<18", "18-20", "21-23", "24-26", "27-29")) {
  for (j in c(2014:2017)) {
    observations_year_ages_vec <- append(observations_year_ages_vec, values = paste("age",i,"year",j,"no:",nrow(observations_data[observations_data$age_bins_small == i & observations_data$year == j,"observations_ages_year"])))
  }
}  
write.csv(observations_year_ages_vec, file = "obs_plots_by_age_bins_small.csv") #NICHT FÜR ANALYSE BENÖTIGT, beinhaltet minimale observationen für alle graphen die nach jahr und den folgenden Altersgruppen gruppiert sind: "<18", "18-20", "21-23", "24-26", "27-29"

observations_year_agel_vec <- c()
for (i in c("<18", "18-23", "24-26", "27-29")) {
  for (j in c(2014:2017)) {
    observations_year_agel_vec <- append(observations_year_agel_vec, values = paste("age",i,"year",j,"no:",nrow(observations_data[observations_data$age_bins_large == i & observations_data$year == j,"observations_agel_year"])))
  }
} 
write.csv(observations_year_agel_vec, file = "obs_plots_by_age_bins_large.csv") #NICHT FÜR ANALYSE BENÖTIGT, beinhaltet minimale observationen für alle graphen die nach jahr und den folgenden Altersgruppen gruppiert sind: ""<18", "18-23", "24-26", "27-29"

# Plots -------------------------------------------------------------------
#outcome variables for plots and regressions
varlist <- list("employed", "unemployed", "labour_force", "not_lbr_force", "marginal_empl")

#in the following i create all the Dif-in-Dif plots
#Alle Graphen werden für die Analyise benötigt, die entsprechenden minimalen Observationen die zugrundeliegen, können den oben erstellten Excel-Tabellen entnommen werden

age_bins_plots <- lapply(X = varlist, FUN = function(vars) {
  ggplot(data = mz_data, aes_(x=as.name("year"), y=as.name(paste("asbin_mean_",vars, sep = "")), group = as.name("age_bins_small"), color = as.name("age_bins_small"), weight = as.name("EF952"))) +
    geom_point() +
    geom_line()
})
ggsave(age_bins_plots[[1]], "age_bins_plot_employed") #FÜR ANALYSE BENÖTIGT
ggsave(age_bins_plots[[2]], "age_bins_plot_unemployed") #FÜR ANALYSE BENÖTIGT
ggsave(age_bins_plots[[3]], "age_bins_plot_labour_force") #FÜR ANALYSE BENÖTIGT
ggsave(age_bins_plots[[1]], "age_bins_plot_not_lbr_force") #FÜR ANALYSE BENÖTIGT
ggsave(age_bins_plots[[1]], "age_bins_plot_marginal_empl") #FÜR ANALYSE BENÖTIGT

age_binl_plots <- lapply(X = varlist, FUN = function(vars) {
  ggplot(data = mz_data, aes_(x=as.name("year"), y=as.name(paste("albin_mean_",vars, sep = "")), group = as.name("age_bins_large"), color = as.name("age_bins_large"), weight = as.name("EF952"))) +
    geom_point() +
    geom_line()
})
ggsave(age_binl_plots[[1]], "age_binl_plot_employed") #FÜR ANALYSE BENÖTIGT
ggsave(age_binl_plots[[2]], "age_binl_plot_unemployed") #FÜR ANALYSE BENÖTIGT
ggsave(age_binl_plots[[3]], "age_binl_plot_labour_force") #FÜR ANALYSE BENÖTIGT
ggsave(age_binl_plots[[1]], "age_binl_plot_not_lbr_force") #FÜR ANALYSE BENÖTIGT
ggsave(age_binl_plots[[1]], "age_binl_plot_marginal_empl") #FÜR ANALYSE BENÖTIGT

year_bin_plots <- lapply(X = varlist, FUN = function(vars) {
  ggplot(data = mz_data, aes_(x=as.name("age"), y=as.name(paste("ybin_mean_",vars, sep = "")), group = as.name("year"), color = as.name("year_fac"), weight = as.name("EF952"))) +
    geom_point() +
    geom_line()
})
ggsave(year_bin_plots[[1]], "year_bin_plot_employed") #FÜR ANALYSE BENÖTIGT
ggsave(year_bin_plots[[2]], "year_bin_plot_unemployed") #FÜR ANALYSE BENÖTIGT
ggsave(year_bin_plots[[3]], "year_bin_plot_labour_force") #FÜR ANALYSE BENÖTIGT
ggsave(year_bin_plots[[1]], "year_bin_plot_not_lbr_force") #FÜR ANALYSE BENÖTIGT
ggsave(year_bin_plots[[1]], "year_bin_plot_marginal_empl") #FÜR ANALYSE BENÖTIGT

# Estimations -------------------------------------------------------------

#Dif-in-Dif regression with various specifications
#Alle Latex-Outputs werden benötigt, Fallzahlen können der jeweils erstellten Tabelle entnommen werden

#reg on years and small agebins
did_y_sa <- lapply(X = varlist, FUN = function(vars) {
  formula_s <- paste(vars, "~ age_bins_small + year + treatment_x_2729s + treatment_x_2426s + treatment_x_2123s + treatment_x_1820s")
  formula <- as.formula(formula_s)
  lm(data = mz_data, formula = formula, weight = EF952)
  } )
stargazer(did_y_sa, out.header = T, out="did_y_sa.tex") #FÜR ANALYSE BENÖTIGT

#reg on ybins and small agebins
did_yb_sa <- lapply(X = varlist, FUN = function(vars) {
  formula_s <- paste(vars, "~ age_bins_small + year_fac + treatment_x_2729s + treatment_x_2426s + treatment_x_2123s + treatment_x_1820s")
  formula <- as.formula(formula_s)
  lm(data = mz_data, formula = formula, weight = EF952)
} )
stargazer(did_yb_sa, out.header = T, out="did_yb_sa.tex") #FÜR ANALYSE BENÖTIGT

#reg on years and large agebins
did_y_la <- lapply(X = varlist, FUN = function(vars) {
  formula_s <- paste(vars, "~ age_bins_large + year + treatment_x_2729s + treatment_x_2426s + treatment_x_2123s + treatment_x_1820s")
  formula <- as.formula(formula_s)
  lm(data = mz_data, formula = formula, weight = EF952)
} )
stargazer(did_y_la, out.header = T, out="did_y_la.tex") #FÜR ANALYSE BENÖTIGT

#reg on ybins and large agebins
did_yb_la <- lapply(X = varlist, FUN = function(vars) {
  formula_s <- paste(vars, "~ age_bins_large + year_fac + treatment_x_2729s + treatment_x_2426s + treatment_x_2123s + treatment_x_1820s")
  formula <- as.formula(formula_s)
  lm(data = mz_data, formula = formula, weight = EF952)
} )
stargazer(did_yb_la, out.header = T, out="did_yb_la.tex") #FÜR ANALYSE BENÖTIGT

#reg on years and age
did_y_a <- lapply(X = varlist, FUN = function(vars) {
  formula_s <- paste(vars, "~ age + year + treatment_x_2729s + treatment_x_2426s + treatment_x_2123s + treatment_x_1820s")
  formula <- as.formula(formula_s)
  lm(data = mz_data, formula = formula, weight = EF952) 
} )
stargazer(did_y_a, out.header = T, out="did_y_a.tex") #FÜR ANALYSE BENÖTIGT

#reg on ybins and age
did_yb_a <- lapply(X = varlist, FUN = function(vars) {
  formula_s <- paste(vars, "~ age + year_fac + treatment_x_2729s + treatment_x_2426s + treatment_x_2123s + treatment_x_1820s")
  formula <- as.formula(formula_s)
  lm(data = mz_data, formula = formula, weight = EF952)
} )
stargazer(did_yb_a, out.header = T, out="did_yb_a.tex") #FÜR ANALYSE BENÖTIGT
