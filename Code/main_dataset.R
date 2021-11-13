# main_dataset.R

# Author: Luca Moreno-Louzada
# 2021
# Code for manuscript:
# "Staying at Home during Covid Outbreaks Leads to Less Conceptions"
# Luca Moreno-Louzada and Naercio Menezes-Filho


# This code: 
# > Prepares dataset with week-municipality-level data
# > Prepares dataset with week-microregion-level data
# > Prepares dataset with month-municipality-level data
# > Prepares dataset with placebo data for other years
# > Prepares dataset with data grouped by different weekdays
# > Prepares datasets with total conceptions for charts and maps

###########################################################################

# Step 0:  Cleaning workplace and loading libraries

rm(list = ls())

setwd("C:/Users/lucam/OneDrive/Área de Trabalho/Economia/COVID SINASC")

library(dplyr) # data wrangling
library(read.dbc) # read .dbc files
library(readstata13) # read .dta files
library(readr) # read .csv files
library(readxl) # read excel files
library(data.table) # dealing with large files
library(lubridate) # dealing with dates
library(tidylog) # tracking changes
library(cowplot) # plot grids
library(ggplot2) # plots
library(stargazer) # regression results
library(estimatr) # robust SE's
library(broom) # tidy regression coefficients
library(tidyr) # organizing data
library(purrr) # applying functions
library(foreign) # save .dta files
library(lfe) # fixed effects models
library(sf) # dealing with maps

options(scipen = 999) # disable scientific notation

###########################################################################

# Step 1: Read source datasets

# Population and GDP
pib = read_excel("pib.xls") %>% select("Ano", 
                                       "Código do Município",
                                       "Produto Interno Bruto per capita, \na preços correntes\n(R$ 1,00)",
                                       "Código da Microrregião",
                                       "Nome da Microrregião",
                                       "Código da Grande Região",
                                       "Nome da Grande Região")

pib = pib %>% filter(Ano == 2018) %>% select(-Ano) %>% 
      rename(codmun = "Código do Município", 
             gdpc = "Produto Interno Bruto per capita, \na preços correntes\n(R$ 1,00)",
             microregion = "Código da Microrregião",
             microregion_name = "Nome da Microrregião",
             region = "Código da Grande Região",
             region_name = "Nome da Grande Região")

pib$codmun = as.character(pib$codmun)

ibge = read_excel("pop.xlsx", skip = 1, sheet = 2)
ibge$`COD. UF` = as.character(ibge$`COD. UF`)

ibge = ibge %>% unite(codmun, "COD. UF", "COD. MUNIC", sep = "", 
                      remove = TRUE) %>% 
       rename(name = `NOME DO MUNICÍPIO`,
              popm = `POPULAÇÃO ESTIMADA`)

# Extract population numbers from string variable
ibge$popm = gsub("\\(.*", "", ibge$popm)
ibge$popm = gsub(" ", "", ibge$popm)
ibge$popm = as.numeric(ibge$popm)

# Join GDP and population datasets
ibge = right_join(ibge, pib, by = "codmun")

# Join Rural-Urban classification dataset
urban = read_excel("Tipologia_municipal_rural_urbano.xlsx", sheet = 2) %>% 
        select(TIPO, CD_GCMUN) %>% 
        rename(type = TIPO,
               codmun = CD_GCMUN)

urban$codmun = as.character(urban$codmun)

ibge = left_join(ibge, urban, by = "codmun")

ibge = ibge %>% 
       mutate(urban = type == "Urbano") %>% 
       select(-type)

ibge$urban[is.na(ibge$urban)] = 0

# Convert 7 digit municipality codes to 6 digits
ibge$codmun = as.numeric(ibge$codmun)
ibge$codmun6 = as.character(floor(ibge$codmun / 10))
ibge = ibge %>% select(-codmun)

# Dataframe with data grouped by microregion
ibge_mic = ibge %>% 
           group_by(UF, region, region_name, microregion, microregion_name) %>% 
           summarise(pop = sum(popm),
                     gdp = weighted.mean(gdpc, popm))

# Microregion dictionary
microregions = ibge %>% select(codmun6, microregion)

# Births
births = fread("birthdata.csv", 
               colClasses = c(V1 = "character",
                              V2 = "character")) 

names(births) = c("codmun6", "date", "sex", "mother_marital",
                  "mother_educ", "kids", "multiple",
                  "delivery", "medical", "race", "mother_age", 
                  "bweeks", "bweight")

# Births from 2021 and 2012-14 are on different data sources
births_21 = fread("births2021.csv", 
                  colClasses = c(CODMUNRES = "character",
                                 DTNASC  = "character"))

names(births_21) = names(births)

births_12_14 = fread("birthdata_12_14.csv", 
                     colClasses = c(V1 = "character",
                                    V2  = "character"))

names(births_12_14) = names(births)

# Drop unused columns
births_21 = births_21 %>% select(codmun6, date, mother_educ, 
                                 delivery, mother_age,
                                 bweeks, bweight, kids)

births = births %>% select(codmun6, date, mother_educ,
                           delivery, mother_age,
                           bweeks, bweight, kids)

births_12_14 = births_12_14 %>% select(codmun6, date, mother_educ,
                                       delivery, mother_age,
                                       bweeks, bweight, kids)

births = bind_rows(births, births_21)
births = bind_rows(births, births_12_14)

births$date = as.Date(as.character(births$date), format = "%d%m%Y")

# Calculate conception date, month and year
births$conc_date = births$date - births$bweeks * 7
births$month = month(births$conc_date)
births$year = year(births$conc_date)

# Add microregion codes
births = left_join(microregions, births, by = "codmun6") 

# Filter to remove missing data and input errors
births = births %>% filter(!(is.na(births$delivery))) #  31,469 rows (<1%)
births = births %>% filter(!(is.na(births$bweight))) # 10,574 rows (<1%)
births = births %>% filter(!(is.na(births$bweeks))) # 766,569 rows (3%)
births = births %>% filter(mother_educ < 9) # 452,525 rows (2%)
births = births %>% filter(between(mother_age, 10, 85)) # 255 rows (<1%)

# Filter to remove 3 microregions with too few observations
births = births %>% filter(microregion != 26019) # Fernando de Noronha - PE
births = births %>% filter(microregion != 27007 ) # Traipu - AL          
births = births %>% filter(microregion != 35006 ) # Auriflama - SP         

births20 = births %>% filter(year == 2020)

# Deaths
deaths = fread("deathdata.csv", 
               colClasses = c(V1 = "character",
                              V2 = "character"))

names(deaths) = c("codmun6", "date", "type", "cause",
                  "age", "puerp", "mother_educ", "mother_age", "race")

deaths$date = as.Date(as.character(deaths$date), format = "%d%m%Y")
deaths$month = month(deaths$date)
deaths$year = year(deaths$date)

# Add microregion codes
deaths = left_join(microregions, deaths, by = "codmun6") 

deaths20 = deaths %>% filter(year == 2020)

# Fetal deaths
fetal = fread("fetaldeathdata.csv", 
              colClasses = c(V1 = "character",
                             V2 = "character")) 

names(fetal) = c("codmun6", "date", "bweeks", "delivery", "mother_educ",
                 "mother_age", "kids")

# For 2021 data we need to filter the total deaths dataset for fetal deaths
fetal21 = fread("do2021opendata.csv",
                colClasses = c(CODMUNRES = "character",
                               DTOBITO = "character")) %>% 
          filter(TIPOBITO == 1) %>% 
          select(CODMUNRES, DTOBITO, SEMAGESTAC, OBITOPARTO,
                 ESCMAE2010, IDADEMAE, QTDFILVIVO)
  
names(fetal21) = c("codmun6", "date", "bweeks", "delivery", "mother_educ",
                   "mother_age", "kids")

# Join both
fetal = rbind(fetal, fetal21)

fetal = fetal %>% filter(!(is.na(bweeks))) #  24,951 rows (9%)
fetal = fetal %>% filter(!(is.na(mother_educ))) # 22,622 rows (9%)
fetal = fetal %>% filter(!(is.na(mother_age))) #  4,776 rows (2%)

# Calculate conception date, month and year
fetal$date = as.Date(as.character(fetal$date), format = "%d%m%Y")
fetal$conc_date = fetal$date - fetal$bweeks * 7
fetal$month = month(fetal$conc_date)
fetal$year = year(fetal$conc_date)

# Add microregion codes
fetal = left_join(microregions, fetal, by = "codmun6") 

fetal20 = fetal %>% filter(year == 2020)

# Isolation
isolation = fread("isolamento_fev-jun.csv", encoding = "UTF-8")

# Use new IBGE dataset to reconcile city names
ibge2 = read_excel("pop.xlsx", skip = 1, sheet = 2)
ibge2$`COD. UF` = as.character(ibge2$`COD. UF`)
ibge2 = ibge2 %>% unite(codmun, "COD. UF", "COD. MUNIC", sep = "", 
                      remove = TRUE) %>% 
        rename(name = `NOME DO MUNICÍPIO`,
              popm = `POPULAÇÃO ESTIMADA`)
ibge2$popm = gsub("\\(.*", "", ibge2$popm)
ibge2$popm = gsub(" ", "", ibge2$popm)
ibge2$popm = as.numeric(ibge2$popm)
ibge2$codmun = as.numeric(ibge2$codmun)
ibge2$codmun6 = as.character(floor(ibge2$codmun / 10))

# State names in the isolation dataset are not acronymized
ibge2 = ibge2 %>% 
        mutate(state_name = 
               case_when(
                UF == "AC" ~ "Acre",					
                UF == "AL" ~ "Alagoas",				
                UF == "AM" ~ "Amazonas",				
                UF == "AP" ~ "Amapá",					
                UF == "BA" ~ "Bahia",					
                UF == "CE" ~ "Ceará",					
                UF == "DF" ~ "Distrito Federal",		
                UF == "ES" ~ "Espírito Santo",		
                UF == "GO" ~ "Goiás",					
                UF == "MA" ~ "Maranhão",				
                UF == "MG" ~ "Minas Gerais",			
                UF == "MS" ~ "Mato Grosso do Sul",	
                UF == "MT" ~ "Mato Grosso",			
                UF == "PA" ~ "Pará",					
                UF == "PB" ~ "Paraíba",				
                UF == "PE" ~ "Pernambuco",			
                UF == "PI" ~ "Piauí",					
                UF == "PR" ~ "Paraná",				
                UF == "RJ" ~ "Rio de Janeiro",		
                UF == "RN" ~ "Rio Grande do Norte",	
                UF == "RO" ~ "Rondônia",				
                UF == "RR" ~ "Roraima",				
                UF == "RS" ~ "Rio Grande do Sul",		
                UF == "SC" ~ "Santa Catarina",		
                UF == "SE" ~ "Sergipe",				
                UF == "SP" ~ "São Paulo",				
                UF == "TO" ~ "Tocantins"))
#We need to add municipality codes to the isolation dataset
# But names are spelled out differently between the IBGE dataframe and
# The isolation dataset, so we make some adjustments

ibge2$name = gsub("D'O", "d'O", ibge2$name)
ibge2$name = gsub("D'A", "d'A", ibge2$name)
ibge2$name = gsub("Januário Cicco", "Boa Saúde", ibge2$name)
ibge2$name = chartr("éêóôãâáõíúz", "eeooaaaoius", ibge2$name)
ibge2$name = gsub("-", " ", ibge2$name)

isolation$city_name = gsub("-", " ", isolation$city_name)
isolation$city_name = gsub("‎", "", isolation$city_name)
isolation$city_name = chartr("éêóôãâáõíúz", "eeooaaaoius", isolation$city_name)

ibge2$name = recode(ibge2$name, 
                   "Muquem do Sao Francisco" = "Muquem de Sao Francisco",
                   "Lajedo do Tabocal" = "Lagedo do Tabocal",
                   "Santo Antonio do Leverger" = "Santo Antonio de Leverger",
                   "Eldorado do Carajas" = "Eldorado dos Carajas",
                   "Munhos de Melo" = "Munhos de Mello",
                   "Sao Caitano" = "Sao Caetano",
                   "Açu" = "Assu",
                   "Sant'Ana do Livramento" = "Santana do Livramento",
                   "Gracho Cardoso" = "Graccho Cardoso",
                   "Santa Lusia do Itanhy" = "Santa Lusia do Itanhi",
                   "Amparo do Sao Francisco" = "Amparo de Sao Francisco",
                   "Tabocao" = "Fortalesa do Tabocao",
                   "Sao Valerio" = "Sao Valerio da Natividade")

# Join with IBGE dataset to get city codes
isolation = left_join(isolation, ibge2, by = c("city_name" = "name",
                                              "state_name"))

# Add microregion codes
isolation = left_join(isolation, microregions, by = "codmun6")

###########################################################################

# Step 2: Group data - Weekly 2020 data by municipality

# Calculate weeks from Monday Dec 30th 2019
# Note that we are considering conception date
births20$week = as.numeric(floor((births20$conc_date - as.Date("2019-12-30"))/7))

# Group by municipality and week, dividing age and education categories
# We use the "complete" function to add 0's where there are no observations
births_mun = births20 %>% group_by(codmun6, week) %>% 
             summarise(nconcep = n(),
                       nconcep_educ_low = sum(mother_educ < 3),
                       nconcep_educ_high = sum(mother_educ >= 3),
                       nconcep_age_young = sum(mother_age < 25),
                       nconcep_age_old = sum(mother_age >= 25),
                       nconcep_kids_no = sum(kids == 0, na.rm = T),
                       nconcep_kids_more = sum(kids > 0, na.rm = T)) %>% 
             ungroup() %>% 
             complete(codmun6, week, fill = list(nconcep = 0,
                                                 nconcep_educ_low = 0,
                                                 nconcep_educ_high = 0,
                                                 nconcep_age_young = 0,
                                                 nconcep_age_old = 0,
                                                 nconcep_kids_no = 0,
                                                 nconcep_kids_more = 0)) %>% 
             arrange(codmun6, week)

# Join with fetal deaths data
fetal20$week = as.numeric(floor((fetal20$conc_date - as.Date("2019-12-30"))/7))

# Group by municipality and week
fetal_mun = fetal20 %>% group_by(codmun6, week) %>% 
            summarise(nfetal = n(),
                      nfetal_educ_low = sum(mother_educ < 3),
                      nfetal_educ_high = sum(mother_educ >= 3),
                      nfetal_age_young = sum(mother_age < 25),
                      nfetal_age_old = sum(mother_age >= 25),
                      nfetal_kids_no = sum(kids == 0, na.rm = T),
                      nfetal_kids_more = sum(kids > 0, na.rm = T)) %>% 
            ungroup() %>% 
            complete(codmun6, week, fill = list(nfetal = 0,
                                                nfetal_educ_low = 0,
                                                nfetal_educ_high = 0,
                                                nfetal_age_young = 0,
                                                nfetal_age_old = 0,
                                                nfetal_kids_no = 0,
                                                nfetal_kids_more = 0)) %>% 
            arrange(codmun6, week)

# Join fetal deaths and live births data
births_mun = left_join(births_mun, fetal_mun, by = c("codmun6", "week"))

# Replace NA's with 0 and calculate total conceptions
births_mun$nfetal[is.na(births_mun$nfetal)] = 0
births_mun$nfetal_educ_low[is.na(births_mun$nfetal_educ_low)] = 0
births_mun$nfetal_educ_high[is.na(births_mun$nfetal_educ_high)] = 0
births_mun$nfetal_age_young[is.na(births_mun$nfetal_age_young)] = 0
births_mun$nfetal_age_old[is.na(births_mun$nfetal_age_old)] = 0
births_mun$nfetal_kids_no[is.na(births_mun$nfetal_kids_no)] = 0
births_mun$nfetal_kids_more[is.na(births_mun$nfetal_kids_more)] = 0

births_mun$totalconcep = births_mun$nconcep + births_mun$nfetal
births_mun$totalconcep_educ_low = births_mun$nconcep_educ_low + births_mun$nfetal_educ_low
births_mun$totalconcep_educ_high = births_mun$nconcep_educ_high + births_mun$nfetal_educ_high
births_mun$totalconcep_age_young = births_mun$nconcep_age_young + births_mun$nfetal_age_young
births_mun$totalconcep_age_old = births_mun$nconcep_age_old + births_mun$nfetal_age_old
births_mun$totalconcep_kids_no = births_mun$nconcep_kids_no + births_mun$nfetal_kids_no
births_mun$totalconcep_kids_more = births_mun$nconcep_kids_more + births_mun$nfetal_kids_more

# Add municipality information
data_mun = left_join(ibge, births_mun, by = c("codmun6"))

# Now we group total deaths
deaths20$week = as.numeric(floor((deaths20$date - as.Date("2019-12-30"))/7))

deaths_mun = deaths20 %>% 
             group_by(codmun6, week) %>% 
             summarise(deaths = n()) %>% 
             ungroup() %>% 
             complete(codmun6, week, fill = list(deaths = 0)) %>% 
             arrange(codmun6, week) 

# Same with isolation
isolation$week = as.numeric(floor((as.Date(isolation$dt) - as.Date("2019-12-30"))/7))

# We filter n=180 to remove municipalities that do not have info on all days
# Then calculate mean isolation by week
isolation_mun = isolation %>% 
                group_by(codmun6) %>%
                filter(n() == 180) %>% ungroup() %>% 
                group_by(week, codmun6) %>% 
                summarise(isolated = sum(isolated)/n(),
                          n = n())

# Join births and deaths data
data_mun = left_join(data_mun, deaths_mun, by = c("codmun6", "week")) %>% 
           ungroup()

# Add log GDP and log population
data_mun$lgdp = log(data_mun$gdpc)
data_mun$lpop = log(data_mun$popm)

# Remove these again
data_mun = data_mun %>% filter(microregion != 26019) # Fernando de Noronha
data_mun = data_mun %>% filter(microregion != 27007 ) # Traipu - AL          
data_mun = data_mun %>% filter(microregion != 35006 ) # Auriflama - SP         

# Join with isolation data
data_mun = left_join(data_mun, isolation_mun, by = c("codmun6", "week")) 

# Calculate the first day of each week
data_mun$s_date = as.Date("2019-12-30") + 7*(data_mun$week)
data_mun$year = year(data_mun$s_date)
data_mun$month = month(data_mun$s_date)

# Delete municipalities with no isolation data at all
data_mun = data_mun %>% group_by(codmun6) %>% 
           filter(sum(isolated, na.rm = TRUE) != 0) %>% ungroup()

# Keep relevant weeks
data_mun = data_mun %>% filter(between(week, 5, 21))

# Save
write.csv(data_mun, "data_2020_mun.csv")
write.dta(data_mun, "data_2020_mun.dta")

###########################################################################

# Step 3:  Weekly 2020 data by microregion

# Same steps as above, but grouping by microregion instead 
births20$week = as.numeric(floor((births20$conc_date - as.Date("2019-12-30"))/7))

births_mic = births20 %>% group_by(microregion, week) %>% 
             summarise(nconcep = n(),
                       nconcep_educ_low = sum(mother_educ < 3),
                       nconcep_educ_high = sum(mother_educ >= 3),
                       nconcep_age_young = sum(mother_age < 25),
                       nconcep_age_old = sum(mother_age >= 25)) %>% 
             ungroup() %>% 
             complete(microregion, week, fill = list(nconcep = 0,
                                                     nconcep_educ_low = 0,
                                                     nconcep_educ_high = 0,
                                                     nconcep_age_young = 0,
                                                     nconcep_age_old = 0)
                      ) %>% 
             arrange(microregion, week)

# Fetal deaths 
fetal20$week = as.numeric(floor((fetal20$conc_date - as.Date("2019-12-30"))/7))

# Group by municipality and week
fetal_mic = fetal20 %>% group_by(microregion, week) %>% 
            summarise(nfetal = n(),
                      nfetal_educ_low = sum(mother_educ < 3),
                      nfetal_educ_high = sum(mother_educ >= 3),
                      nfetal_age_young = sum(mother_age < 25),
                      nfetal_age_old = sum(mother_age >= 25)) %>% 
            ungroup() %>% 
            complete(microregion, week, fill = list(nfetal = 0,
                                                    nfetal_educ_low = 0,
                                                    nfetal_educ_high = 0,
                                                    nfetal_age_young = 0,
                                                    nfetal_age_old = 0)) %>% 
            arrange(microregion, week)

# Join fetal deaths and live births data
births_mic = left_join(births_mic, fetal_mic, by = c("microregion", "week"))

# Replace NA's with 0 and calculate total conceptions
births_mic$nfetal[is.na(births_mic$nfetal)] = 0
births_mic$nfetal_educ_low[is.na(births_mic$nfetal_educ_low)] = 0
births_mic$nfetal_educ_high[is.na(births_mic$nfetal_educ_high)] = 0
births_mic$nfetal_age_young[is.na(births_mic$nfetal_age_young)] = 0
births_mic$nfetal_age_old[is.na(births_mic$nfetal_age_old)] = 0

births_mic$totalconcep = births_mic$nconcep + births_mic$nfetal
births_mic$totalconcep_educ_low = births_mic$nconcep_educ_low + births_mic$nfetal_educ_low
births_mic$totalconcep_educ_high = births_mic$nconcep_educ_high + births_mic$nfetal_educ_high
births_mic$totalconcep_age_young = births_mic$nconcep_age_young + births_mic$nfetal_age_young
births_mic$totalconcep_age_old = births_mic$nconcep_age_old + births_mic$nfetal_age_old

data_mic = left_join(ibge_mic, births_mic, by = c("microregion"))

deaths20$week = as.numeric(floor((deaths20$date - as.Date("2019-12-30"))/7))

deaths_mic = deaths20 %>% 
             group_by(microregion, week) %>% 
             summarise(deaths = n()) %>% 
             ungroup() %>% 
             complete(microregion, week, fill = list(deaths = 0)) %>% 
             arrange(microregion, week) 

isolation$week = as.numeric(floor((as.Date(isolation$dt) - as.Date("2019-12-30"))/7))

# For isolation, we calculate weighted means by municipality population 
isolation_mic = isolation %>% 
                group_by(codmun6) %>% 
                filter(n() == 180) %>% ungroup() %>% 
                group_by(week, microregion, codmun6) %>% 
                summarise(isolated = sum(isolated)/n(),
                          popm = mean(popm)) %>% ungroup() %>% 
                group_by(microregion, week) %>% 
                summarise(isolated = weighted.mean(isolated, popm))
            
data_mic = left_join(data_mic, deaths_mic, by = c("microregion", "week")) %>% 
           ungroup()

data_mic$lgdp = log(data_mic$gdp)
data_mic$lpop = log(data_mic$pop)

data_mic = data_mic %>% filter(microregion != 26019) # Fernando de Noronha
data_mic = data_mic %>% filter(microregion != 27007 ) # Traipu - AL          
data_mic = data_mic %>% filter(microregion != 35006 ) # Auriflama - SP         

data_mic = left_join(data_mic, isolation_mic, by = c("microregion", "week")) 

data_mic$s_date = as.Date("2019-12-30") + 7*(data_mic$week)
data_mic$year = year(data_mic$s_date)
data_mic$month = month(data_mic$s_date)

data_mic = data_mic %>% group_by(microregion) %>% 
           filter(sum(isolated, na.rm = TRUE) != 0) 

data_mic = data_mic %>% filter(between(week, 5, 21))

# Save
write.csv(data_mic, "data_2020_mic.csv")

###########################################################################

# Step 4:  Monthly 2020 data by municipality

# Similar steps, but we group by month
births_all = births20 %>% 
             group_by(codmun6, month) %>% 
             summarise(nconcep = n(),
                       nconcep_educ_low = sum(mother_educ < 3),
                       nconcep_educ_high = sum(mother_educ >= 3),
                       nconcep_age_young = sum(mother_age < 25),
                       nconcep_age_old = sum(mother_age >= 25)) %>% 
             ungroup() %>% 
             complete(codmun6, month, fill = list(nconcep = 0,
                                                  nconcep_educ_low = 0,
                                                  nconcep_educ_high = 0,
                                                  nconcep_age_young = 0,
                                                  nconcep_age_old = 0)) %>% 
             arrange(codmun6, month)


fetal_all = fetal20 %>% group_by(codmun6, month) %>% 
            summarise(nfetal = n(),
                      nfetal_educ_low = sum(mother_educ < 3),
                      nfetal_educ_high = sum(mother_educ >= 3),
                      nfetal_age_young = sum(mother_age < 25),
                      nfetal_age_old = sum(mother_age >= 25)) %>% 
            ungroup() %>% 
            complete(codmun6, month, fill = list(nfetal = 0,
                                                nfetal_educ_low = 0,
                                                nfetal_educ_high = 0,
                                                nfetal_age_young = 0,
                                                nfetal_age_old = 0)) %>% 
            arrange(codmun6, month)

# Join fetal deaths and live births data
births_all = left_join(births_all, fetal_all, by = c("codmun6", "month"))

# Replace NA's with 0 and calculate total conceptions
births_all$nfetal[is.na(births_all$nfetal)] = 0
births_all$nfetal_educ_low[is.na(births_all$nfetal_educ_low)] = 0
births_all$nfetal_educ_high[is.na(births_all$nfetal_educ_high)] = 0
births_all$nfetal_age_young[is.na(births_all$nfetal_age_young)] = 0
births_all$nfetal_age_old[is.na(births_all$nfetal_age_old)] = 0

births_all$totalconcep = births_all$nconcep + births_all$nfetal
births_all$totalconcep_educ_low = births_all$nconcep_educ_low + births_all$nfetal_educ_low
births_all$totalconcep_educ_high = births_all$nconcep_educ_high + births_all$nfetal_educ_high
births_all$totalconcep_age_young = births_all$nconcep_age_young + births_all$nfetal_age_young
births_all$totalconcep_age_old = births_all$nconcep_age_old + births_all$nfetal_age_old

data_all = left_join(ibge, births_all, by = c("codmun6"))

deaths_all = deaths20 %>% 
             group_by(codmun6, month) %>% 
             summarise(deaths = n()) %>% 
             ungroup() %>% 
             complete(codmun6, month, fill = list(deaths = 0)) %>% 
             arrange(codmun6,  month) 

isolation$month = month(isolation$dt)

isolation_all = isolation %>% 
                group_by(codmun6) %>%
                filter(n() == 180) %>% ungroup() %>% 
                group_by(codmun6, month) %>% 
                summarise(isolated = sum(isolated)/n(),
                          n = n())

data_all = left_join(data_all, deaths_all, by = c("codmun6", "month")) %>% 
           ungroup()

data_all$lgdp = log(data_all$gdpc)
data_all$lpop = log(data_all$popm)

data_all = data_all %>% filter(microregion != 26019) # Fernando de Noronha
data_all = data_all %>% filter(microregion != 27007 ) # Traipu - AL          
data_all = data_all %>% filter(microregion != 35006 ) # Auriflama - SP         

data_all = left_join(data_all, isolation_all, by = c("codmun6", "month")) 

data_all = data_all %>% group_by(codmun6) %>% 
           filter(sum(isolated, na.rm = TRUE) != 0) 

data_all = data_all %>% filter(between(month, 2, 5))

# Save
write.csv(data_all, "data_monthly_mun.csv")

###########################################################################

# Step 5: Weekly conception data by municipality for other years
births_yr = births %>% ungroup()

births_yr = births_yr %>% 
            mutate(week = 
                     case_when(
                       conc_date >= as.Date("2019-12-30") ~ 
                         as.numeric(floor((births_yr$conc_date - 
                                             as.Date("2019-12-30"))/7)),
                       conc_date >= as.Date("2018-12-30") & 
                         conc_date < as.Date("2019-12-30") ~
                         as.numeric(floor((births_yr$conc_date - 
                                             as.Date("2018-12-30"))/7)),
                       conc_date >= as.Date("2017-12-30") & 
                         conc_date < as.Date("2018-12-30") ~
                         as.numeric(floor((births_yr$conc_date - 
                                             as.Date("2017-12-30"))/7)),
                       conc_date >= as.Date("2016-12-30") & 
                         conc_date < as.Date("2017-12-30") ~
                         as.numeric(floor((births_yr$conc_date - 
                                             as.Date("2016-12-30"))/7)),
                       conc_date >= as.Date("2015-12-30") & 
                         conc_date < as.Date("2016-12-30") ~
                         as.numeric(floor((births_yr$conc_date - 
                                             as.Date("2015-12-30"))/7)),
                       conc_date >= as.Date("2014-12-30") & 
                         conc_date < as.Date("2015-12-30") ~
                         as.numeric(floor((births_yr$conc_date - 
                                             as.Date("2014-12-30"))/7)),
                       conc_date >= as.Date("2013-12-30") & 
                         conc_date < as.Date("2014-12-30") ~
                         as.numeric(floor((births_yr$conc_date - 
                                             as.Date("2013-12-30"))/7)),
                       conc_date >= as.Date("2012-12-30") & 
                         conc_date < as.Date("2013-12-30") ~
                         as.numeric(floor((births_yr$conc_date - 
                                             as.Date("2012-12-30"))/7)),
                       conc_date >= as.Date("2011-12-30") & 
                         conc_date < as.Date("2012-12-30") ~
                         as.numeric(floor((births_yr$conc_date - 
                                             as.Date("2011-12-30"))/7))))

births_yr = births_yr %>% filter(year > 2011) %>% 
            filter(between(week, 5, 21)) %>% 
            group_by(codmun6, year, week) %>% 
            summarise(nconcep = n()) %>% 
            ungroup()

fetal_yr = fetal %>% ungroup()

fetal_yr = fetal_yr %>% 
           mutate(week = 
                    case_when(
                      conc_date >= as.Date("2019-12-30") ~ 
                        as.numeric(floor((fetal_yr$conc_date - 
                                            as.Date("2019-12-30"))/7)),
                      conc_date >= as.Date("2018-12-30") & 
                        conc_date < as.Date("2019-12-30") ~
                        as.numeric(floor((fetal_yr$conc_date - 
                                            as.Date("2018-12-30"))/7)),
                      conc_date >= as.Date("2017-12-30") & 
                        conc_date < as.Date("2018-12-30") ~
                        as.numeric(floor((fetal_yr$conc_date - 
                                            as.Date("2017-12-30"))/7)),
                      conc_date >= as.Date("2016-12-30") & 
                        conc_date < as.Date("2017-12-30") ~
                        as.numeric(floor((fetal_yr$conc_date - 
                                            as.Date("2016-12-30"))/7)),
                      conc_date >= as.Date("2015-12-30") & 
                        conc_date < as.Date("2016-12-30") ~
                        as.numeric(floor((fetal_yr$conc_date - 
                                            as.Date("2015-12-30"))/7)),
                      conc_date >= as.Date("2014-12-30") & 
                        conc_date < as.Date("2015-12-30") ~
                        as.numeric(floor((fetal_yr$conc_date - 
                                            as.Date("2014-12-30"))/7)),
                      conc_date >= as.Date("2013-12-30") & 
                        conc_date < as.Date("2014-12-30") ~
                        as.numeric(floor((fetal_yr$conc_date - 
                                            as.Date("2013-12-30"))/7)),
                      conc_date >= as.Date("2012-12-30") & 
                        conc_date < as.Date("2013-12-30") ~
                        as.numeric(floor((fetal_yr$conc_date - 
                                            as.Date("2012-12-30"))/7)),
                      conc_date >= as.Date("2011-12-30") & 
                        conc_date < as.Date("2012-12-30") ~
                        as.numeric(floor((fetal_yr$conc_date - 
                                            as.Date("2011-12-30"))/7))))

fetal_yr = fetal_yr %>% filter(year > 2011) %>% 
           filter(between(week, 5, 21)) %>% 
           group_by(codmun6, year, week) %>% 
           summarise(nfetal = n()) %>% 
           ungroup()
 
 # Join fetal deaths and live births data
 births_yr = left_join(births_yr, fetal_yr, by = c("codmun6", "year", "week"))
 
 # Replace NA's with 0 and calculate total conceptions
 births_yr$nfetal[is.na(births_yr$nfetal)] = 0
 births_yr$totalconcep = births_yr$nconcep + births_yr$nfetal

 # Save
 write.csv(births_yr, "concep_week_mun.csv")
 
 #######################################################################

# Step 6: Weekly 2020 data by municipality grouping by different weekdays

# We'll do pretty much the same as we did on Step 2, but grouping by other days
weekdays_df = list()

# Loop over several weekdays Starting Monday Dec 30th 2019
for (i in 0:6) {
      day = as.Date("2019-12-30") + i 
      
      # We'll work on a copy of the original dataset
      births20w = births20
      fetal20w = fetal20
      deaths20w = deaths20
      isolationw = isolation
      
      births20w$week = as.numeric(floor((births20w$conc_date - as.Date(day))/7))
      
      births_munw = births20w %>% group_by(codmun6, week) %>% 
                    summarise(nconcep = n()) %>% 
                    ungroup() %>% 
                    complete(codmun6, week, fill = list(nconcep = 0)) %>% 
                    arrange(codmun6, week)
      
      fetal20w$week = as.numeric(floor((fetal20w$conc_date - as.Date(day))/7))
      
        fetal_munw = fetal20 %>% group_by(codmun6, week) %>% 
        summarise(nfetal = n()) %>% 
        ungroup() %>% 
        complete(codmun6, week, fill = list(nfetal = 0)) %>% 
        arrange(codmun6, week)
            
      births_munw = left_join(births_munw, fetal_munw, by = c("codmun6", "week"))
      
      births_munw$nfetal[is.na(births_munw$nfetal)] = 0
      births_munw$totalconcep = births_munw$nconcep + births_munw$nfetal
     
      data_munw = left_join(ibge, births_munw, by = c("codmun6"))
      
      deaths20w$week = as.numeric(floor((deaths20w$date - as.Date(day))/7))
      
      deaths_munw = deaths20w %>% 
                    group_by(codmun6, week) %>% 
                    summarise(deaths = n()) %>% 
                    ungroup() %>% 
                    complete(codmun6, week, fill = list(deaths = 0)) %>% 
                    arrange(codmun6, week) 
                  
      isolationw$week = as.numeric(floor((as.Date(isolationw$dt) - as.Date(day))/7))
      
      isolation_munw = isolationw %>% 
                       group_by(codmun6) %>%
                       filter(n() == 180) %>% ungroup() %>% 
                       group_by(week, codmun6) %>% 
                       summarise(isolated = sum(isolated)/n(),
                                 n = n())
      
      data_munw = left_join(data_munw, deaths_munw, by = c("codmun6", "week")) %>% 
                  ungroup()
      
      data_munw$lgdp = log(data_munw$gdpc)
      data_munw$lpop = log(data_munw$popm)
      
      data_munw = data_munw %>% filter(microregion != 26019) # Fernando de Noronha
      data_munw = data_munw %>% filter(microregion != 27007 ) # Traipu - AL          
      data_munw = data_munw %>% filter(microregion != 35006 ) # Auriflama - SP         
      
      data_munw = left_join(data_munw, isolation_munw, by = c("codmun6", "week")) 
      
      # Calculate the first day of each week
      data_munw$s_date = as.Date(day) + 7*(data_munw$week)
      data_munw$year = year(data_munw$s_date)
      data_munw$month = month(data_munw$s_date)
      
      data_munw = data_munw %>% group_by(codmun6) %>% 
                  filter(sum(isolated, na.rm = TRUE) != 0) %>% ungroup()
      
      data_munw = data_munw %>% filter(between(week, 5, 21))
      
      # Add weekday label
      data_munw$weekday = weekdays(data_munw$s_date)
      
      # Save in loop
      weekdays_df[[i + 1]] = data_munw

}

weekdays_df = weekdays_df %>% bind_rows()

# Save
write.csv(weekdays_df, "data_weekdays.csv")

#######################################################################

# Step 7: Total conceptions in Brazil by month (for chart)

# We'll group data by year and month and calculate total conceptions
births_br = births %>% filter(year > 2011) %>% 
            filter(between(month, 1, 5)) %>% 
            group_by(year, month) %>% 
            summarise(nconcep = n(),
                      nconcep_educ_low = sum(mother_educ < 3),
                      nconcep_educ_high = sum(mother_educ >= 3),
                      nconcep_age_young = sum(mother_age < 25),
                      nconcep_age_old = sum(mother_age >= 25)) %>% 
            ungroup() 

fetal_br = fetal %>% filter(year > 2011) %>% 
           filter(between(month, 1, 5)) %>% 
           group_by(year, month) %>% 
           summarise(nfetal = n(),
                     nfetal_educ_low = sum(mother_educ < 3),
                     nfetal_educ_high = sum(mother_educ >= 3),
                     nfetal_age_young = sum(mother_age < 25),
                     nfetal_age_old = sum(mother_age >= 25)) %>% 
           ungroup() 

# Join fetal deaths and live births data
births_br = left_join(births_br, fetal_br, by = c("year", "month"))

# Replace NA's with 0 and calculate total conceptions
births_br$nfetal[is.na(births_br$nfetal)] = 0
births_br$totalconcep = births_br$nconcep + births_br$nfetal

births_br$nfetal_educ_low[is.na(births_br$nfetal_educ_low)] = 0
births_br$nfetal_educ_high[is.na(births_br$nfetal_educ_high)] = 0

births_br$totalconcep_educ_low = births_br$nconcep_educ_low + births_br$nfetal_educ_low
births_br$totalconcep_educ_high = births_br$nconcep_educ_high + births_br$nfetal_educ_high

births_br$nfetal_age_young[is.na(births_br$nfetal_age_young)] = 0
births_br$nfetal_age_old[is.na(births_br$nfetal_age_old)] = 0

births_br$totalconcep_age_young = births_br$nconcep_age_young + births_br$nfetal_age_young
births_br$totalconcep_age_old = births_br$nconcep_age_old + births_br$nfetal_age_old

# Save
write.csv(births_br, "data_births_br.csv")

#######################################################################

# Step 8: Total conceptions by State and month (for maps)

# Add municipality information
births_uf = left_join(ibge, births, by = c("codmun6", "microregion"))

fetal_uf = left_join(ibge, fetal, by = c("codmun6", "microregion"))

# Group by state, year and month
births_uf = births_uf %>% filter(year > 2017) %>% 
            filter(between(month, 1, 5)) %>% 
            group_by(year, month, UF) %>% 
            summarise(nconcep = n(),
                      nconcep_educ_low = sum(mother_educ < 3),
                      nconcep_educ_high = sum(mother_educ >= 3),
                      nconcep_age_young = sum(mother_age < 25),
                      nconcep_age_old = sum(mother_age >= 25)) %>% 
            ungroup() 
          
fetal_uf = fetal_uf %>% filter(year > 2017) %>% 
           filter(between(month, 1, 5)) %>% 
           group_by(year, month, UF) %>% 
           summarise(nfetal = n(),
                     nfetal_educ_low = sum(mother_educ < 3),
                     nfetal_educ_high = sum(mother_educ >= 3),
                     nfetal_age_young = sum(mother_age < 25),
                     nfetal_age_old = sum(mother_age >= 25)) %>% 
           ungroup() 

# Join fetal deaths and live births data
births_uf = left_join(births_uf, fetal_uf, by = c("year", "month", "UF"))

# Replace NA's with 0 and calculate total conceptions
births_uf$nfetal[is.na(births_uf$nfetal)] = 0
births_uf$totalconcep = births_uf$nconcep + births_uf$nfetal

births_uf$nfetal_educ_low[is.na(births_uf$nfetal_educ_low)] = 0
births_uf$nfetal_educ_high[is.na(births_uf$nfetal_educ_high)] = 0

births_uf$totalconcep_educ_low = births_uf$nconcep_educ_low + births_uf$nfetal_educ_low
births_uf$totalconcep_educ_high = births_uf$nconcep_educ_high + births_uf$nfetal_educ_high

births_uf$nfetal_age_young[is.na(births_uf$nfetal_age_young)] = 0
births_uf$nfetal_age_old[is.na(births_uf$nfetal_age_old)] = 0

births_uf$totalconcep_age_young = births_uf$nconcep_age_young + births_uf$nfetal_age_young
births_uf$totalconcep_age_old = births_uf$nconcep_age_old + births_uf$nfetal_age_old

# Save
write.csv(births_uf, "data_births_uf.csv")

#######################################################################
