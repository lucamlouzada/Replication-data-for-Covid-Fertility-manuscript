#  append_data.R

# Author: Luca Moreno-Louzada
# 2023
# Code for manuscript:
# "The relationship between staying at home during the pandemic and the number of conceptions: a national panel data analysis"
# Luca Moreno-Louzada and Naercio Menezes-Filho

# This code:
# > Opens .dbc files and appends them, converting into .csv

###########################################################################

# Step 0: Cleaning workplace and loading libraries

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

# Step 1: Read and append all files to a single .csv file for each data source
# https://datasus.saude.gov.br/transferencia-de-arquivos/#

# Births
data_files = list.files("./SINASC", full.names = TRUE)

for (i in 1:length(data_files)){
 
 path = data_files[i]
 
 temp = read.dbc(path) %>% select(CODMUNRES, DTNASC, SEXO, ESTCIVMAE,
                                  ESCMAE2010, QTDFILVIVO, GRAVIDEZ,
                                  PARTO, CONSULTAS, RACACOR, IDADEMAE,
                                  SEMAGESTAC, PESO)

 cat("saving", i, "out of", length(data_files), "| ") # update loop status
 
 write_csv(temp, file = "birthdata.csv", append = TRUE)
 
}

# Fetal deaths
data_files = list.files("./SIM_DOFET", full.names = TRUE)

for (i in 1:length(data_files)){
  
  path = data_files[i]
  
  temp = read.dbc(path) %>% select(CODMUNRES, DTOBITO, SEMAGESTAC,
                                   OBITOPARTO, ESCMAE2010, IDADEMAE, QTDFILVIVO)
  
  cat("saving", i, "out of", length(data_files), "| ") # update loop status
  
  write_csv(temp, file = "fetaldeathdata.csv", append = TRUE)
  
}

# Total deaths
data_files = list.files("./SIM", full.names = TRUE)

for (i in 1:length(data_files)){
  
  path = data_files[i]
  
  temp = read.dbc(path) %>% select(CODMUNRES, DTOBITO, TIPOBITO,
                                   CAUSABAS, IDADE, OBITOPUERP, ESCMAE2010,
                                   IDADEMAE, RACACOR)
  
  cat("saving", i, "out of", length(data_files), "| ") # update loop status
  
  write_csv(temp, file = "deathdata.csv", append = TRUE)
  
}


# 2021 Births
df_21 = read.dbf("DN21OPEN.dbf") %>%  select(CODMUNRES, DTNASC, SEXO, ESTCIVMAE,
                                     ESCMAE2010, QTDFILVIVO, GRAVIDEZ,
                                     PARTO, CONSULTAS, RACACOR, IDADEMAE,
                                     SEMAGESTAC, PESO)

write_csv(df_21, file = "births2021_updated.csv")

# 2012-2014 births
data_files = list.files("./SINASC_11_14", full.names = TRUE)

# We won't use 2011
data_files = data_files[!grepl("2011", data_files)]

for (i in 1:length(data_files)){
  
  path = data_files[i]
  
  temp = read.dbc(path) %>% select(CODMUNRES, DTNASC, SEXO, ESTCIVMAE,
                                   ESCMAE, QTDFILVIVO, GRAVIDEZ,
                                   PARTO, CONSULTAS, RACACOR, IDADEMAE,
                                   SEMAGESTAC, PESO)
  
  cat("saving", i, "out of", length(data_files), "| ") # update loop status
  
  write_csv(temp, file = "birthdata_12_14.csv", append = TRUE)
  
}

###########################################################################
