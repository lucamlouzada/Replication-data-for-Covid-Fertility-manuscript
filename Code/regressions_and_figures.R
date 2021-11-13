# regressions_and_figures.R

# Author: Luca Moreno-Louzada
# 2021
# Code for manuscript:
# "Staying at Home during Covid Outbreaks Leads to Less Conceptions"
# Luca Moreno-Louzada and Naercio Menezes-Filho

# This code:
# > Runs regressions on the effect of isolation on conceptions
# > Prepares plots and tables with regressions results
# > Prepares plots and maps with data on isolation and conceptions

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

# Step 1: Main Regression Table

data_main = fread("data_2020_mun.csv")

fdf = data_main %>%  
      mutate(msm10 = deaths == 0 | totalconcep < 10,
      msm0 = deaths == 0 | totalconcep == 0,
      msm20 = deaths == 0 | totalconcep < 20) %>% 
      group_by(codmun6) %>% 
      mutate(msm10 = mean(msm10),
             msm0 = mean(msm0),
             msm20 = mean(msm20)) %>% 
      ungroup()

fdf = fdf %>% 
      mutate(lm = log(deaths),
             lb = log(totalconcep),
             lbagey = log(totalconcep_age_young),
             lbageo = log(totalconcep_age_old),
             lbled = log(totalconcep_educ_low),
             lbhed = log(totalconcep_educ_high),
             lbkno = log(totalconcep_kids_no),
             lbkye = log(totalconcep_kids_more)
             ) %>% 
      mutate(big = popm > 120000,
             poor = gdpc < 17427) %>% 
      group_by(codmun6) %>% 
      mutate(disol = isolated - lag(isolated, 1),
             dlm = lm - lag(lm, 1),
             dlb = lb - lag(lb, 1),
             dlbagey = lbagey - lag(lbagey, 1),
             dlbageo = lbageo - lag(lbageo, 1),
             dlbled = lbled - lag(lbled, 1),
             dlbhed = lbhed - lag(lbhed, 1),
             dlbkno = lbkno - lag(lbkno, 1),
             dlbkye = lbkye - lag(lbkye, 1),
             dm = deaths - lag(deaths, 1),
             db = totalconcep - lag(totalconcep, 1)) %>%  
     ungroup() %>% 
     mutate_at(vars(dlm, dlb, disol,
                    dlbagey, dlbageo, 
                    dlbled, dlbhed, dlbkno, dlbkye), ~ifelse(is.nan(.), NA, .)) %>% 
     mutate_at(vars(dlm, dlb, disol,
                    dlbagey, dlbageo,
                    dlbled, dlbhed, dlbkno, dlbkye), ~ifelse(is.infinite(.), NA, .)) 

fdf$codmun6 = factor(fdf$codmun6)
fdf$month = factor(fdf$month)
fdf$week = factor(fdf$week)

# First columns
data = filter(fdf, msm10 == 0)
m1 = feols(dlb ~ disol| week, cluster = "codmun6", data = data,
      weights = data$popm) 

data = filter(fdf, msm10 == 0)
m2 = felm(dlb ~ disol + dlm| week| 0 | codmun6, data = data,
          weights = data$popm, cmethod = "reghdfe")

data = filter(fdf, msm10 == 0)
m3 = felm(dlb ~ disol + dlm | week + codmun6| 0 | codmun6, data = data,
          weights = data$popm, cmethod = "reghdfe")

data = filter(fdf, msm10 == 0)
m4 = felm(dlb ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = data,
          weights = data$popm, cmethod = "reghdfe")

# Microregion
data_mic = fread("data_2020_mic.csv")

mdf = data_mic %>%  
      mutate(msm10 = deaths == 0 | totalconcep < 10) %>% 
      group_by(microregion) %>% 
      mutate(msm10 = mean(msm10)) %>% 
      ungroup()

mdf = mdf %>% 
      mutate(lm = log(deaths),
             lb = log(totalconcep),
             lbagey = log(totalconcep_age_young),
             lbageo = log(totalconcep_age_old),
             lbled = log(totalconcep_educ_low),
             lbhed = log(totalconcep_educ_high)
      ) %>% 
      group_by(microregion) %>% 
      mutate(disol = isolated - lag(isolated, 1),
             dlm = lm - lag(lm, 1),
             dlb = lb - lag(lb, 1),
             dlbagey = lbagey - lag(lbagey, 1),
             dlbageo = lbageo - lag(lbageo, 1),
             dlbled = lbled - lag(lbled, 1),
             dlbhed = lbhed - lag(lbhed, 1),
             dm = deaths - lag(deaths, 1),
             db = totalconcep - lag(totalconcep, 1)) %>%  
      ungroup() %>% 
      mutate_at(vars(dlm, dlb, disol,
                     dlbagey, dlbageo, 
                     dlbled, dlbhed), ~ifelse(is.nan(.), NA, .)) %>% 
      mutate_at(vars(dlm, dlb, disol,
                     dlbagey, dlbageo,
                     dlbled, dlbhed), ~ifelse(is.infinite(.), NA, .)) 

mdf$microregion = factor(mdf$microregion)
mdf$month = factor(mdf$month)

data = filter(mdf, msm10 == 0)
m5 = felm(dlb ~ disol + dlm| week + microregion + month + microregion:month| 0 | microregion, data = data,
           weights = data$pop, cmethod = "reghdfe")

# Monthly
data_mon = fread("data_monthly_mun.csv")

ndf = data_mon %>%  
      mutate(msm10 = deaths == 0 | totalconcep < 10) %>% 
      group_by(codmun6) %>% 
      mutate(msm10 = mean(msm10)) %>% 
      ungroup() 

ndf = ndf %>% 
      mutate(lm = log(deaths),
             lb = log(totalconcep),
             lbagey = log(totalconcep_age_young),
             lbageo = log(totalconcep_age_old),
             lbled = log(totalconcep_educ_low),
             lbhed = log(totalconcep_educ_high)
      ) %>% 
      group_by(codmun6) %>% 
      mutate(disol = isolated - lag(isolated, 1),
             dlm = lm - lag(lm, 1),
             dlb = lb - lag(lb, 1),
             dlbagey = lbagey - lag(lbagey, 1),
             dlbageo = lbageo - lag(lbageo, 1),
             dlbled = lbled - lag(lbled, 1),
             dlbhed = lbhed - lag(lbhed, 1),
             dm = deaths - lag(deaths, 1),
             db = totalconcep - lag(totalconcep, 1)) %>%  
      ungroup() %>% 
      mutate_at(vars(dlm, dlb, disol,
                     dlbagey, dlbageo, 
                     dlbled, dlbhed), ~ifelse(is.nan(.), NA, .)) %>% 
      mutate_at(vars(dlm, dlb, disol,
                     dlbagey, dlbageo,
                     dlbled, dlbhed), ~ifelse(is.infinite(.), NA, .)) 

data = filter(ndf, msm10 == 0)
m6 = felm(dlb ~ disol + dlm| codmun6 + microregion| 0 | codmun6, data = data,
           weights = data$popm, cmethod = "reghdfe")

stargazer(m1, m2, m3, m4, m5, m6, type = "html",
          title = "Effect of isolation on conceptions",
          keep.stat = c("n", "rsq"),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Delta ln Conceptions",
          covariate.labels = c("Delta Isolation", "Delta ln Deaths"),
          add.lines = list(c("Two Way Fixed Effects:", "", "", "Y", "Y", "Y", ""),
                           c("Month interactions:", "", "", "", "Y", "Y", ""),
                           c("Group by microregion:", rep("", 4), "Y", ""),
                           c("Group by month:", rep("", 5), "Y")),
          out = "regression_main.doc")

###########################################################################

# Step 2: Heterogeneous effects

# Municipalities
df = filter(fdf, msm10 == 0)

m7 = felm(dlb ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = filter(df, poor == 1), 
          weights = filter(df, poor == 1)$popm, cmethod = "reghdfe")

m8 = felm(dlb ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = filter(df, poor == 0), 
          weights = filter(df, poor == 0)$popm, cmethod = "reghdfe")

m9 = felm(dlb ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = filter(df, big == 1), 
          weights = filter(df, big == 1)$popm, cmethod = "reghdfe")
 
m10 = felm(dlb ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = filter(df, big == 0), 
          weights = filter(df, big == 0)$popm, cmethod = "reghdfe")

m11 = felm(dlb ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = filter(df, urban == 1), 
          weights = filter(df, urban == 1)$popm, cmethod = "reghdfe")

m12 = felm(dlb ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = filter(df, urban  == 0), 
          weights = filter(df, urban == 0)$popm, cmethod = "reghdfe")

stargazer(m7, m8, m9, m10, m11, m12, type = "html",
          title = "Heterogeneous effects",
          keep.stat = c("n", "rsq"),
          dep.var.labels.include = F,
          dep.var.caption = "Delta ln Conceptions",
          column.labels= c("Poorer", "Richer", "Larger", "Smaller", "Urban", "Rural"),
          covariate.labels = c("Delta Isolation", "Delta ln Deaths"),
          out = "regression_mun.doc")


# Women
df = filter(fdf, msm10 == 0)

m13 = felm(dlbled ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
          weights = df$popm, cmethod = "reghdfe")

m14 = felm(dlbhed ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
          weights = df$popm, cmethod = "reghdfe")

m15 = felm(dlbagey ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
          weights = df$popm, cmethod = "reghdfe")

m16 = felm(dlbageo ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

m17 = felm(dlbkye ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

m18 = felm(dlbkno ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

stargazer(m13, m14, m15, m16, m17, m18, type = "html",
          title = "Heterogeneous effects",
          keep.stat = c("n", "rsq"),
          dep.var.labels.include = F,
          dep.var.caption = "Delta ln Conceptions",
          column.labels= c("Less Educ", "More Educ", 
                           "Younger", "Older",
                           "Previous kids", "No kids"),
          covariate.labels = c("Delta Isolation", "Delta ln Deaths"),
          out = "regression_wom.doc")

###########################################################################

# Step 3: Alternative models

df = filter(fdf, msm10 == 0)
# Baseline
m19 = felm(dlb ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

# Month dummies no weight
m20 = felm(dlb ~ disol + dlm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           cmethod = "reghdfe")

# No weight no month dummies
m21 = felm(dlb ~ disol + dlm| week + codmun6| 0 | codmun6, data = df, 
           cmethod = "reghdfe")

# No log
m22 = felm(lb ~ disol + lm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

df = fdf 
# No filter
m23 = felm(dlb ~ disol + dlm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

df = fdf %>% filter(msm0 == 0)
# Filter 0
m24 = felm(dlb ~ disol + dlm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

df = fdf %>% filter(msm20 == 0)
# Filter 20
m25 = felm(dlb ~ disol + dlm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

stargazer(m19, m20, m21, m22, m23, m24, m25,  type = "html",
          title = "Robustness tests",
          keep.stat = c("n", "rsq"),
          dep.var.labels.include = F,
          dep.var.caption = "Delta ln Conceptions",
          column.labels= c("Baseline", "", "", "No log", "No filter", "Filter 0",
                           "Filter 20"),
          covariate.labels = c("Delta Isolation", "Delta ln Deaths", "Delta Deaths"),
          add.lines = list(c("Weighted:", "Y", "", "", "Y", "Y",
                             "Y", "Y"),
                           c("Month dummies:", "Y", "Y", "", "Y","Y" ,
                             "Y","Y")),
          out = "regression_rob.doc")

###########################################################################

# Step 4: Weekdays tests

# We'll run the main regression with the dataset grouped by different weekdays
weekdays = fread("data_weekdays.csv")

fdf = weekdays %>%  
        mutate(msm10 = deaths == 0 | totalconcep < 10) %>% 
        group_by(codmun6, weekday) %>% 
        mutate(msm10 = mean(msm10)) %>% 
        ungroup()

fdf = fdf %>% 
        mutate(lm = log(deaths),
               lb = log(totalconcep)) %>% 
        group_by(codmun6, weekday) %>% 
        mutate(disol = isolated - lag(isolated, 1),
               dlm = lm - lag(lm, 1),
               dlb = lb - lag(lb, 1)) %>% 
        ungroup() %>% 
        mutate_at(vars(dlm, dlb, disol), ~ifelse(is.nan(.), NA, .)) %>% 
        mutate_at(vars(dlm, dlb, disol), ~ifelse(is.infinite(.), NA, .)) 

msmdf = fdf %>% filter(msm10 == 0)

msmdf$month = factor(msmdf$month)
msmdf$codmun6 = factor(msmdf$codmun6)

df = msmdf %>% filter(weekday == "Monday")
m26 =felm(dlb ~ disol + dlm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
          weights = df$popm, cmethod = "reghdfe")

df = msmdf %>% filter(weekday == "Tuesday")
m27 = felm(dlb ~ disol + dlm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

df = msmdf %>% filter(weekday == "Wednesday")
m28 = felm(dlb ~ disol + dlm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

df = msmdf %>% filter(weekday == "Thursday")
m29 = felm(dlb ~ disol + dlm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

df = msmdf %>% filter(weekday == "Friday")
m30 = felm(dlb ~ disol + dlm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

df = msmdf %>% filter(weekday == "Saturday")
m31 = felm(dlb ~ disol + dlm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

df = msmdf %>% filter(weekday == "Sunday")
m32 = felm(dlb ~ disol + dlm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")


stargazer(m26, m27, m28, m29, m30, m31, m32, type = "html",
          title = "Robustness tests",
          keep.stat = c("n", "rsq"),
          dep.var.labels.include = F,
          column.labels= c("Monday", "Tuesday", "Wednesday", "Thursday",
                           "Friday", "Saturday", "Sunday"),
          covariate.labels = c("Delta Isolation", "Delta deaths"),
          out = "regression_week.doc")

###########################################################################

# Step 5: Placebo tests

# We'll join our dataset with conception data for other years
data_years = fread("concep_week_mun.csv") %>% 
             select(codmun6, year, week, totalconcep)

data_join = data_main %>% select(-totalconcep)

tbl = list()

# Loop and run the main specification regression for each year
for (i in 2012:2020) {
        
        data_year = data_years %>% filter(year == i)
        
        data_year$year = 2020
        
        data20 = left_join(data_join, data_year, by = c("codmun6", "year", "week"))
        
        data20 = data20 %>%  
                mutate(msm10 = deaths == 0 | totalconcep < 10) %>% 
                group_by(codmun6) %>% 
                mutate(msm10 = mean(msm10)) %>% 
                ungroup()
        
        data20 = data20 %>% 
                mutate(lm = log(deaths),
                       lb = log(totalconcep)) %>% 
                group_by(codmun6) %>% 
                mutate(disol = isolated - lag(isolated, 1),
                       dlm = lm - lag(lm, 1),
                       dlb = lb - lag(lb, 1)) %>% 
                ungroup() %>% 
                mutate_at(vars(dlm, dlb, disol), ~ifelse(is.nan(.), NA, .)) %>% 
                mutate_at(vars(dlm, dlb, disol), ~ifelse(is.infinite(.), NA, .)) 
        
        data20 = data20 %>% filter(msm10 == 0)
        
        data20$codmun6 = factor(data20$codmun6)
        data20$month = factor(data20$month)
        data20$week = factor(data20$week)
        
        tbl[[i - 2011]] = 
                felm(dlb ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = data20, 
             weights = data20$popm, cmethod = "reghdfe") %>%
                tidy(conf.int = TRUE) %>% 
                filter(term == "disol") %>% 
                select(estimate, conf.low, conf.high) %>% 
                mutate(year = i) 

}

tbl = tbl %>% bind_rows()

# Plot coefficients
ggplot(data = tbl, aes(x = factor(year), y = estimate)) + geom_point() +
        geom_errorbar(aes(ymax = conf.high, ymin = conf.low,  width=.1)) +
        ylab("Coefficient") +
        xlab("Year") + 
        theme_classic() + geom_hline(yintercept=0, linetype='dotted', col = "red2", size = 0.8)+
        scale_x_discrete(labels = c(2012:2020)) +
        theme(text = element_text(family = "Helvetica"))

###########################################################################

# Step 6: Margins plots

# We use numbers calculated by Stata (see file Margins.do)

# Main regression (column 4)
#  margins, at(disol=(-0.05 -0.019 0 0.026 0.09))
#------------------------------------------------------------------------------
#|            Delta-method
#|     Margin   Std. Err.      z    P>|z|     [95% Conf. Interval]
#-------------+----------------------------------------------------------------
#_at |
#1  |   .0369346   .0110555     3.34   0.001     .0152661    .0586031
#2  |   .0198127   .0052896     3.75   0.000     .0094453    .0301801
#3  |   .0093187   .0017556     5.31   0.000     .0058777    .0127596
#4  |  -.0050416   .0030803    -1.64   0.102    -.0110789    .0009956
#5 |    -.0182973   .0075443    -2.43   0.015    -.0330838   -.0035108
#6  |  -.0403901   .0149842    -2.70   0.007    -.0697585   -.0110216


maintbl = tibble(predict = c(.0369346,
                             .0198127,
                             .0093187,
                            -.0050416,
                            -.0182973,
                            -.0403901),
                 isol = c(-0.05,
                          -0.019,
                           0,
                           0.026,
                           0.05,
                           0.09),
                 conf.low = c(.0152661,
                              .0094453,
                              .0058777,
                              -.0110789,
                              -.0330838,
                              -.0697585),
                 conf.high = c(.0586031,
                               .0301801,
                               .0127596,
                               .0009956,
                               -.0035108,
                               -.0110216))

ggplot(data = maintbl, aes(x = isol, y = predict)) + 
        geom_bar(stat = "identity", fill = "#3B4992") +
        geom_errorbar(aes(ymax = conf.high, ymin = conf.low), width = .01, size = 1) +
        geom_hline(yintercept=0, linetype='dashed', col = "#BB0021FF", size = 1.2)+
        ylab("Marginal effect of isolation on conceptions") +
        xlab("Isolation") + 
        theme_classic() +
        scale_x_continuous(breaks = c(-0.05, -0.019, 0, 0.026, 0.05, 0.09))


# Margins plot 9pp

# Municipalities
# margins, at(disol=(0.09))
# -----------------------------------------------------------------------------
#|            Delta-method
#|     Margin   Std. Err.      z    P>|z|     [95% Conf. Interval]

# 
# Rich |  -.0418371   .0172325    -2.43   0.015    -.0756122   -.0080621
# Poor |  -.0286973   .0346156    -0.83   0.407    -.0965427    .0391481
# Urban |  -.0416514     .01524    -2.73   0.006    -.0715214   -.0117815
# Rural | - .0997892   .1221155    -0.82   0.414    -.3391312    .1395528
# Large |  -.0395408   .0177277    -2.23   0.026    -.0742865   -.0047952
# Small |   -.0573708   .0307981    -1.86   0.062    -.1177341    .0029924

# High |  -.0448192   .0176999    -2.53   0.011    -.0795103   -.0101281
# Low | -.0375544   .0311007    -1.21   0.227    -.0985107    .0234018
# Young  -.0824863   .0244688    -3.37   0.001    -.1304443   -.0345284
# Old |  -.0170619   .0193616    -0.88   0.378    -.0550099    .0208861
# No K | -.0479975   .0245004    -1.96   0.050    -.0960174    .0000224
# Ye K|   -.0375375   .0228194    -1.64   0.100    -.0822627    .0071878


marginswom = tibble(predict = c(-.0448192,
                                -.0375544,
                                -.0824863,
                                -.0170619,
                                -.0479975,
                                -.0375375),
                   conf.low = c(-.0795103,
                                 -.0985107,
                                 -.1304443,
                                 -.0550099,
                                 -.0960174,
                                 -.0822627),
                   conf.high = c(-.0101281,
                                 .0234018,
                                 -.0345284,
                                 .0208861,
                                 .0000224,
                                 .0071878),
                   group = c("More Educated",
                             "Less Educated",
                             "Younger",
                             "Older",
                             "No kids",
                              "More than 1 kid"))

marginsmun = tibble(predict = c(-.0418371,
                                -.0286973,
                                -.0416514,
                                -.0997892,
                                -.0395408,
                                -.0573708),
                 conf.low = c(-.0756122,
                              -.0965427,
                              -.0715214,
                              -.3391312,
                              -.0742865,
                              -.1177341),
                 conf.high = c(-.0080621,
                               .0391481,
                               -.0117815,
                               .1395528,
                               -.0047952,
                               .0029924),
                 group = c("Richer",
                           "Poorer",
                           "Urban",
                           "Rural",
                           "Larger",
                           "Smaller"))
                     
marginsmun$group = factor(marginsmun$group, levels = c("Richer",
                                                       "Poorer",
                                                       "Urban",
                                                       "Rural",
                                                       "Larger",
                                                       "Smaller"))


marginswom$group = factor(marginswom$group, levels = c("More Educated",
                                                        "Less Educated",
                                                        "Younger",
                                                        "Older",
                                                        "No kids",
                                                        "More than 1 kid"))
# Plots
ggplot(data = marginsmun, aes(x = group, y = predict)) + 
       geom_bar(stat = "identity", fill  = rep(c("#BB0021", "#3B4992"), 3)) +
       geom_errorbar(aes(ymax = conf.high, ymin = conf.low), width = 0.2, size = 1) +
       geom_hline(yintercept=0, color = "black", linetype='dashed', size = 1) +
       theme_classic() +
       ylab("Marginal effect of isolation on conceptions") +
       xlab("Group")

ggplot(data = marginswom, aes(x = group, y = predict)) + 
       geom_bar(stat = "identity", fill  = rep(c("#BB0021", "#3B4992"), 3)) +
       geom_errorbar(aes(ymax = conf.high, ymin = conf.low), width = 0.2, size = 1) +
       geom_hline(yintercept=0, color = "black", linetype='dashed', size = 1) +
       theme_classic() +
       ylab("Marginal effect of isolation on conceptions") +
       xlab("Group")

###########################################################################

# Step 7: Conceptions plot

data = fread("data_births_br.csv")

data = data %>% 
        mutate(days = case_when(month == 1 ~ 31,
                                month == 2 ~ 28,
                                month == 3 ~ 31,
                                month == 4 ~ 30,
                                month == 5 ~ 31,
                                month == 2 & year == 2020 ~ 29,
                                month == 2 & year == 2016 ~ 29,
                                month == 2 & year == 2012 ~ 29)) %>% 
        select(month, year, days, totalconcep) %>% 
        mutate(dailyconcep = totalconcep/days) %>% 
        group_by(month) %>% 
        mutate(max = max(dailyconcep[year != 2020]),
               min = min(dailyconcep[year != 2020])) %>% 
        ungroup()

data = data %>% 
       mutate(categ = case_when(between(year, 2012, 2013) ~ "2012-2013",
                                between(year, 2014, 2015) ~ "2014-2015",
                                between(year, 2016, 2017) ~ "2016-2017",
                                between(year, 2018, 2019) ~ "2018-2019",
                                between(year, 2020, 2020) ~ "2020")) %>% 
       group_by(month, categ) %>% 
       summarise(dailyconcep = mean(dailyconcep),
                 max = mean(max),
                 min = mean(min)) 
  
ggplot(data, aes(x=factor(month), y= dailyconcep, group = factor(categ))) +
       geom_line(aes(color = factor(categ)), size = 1.2) +
       scale_color_manual(values=c("2012-2013" = "#A20056FF",
                                   "2014-2015" = "#808180FF",
                                   "2016-2017" = "#008280FF",
                                   "2018-2019" = "#3B4992FF",
                                   "2020" = "#BB0021FF")) +
       scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May")) +
       ylab("Daily Conceptions") + 
       xlab("Month")   +
       labs(color = "Year", fill = "") +
       geom_ribbon(aes(x = month, ymin = min, ymax = max, fill = "range"), 
                   alpha = 0.1, fill = "grey") +
       theme_classic() 
       
###########################################################################

# Step 8: Isolation plot

# Isolation
isolation = fread("isolamento_fev-jun.csv", encoding = "UTF-8")

# Use new IBGE dataset to reconcile city names
ibge = read_excel("pop.xlsx", skip = 1, sheet = 2)
ibge$`COD. UF` = as.character(ibge$`COD. UF`)
ibge = ibge %>% unite(codmun, "COD. UF", "COD. MUNIC", sep = "", 
                      remove = TRUE) %>% 
  rename(name = `NOME DO MUNICÍPIO`,
         popm = `POPULAÇÃO ESTIMADA`)
ibge$popm = gsub("\\(.*", "", ibge$popm)
ibge$popm = gsub(" ", "", ibge$popm)
ibge$popm = as.numeric(ibge$popm)
ibge$codmun = as.numeric(ibge$codmun)
ibge$codmun6 = as.character(floor(ibge$codmun / 10))

# Add gdp data
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

ibge = left_join(ibge, pib, by = "codmun")

# State names in the isolation dataset are not acronymized
ibge = ibge %>% 
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

# We need to add municipality codes to the isolation dataset
# But names are spelled out differently between the IBGE dataframe and
# The isolation dataset, so we make some adjustments

ibge$name = gsub("D'O", "d'O", ibge$name)
ibge$name = gsub("D'A", "d'A", ibge$name)
ibge$name = gsub("Januário Cicco", "Boa Saúde", ibge$name)
ibge$name = chartr("éêóôãâáõíúz", "eeooaaaoius", ibge$name)
ibge$name = gsub("-", " ", ibge$name)

isolation$city_name = gsub("-", " ", isolation$city_name)
isolation$city_name = gsub("‎", "", isolation$city_name)
isolation$city_name = chartr("éêóôãâáõíúz", "eeooaaaoius", isolation$city_name)

ibge$name = recode(ibge$name, 
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

# Join with IBGE dataset to get city codes and population
isolation = left_join(isolation, ibge, by = c("city_name" = "name",
                                              "state_name"))

# Calculate weighted daily mean
isolation = isolation %>% 
            mutate(poor = gdpc < 17427) %>% 
            group_by(poor, codmun6) %>%
            filter(n() == 180) %>% ungroup() %>% 
            group_by(poor, dt) %>% 
            summarise(isolated = weighted.mean(isolated, popm)) %>% 
            filter(dt < as.Date("2020-06-01"))

# Plot
  ggplot(isolation, aes(x = dt, y = isolated * 100, group = poor)) + 
         geom_line(aes(color = poor), size = 1) +
         ylab("Mean Isolation (%)") + 
         xlab("Date")   +
         theme_classic() +
         scale_color_manual(values = c("#BB0021FF", "#3B4992FF"),
                            name = "Municipalities",
                            labels = c("Richer", "Poorer")) +
         scale_x_date(date_breaks = "1 month", date_labels = "%b")

###########################################################################
  
# Step 9: Map and scatterplot
  
data = fread("data_monthly_mun.csv")
  
data = data %>% 
       group_by(month, UF) %>% 
       summarise(isolated = weighted.mean(isolated, popm)) %>% 
       ungroup() %>% 
       mutate(year = 2020)

data_uf = fread("data_births_uf.csv")

data = right_join(data, data_uf, by = c("UF", "month", "year"))
  
df = data %>% 
     mutate(lb = log(totalconcep)) %>% 
     arrange(UF, year, month) %>% 
     group_by(UF) %>% 
     mutate(ddlb = lb - lag(lb, 1) - (lag(lb, 5) - lag(lb, 6)),
            disol = isolated - lag(isolated, 1)) %>% 
     ungroup() %>% 
     filter(year == 2020 & month > 2)
  
df$month = factor(df$month)
levels(df$month) = c("Mar", "Apr", "May")

# Scatterplot of isolation and conception double differences  
ggplot(data = df, aes(x = disol, y = ddlb)) +
       geom_point(size = 2.5, alpha = 0.9, color = "black") + 
       geom_smooth(method = "lm", color = "firebrick2", size = 1.6, se = FALSE) + 
       stat_cor(method = "pearson", aes(label = ..r.label..)) +
       facet_grid(rows = list(df$month)) +
       ylab("\u0394 Conceptions (Double Differences)") +
       xlab("\u0394 Isolation") +
       theme_classic() + theme(aspect.ratio = 0.5) +
       theme(strip.background = element_rect(fill = "grey", size = 0.1))+
       theme(strip.text.y.right = element_text(angle = 0, color = "#3B4992FF",
                                               face = "bold")) +
       theme(text = element_text(size = 14))


# Map with double differences by state
map = read_sf("./BR_UF_2020/BR_UF_2020.shp", layer="BR_UF_2020")

d = left_join(df, map, by = c("UF" = "SIGLA_UF"))
  
d$month = factor(d$month)
levels(d$month) = c("Mar", "Apr", "May")
  
d$ddlb[d$ddlb < -0.1] = -0.1
d$ddlb[d$ddlb  > 0.1] = 0.1

ggplot(data = d) +
       geom_sf(data = d$geometry, color = NA,
               aes(fill = d$ddlb)) +
       theme_void() +
       facet_wrap(~ d$month, ncol = 3) +
       theme(legend.position = "bottom") +
       theme(text = element_text(size = 13)) +
       binned_scale(aesthetics = "fill",
                    name = "\u0394 Conceptions",
                    scale_name = "stepsn", 
                    palette = function(x) c("#BB0021", "indianred1", 
                                            "lightslateblue", "#3B4992"),
                    breaks = c(-1, -0.05, 0, 0.05, 1),
                    limits = c(-0.10, 0.10),
                    show.limits = TRUE, 
                    guide = guide_colorbar(direction = "horizontal",
                                           title.position = "top",
                                           title.hjust = 0.5,
                                           barheight = 0.5,
                                           barwidth = 15))

###########################################################################
  

