# regressions_and_figures.R

# Author: Luca Moreno-Louzada
# 2023
# Code for manuscript:
# "The relationship between staying at home during the pandemic and the number of conceptions: a national panel data analysis"
# Luca Moreno-Louzada and Naercio Menezes-Filho

# This code:
# > Runs regressions on the effect of isolation on conceptions
# > Prepares plots and tables with regressions results
# > Prepares plots and maps with data on isolation and conceptions

###########################################################################

# Step 0:  Cleaning workplace and loading libraries

rm(list = ls())

setwd("P:/Luca Louzada/COVID-19 Births/Final")

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
library(ggpubr) # other data visualization

options(scipen = 999) # disable scientific notation

###########################################################################

# Step 1: Main Regression Table

data_main = fread("data_2020_mun_final.csv") %>% filter(year < 2021)

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
             lbage1 = log(totalconcep_age_first),
             lbage2 = log(totalconcep_age_second),
            lbage3 = log(totalconcep_age_third),
            lbage4 = log(totalconcep_age_fourth),
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
             dlbage1 = lbage1 - lag(lbage1, 1),
             dlbage2 = lbage2 - lag(lbage2, 1),
             dlbage3 = lbage3 - lag(lbage3, 1),
             dlbage4 = lbage4 - lag(lbage4, 1),
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
                    dlbage1, dlbage2, dlbage3, dlbage4,
                    dlbagey, dlbageo, 
                    dlbled, dlbhed, dlbkno, dlbkye), ~ifelse(is.nan(.), NA, .)) %>% 
     mutate_at(vars(dlm, dlb, disol,
                    dlbagey, dlbageo,
                    dlbage1, dlbage2, dlbage3, dlbage4,
                    dlbled, dlbhed, dlbkno, dlbkye), ~ifelse(is.infinite(.), NA, .)) 

fdf$codmun6 = factor(fdf$codmun6)
fdf$month = factor(fdf$month)
fdf$week = factor(fdf$week)

# First columns
data = filter(fdf, msm10 == 0)
m1 = felm(dlb ~ disol | week| 0 | codmun6, data = data,
          weights = data$popm, cmethod = "reghdfe")

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
data_mic = fread("data_2020_mic_new.csv")

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
data_mon = fread("data_monthly_mun_new.csv")

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
          out = "./PLOS R&R 2/regression_main_new.doc")

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
          out = "./PLOS R&R 2/regression_mun_new.doc")


# Women
df = filter(fdf, msm10 == 0)

m13 = felm(dlbled ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
          weights = df$popm, cmethod = "reghdfe")

m14 = felm(dlbhed ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
          weights = df$popm, cmethod = "reghdfe")

m15 = felm(dlbkye ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

m16 = felm(dlbkno ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

m17 =  felm(dlbage1 ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
                 weights = df$popm, cmethod = "reghdfe")

m18 =  felm(dlbage2 ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
            weights = df$popm, cmethod = "reghdfe")
m19 =  felm(dlbage3 ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
            weights = df$popm, cmethod = "reghdfe")
m20 =  felm(dlbage4 ~ disol + dlm | week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
            weights = df$popm, cmethod = "reghdfe")


stargazer(m13, m14, m15, m16, m17, m18, m19, m20, type = "html",
          title = "Heterogeneous effects",
          keep.stat = c("n", "rsq"),
          dep.var.labels.include = F,
          dep.var.caption = "Delta ln Conceptions",
          column.labels= c("Less Educ", "More Educ", 
                           "Previous kids", "No kids",
                           "Aged < 21", "Aged 21-25",
                           "Aged 26-32", "Aged > 32"
                           ),
          covariate.labels = c("Delta Isolation", "Delta ln Deaths"),
          out = "./PLOS R&R 2/regression_wom_final.doc")


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
          out = "./PLOS R&R 2/regression_rob_new.doc")

###########################################################################

# Step 4: Weekdays tests

# We'll run the main regression with the dataset grouped by different weekdays
weekdays = fread("data_weekdays_new.csv", encoding = "Latin-1")

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

df = msmdf %>% filter(weekday == "segunda-feira")
m26 =felm(dlb ~ disol + dlm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
          weights = df$popm, cmethod = "reghdfe")

df = msmdf %>% filter(weekday == "terça-feira")
m27 = felm(dlb ~ disol + dlm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

df = msmdf %>% filter(weekday == "quarta-feira")
m28 = felm(dlb ~ disol + dlm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

df = msmdf %>% filter(weekday == "quinta-feira")
m29 = felm(dlb ~ disol + dlm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

df = msmdf %>% filter(weekday == "sexta-feira")
m30 = felm(dlb ~ disol + dlm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

df = msmdf %>% filter(weekday == "sábado")
m31 = felm(dlb ~ disol + dlm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")

df = msmdf %>% filter(weekday == "domingo")
m32 = felm(dlb ~ disol + dlm| week + codmun6 + month + codmun6:month| 0 | codmun6, data = df, 
           weights = df$popm, cmethod = "reghdfe")


stargazer(m26, m27, m28, m29, m30, m31, m32, type = "html",
          title = "Robustness tests",
          keep.stat = c("n", "rsq"),
          dep.var.labels.include = F,
          column.labels= c("Monday", "Tuesday", "Wednesday", "Thursday",
                           "Friday", "Saturday", "Sunday"),
          covariate.labels = c("Delta Isolation", "Delta deaths"),
          out = "./PLOS R&R 2/regression_week_new.doc")

###########################################################################

# Step 5: Placebo tests

# We'll join our dataset with conception data for other years
data_years = fread("concep_week_mun_new.csv") %>% 
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
plot =  ggplot(data = tbl, aes(x = factor(year), y = estimate)) + geom_point() +
        geom_errorbar(aes(ymax = conf.high, ymin = conf.low,  width=.1)) +
        ylab("Coefficient") +
        xlab("Year") + 
        theme_classic() + geom_hline(yintercept=0, linetype='dotted', col = "red2", size = 0.8)+
        scale_x_discrete(labels = c(2012:2020)) 

ggsave(plot = plot, filename = "./PLOS R&R 2/Figures/Figure_S1.tiff", dpi = 500, width = 10, height = 6)

###########################################################################

# Step 6: Margins plots

# We use numbers calculated by Stata (see file Margins.do)

# Main regression (column 4)
#  margins, at(disol=(-0.05 -0.019 0 0.026 0.09))

#------------------------------------------------------------------------------
#  |            Delta-method
#|     Margin   Std. Err.      z    P>|z|     [95% Conf. Interval]
#-------------+----------------------------------------------------------------
#  _at |
# 1  |   .0313938   .0091653     3.43   0.001       .01343    .0493575
#2  |   .0158972   .0040108     3.96   0.000     .0080363    .0237582
#3  |   .0063993   .0008515     7.52   0.000     .0047304    .0080683
#4  |  -.0065978   .0034717    -1.90   0.057    -.0134021    .0002066
#5  |  -.0185951   .0074623    -2.49   0.013    -.0332209   -.0039692
#6  |  -.0385906   .0141134    -2.73   0.006    -.0662523   -.0109289
#------------------------------------------------------------------------------
  
  


maintbl = tibble(predict = c(.0313938,
                             .0158972,
                             .0063993,
                             -.0065978 ,
                             -.0185951,
                             -.0385906),
                 isol = c(-0.05,
                          -0.019,
                           0,
                           0.026,
                           0.05,
                           0.09),
                 conf.low = c(.01343,
                              .0080363,
                              .0047304,
                              -.0134021,
                              -.0332209,
                              -.0662523),
                 conf.high = c(.0493575,
                               .0237582,
                               .0080683,
                               .0002066,
                               -.0039692,
                               -.0109289))

plot =  ggplot(data = maintbl, aes(x = isol, y = predict)) + 
        geom_bar(stat = "identity", fill = "#3B4992") +
        geom_errorbar(aes(ymax = conf.high, ymin = conf.low), width = .01, size = 1) +
        geom_hline(yintercept=0, linetype='dashed', col = "#BB0021FF", size = 1.2)+
        ylab("Marginal effect of isolation on conceptions") +
        xlab("Isolation") + 
        theme_classic() +
        scale_x_continuous(breaks = c(-0.05, -0.019, 0, 0.026, 0.05, 0.09))

ggsave(plot = plot, filename = "./Article/Figures_new2/Figure_3A.tiff", dpi = 500, width = 10, height = 6)

# Margins plot 9pp

# Municipalities
# margins, at(disol=(0.09))
# -----------------------------------------------------------------------------
#|            Delta-method
#|     Margin   Std. Err.      z    P>|z|     [95% Conf. Interval]

# 
# Rich |   -.0395243   .0159793    -2.47   0.013    -.0708431   -.0082055
# Poor |    -.0368846   .0315297    -1.17   0.242    -.0986817    .0249125
# Urban |    -.0399925   .0144223    -2.77   0.006    -.0682597   -.0117253
# Rural |  -.0831898   .0679901    -1.22   0.221    -.2164479    .0500684
# Large |    -.0418692   .0166911    -2.51   0.012     -.074583   -.0091553
# Small |     -.0378896   .0276033    -1.37   0.170    -.0919912    .0162119

# High |   -.0402345   .0164928    -2.44   0.015    -.0725598   -.0079093
# Low |  -.0339292   .0291972    -1.16   0.245    -.0911546    .0232962
# No K |  -.0107084   .0252173    -0.42   0.671    -.0601334    .0387166
# Ye K|    -.0532698   .0216048    -2.47   0.014    -.0956145   -.0109252
# Age 1   -.0256051   .0390969    -0.65   0.513    -.1022336    .0510233
# Age 2|    -.0973456   .0270123    -3.60   0.000    -.1502888   -.0444025
# Age 3|   -.0196996   .0255894    -0.77   0.441     -.069854    .0304547
# Age 4|    -.0019337   .0273573    -0.07   0.944     -.055553    .0516857


marginswom = tibble(predict = c(-.0402345,
                                -.0339292,
                                -.0107084,
                                -.0532698,
                                -.0256051,
                                -.0973456,
                                -.0196996,
                                -.0019337),
                   conf.low = c(-.0725598,
                                -.0911546,
                                -.0601334,
                                -.0956145 ,
                                -.1022336,
                                -.1502888,
                                -.069854,
                                -.055553
                                ),
                   conf.high = c(-.0079093,
                                 .0232962,
                                 .0387166,
                                 -.0109252,
                                 .0510233,
                                 -.0444025,
                                 .0304547,
                                 .0516857
                                 ),
                   group = c("More Educated",
                             "Less Educated",
                             "No kids",
                              "1 kid or more",
                             "Aged < 21",
                             "Aged 21-25",
                             "Aged 26-32",
                             "Aged > 32"))

marginsmun = tibble(predict = c(-.0395243,
                                -.0368846,
                                -.0399925,
                                -.0831898,
                                -.0418692,
                                -.0378896),
                    conf.low = c(-.0708431,
                                 -.0986817,
                                 -.0682597,
                                 -.2164479 ,
                                 -.074583,
                                 -.0919912),
                    conf.high = c(-.0082055,
                                  .0249125,
                                  -.0117253,
                                  .0500684,
                                  -.0091553,
                                  .0162119),
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
                                                       "No kids",
                                                       "1 kid or more",
                                                       "Aged < 21",
                                                       "Aged 21-25",
                                                       "Aged 26-32",
                                                       "Aged > 32"))
# Plots
plot = ggplot(data = marginsmun, aes(x = group, y = predict)) + 
       geom_bar(stat = "identity", fill  = rep(c("#BB0021", "#3B4992"), 3)) +
       geom_errorbar(aes(ymax = conf.high, ymin = conf.low), width = 0.2, size = 1) +
       geom_hline(yintercept=0, color = "black", linetype='dashed', size = 1) +
       theme_classic() +
       ylab("Marginal effect of isolation on conceptions") +
       xlab("Group")

ggsave(plot = plot, filename = "./Article/Figures_new2/Figure_3B.tiff", dpi = 500, width = 10, height = 6)


plot = ggplot(data = marginswom, aes(x = group, y = predict)) + 
       geom_bar(stat = "identity", fill  = rep(c("#BB0021", "#3B4992"), 4)) +
       geom_errorbar(aes(ymax = conf.high, ymin = conf.low), width = 0.2, size = 1) +
       geom_hline(yintercept=0, color = "black", linetype='dashed', size = 1) +
       theme_classic() +
       ylab("Marginal effect of isolation on conceptions") +
       xlab("Group")

ggsave(plot = plot, filename = "./PLOS R&R 2/Figures/Figure_3C.tiff", dpi = 500, width = 10, height = 6)


###########################################################################

# Step 7: Conceptions plot

data = fread("data_births_br.csv") 

data = data %>% 
        mutate(days = case_when(month == 1 ~ 31,
                                month == 2 ~ 28,
                                month == 3 ~ 31,
                                month == 4 ~ 30,
                                month == 5 ~ 31,
                                month == 6 ~ 30,
                                month == 7 ~ 31,
                                month == 8 ~ 31,
                                month == 9 ~ 30,
                                month == 10 ~ 31,
                                month == 11 ~ 30,
                                month == 12 ~ 31,
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
  
plot = ggplot(data, aes(x=factor(month), y= dailyconcep, group = factor(categ))) +
       geom_line(aes(color = factor(categ)), size = 1.2) +
       scale_color_manual(values=c("2012-2013" = "#A20056FF",
                                   "2014-2015" = "#808180FF",
                                   "2016-2017" = "#008280FF",
                                   "2018-2019" = "#3B4992FF",
                                   "2020" = "#BB0021FF")) +
       scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May",
                                   "Jun", "Jul", "Aug", "Sep", "Oct",
                                   "Nov", "Dec")) +
       ylab("Daily Conceptions") + 
       xlab("Month")   +
       labs(color = "Year", fill = "") +
       geom_ribbon(aes(x = month, ymin = min, ymax = max, fill = "range"), 
                   alpha = 0.1, fill = "grey") +
       theme_classic() 

ggsave(plot = plot, filename = "./Article/Figures_new2/Figure_1A.tiff", dpi = 500, width = 10, height = 6)

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
            summarise(isolated = weighted.mean(isolated, popm)) 

# Plot
plot = ggplot(isolation, aes(x = dt, y = isolated * 100, group = poor)) + 
       geom_line(aes(color = poor), size = 1) +
       ylab("Mean Isolation (%)") + 
       xlab("Date")   +
       theme_classic() +
       scale_color_manual(values = c("#BB0021FF", "#3B4992FF"),
                          name = "Municipalities",
                          labels = c("Richer", "Poorer")) +
       scale_x_date(date_breaks = "1 month", date_labels = "%b")

ggsave(plot = plot, filename = "./Article/Figures_new2/Figure_2A.tiff", dpi = 500, width = 10, height = 6)

#
###########################################################################
  
# Step 9: Map and scatterplot
  
data = fread("data_monthly_mun_new.csv")
  
data = data %>% 
       group_by(month, UF) %>% 
       summarise(isolated = weighted.mean(isolated, popm)) %>% 
       ungroup() %>% 
       mutate(year = 2020)

data_uf = fread("data_births_uf_new.csv")

data = right_join(data, data_uf, by = c("UF", "month", "year"))
  
df = data %>% 
     mutate(lb = log(totalconcep)) %>% 
     arrange(UF, year, month) %>% 
     group_by(UF) %>% 
     mutate(ddlb = lb - lag(lb, 1) - (lag(lb, 7) - lag(lb, 8)),
            disol = isolated - lag(isolated, 1)) %>% 
     ungroup() %>% 
     filter(year == 2020 & month > 1)
  
df$month = factor(df$month)
levels(df$month) = c("Feb", "Mar", "Apr", "May", "Jun", "Jul")

# Scatterplot of isolation and conception double differences  
plot = ggplot(data = df, aes(x = disol, y = ddlb)) +
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

ggsave(plot = plot, filename = "./Article/Figures_new2/Figure_2B.tiff", dpi = 500, width = 6, height = 10)


# Map with double differences by state
map = read_sf("./BR_UF_2020/BR_UF_2020.shp", layer="BR_UF_2020")

d = left_join(df, map, by = c("UF" = "SIGLA_UF"))
  
d$month = factor(d$month)
levels(d$month) = c("Feb", "Mar", "Apr", "May", "Jun", "Jul")
  
d$ddlb[d$ddlb < -0.1] = -0.1
d$ddlb[d$ddlb  > 0.1] = 0.1

plot = ggplot(data = d) +
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

ggsave(plot = plot, filename = "./Article/Figures_new2/Figure_1B.tiff", dpi = 500, width = 10, height = 6)


###########################################################################
  

