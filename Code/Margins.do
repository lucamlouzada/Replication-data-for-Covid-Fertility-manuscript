* Author: Luca Moreno-Louzada
* 2021
* Code for manuscript:
* "Staying at Home during Covid Outbreaks Leads to Less Conceptions"
* Luca Moreno-Louzada and Naercio Menezes-Filho

use "C:\Users\lucam\OneDrive\Área de Trabalho\Economia\COVID SINASC\data_2020_mun.dta", clear
 
gen codmun = real(codmun6)
 
gen sm10=1 if deaths==0| totalconcep<10
replace sm10=0 if deaths~=0 & totalconcep>=10
egen msm10=mean(sm), by(codmun)

gen lb=ln(totalconcep)
gen lm=ln(deaths)

so codmun week
qui by codmun: gen disol=isol-isol[_n-1]
qui by codmun: gen dlb=lb-lb[_n-1]
qui by codmun: gen dlm=lm-lm[_n-1]

gen lbagey=ln(totalconcep_age_young)
gen lbageo=ln(totalconcep_age_old)
qui by codmun: gen dlbagey=lbagey-lbagey[_n-1]
qui by codmun: gen dlbageo=lbageo-lbageo[_n-1]

gen lbled=ln(totalconcep_educ_low)
gen lbhed=ln(totalconcep_educ_high)
qui by codmun: gen dlbhed=lbhed-lbhed[_n-1]
qui by codmun: gen dlbled=lbled-lbled[_n-1]

gen lbkno =ln(totalconcep_kids_no)
gen lbkye =ln(totalconcep_kids_more)
qui by codmun: gen dlbkno =lbkno-lbkno[_n-1]
qui by codmun: gen dlbkye =lbkye-lbkye[_n-1]

gen poor=1 if  gdp<17427
replace poor=0 if gdp>17427 & gdp~=.

gen big=1 if pop>120000
replace big=0 if pop<=120000

* Margins Main
reghdfe dlb disol dlm if msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(-0.05 -0.019 0 0.026 0.05 0.09))

* Margins Poor vs Rich
reghdfe dlb disol dlm if poor==1 & msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))

reghdfe dlb disol dlm if poor==0 & msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))

* Margins Urban vs Rural
reghdfe dlb disol dlm if urban==1 & msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))

reghdfe dlb disol dlm if urban==0 & msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))

* Margins Large vs Small
reghdfe dlb disol dlm if big==1 & msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))
reghdfe dlb disol dlm if big==0 & msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))

* Margins High Educ vs Low Educ
reghdfe dlbhed disol dlm if msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))
reghdfe dlbled disol dlm if msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))

* Margins Young vs Old
reghdfe dlbagey disol dlm if msm10==0 [aw=pop],abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))
reghdfe dlbageo disol dlm if msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))

* Margins No Kids vs Kids
reghdfe dlbkno disol dlm if msm10==0 [aw=pop],abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))
reghdfe dlbkye disol dlm if msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))

