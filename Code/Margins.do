* Author: Luca Moreno-Louzada
* 2023
* Code for manuscript:
* "The relationship between staying at home during the pandemic and the number of conceptions: a national panel data analysis"
* Luca Moreno-Louzada and Naercio Menezes-Filho

use "P:\Luca Louzada\COVID-19 Births\Final\data_2020_mun_final.dta", clear
 
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

gen lbage1=ln(totalconcep_age_first)
gen lbage2=ln(totalconcep_age_second)
gen lbage3=ln(totalconcep_age_third)
gen lbage4=ln(totalconcep_age_fourth)


qui by codmun: gen dlbage1=lbage1-lbage1[_n-1]
qui by codmun: gen dlbage2=lbage2-lbage2[_n-1]
qui by codmun: gen dlbage3=lbage3-lbage3[_n-1]
qui by codmun: gen dlbage4=lbage4-lbage4[_n-1]



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
qui reghdfe dlb disol dlm if msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(-0.05 -0.019 0 0.026 0.05 0.09))
margins, at(disol=(-0.05 -0.019 0 0.026 0.05 0.09)) pwcompare post
 
* Margins Poor vs Rich
qui reghdfe dlb disol dlm if poor==1 & msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))
margins, at(disol=(0 0.09)) pwcompare post

qui reghdfe dlb disol dlm if poor==0 & msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))
margins, at(disol=(0 0.09)) pwcompare post

* Margins Urban vs Rural
qui reghdfe dlb disol dlm if urban==1 & msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))
margins, at(disol=(0 0.09)) pwcompare post

qui reghdfe dlb disol dlm if urban==0 & msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))
margins, at(disol=(0 0.09)) pwcompare post

* Margins Large vs Small
qui reghdfe dlb disol dlm if big==1 & msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))
margins, at(disol=(0 0.09)) pwcompare post

qui reghdfe dlb disol dlm if big==0 & msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))
margins, at(disol=(0 0.09)) pwcompare post

* Margins High Educ vs Low Educ
qui reghdfe dlbhed disol dlm if msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))
margins, at(disol=(0 0.09)) pwcompare post

qui reghdfe dlbled disol dlm if msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))
margins, at(disol=(0 0.09)) pwcompare post

* Margins Young vs Old
qui reghdfe dlbage1 disol dlm if msm10==0 [aw=pop],abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))
margins, at(disol=(0 0.09)) pwcompare post

qui reghdfe dlbage2 disol dlm if msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))
margins, at(disol=(0 0.09)) pwcompare post

qui reghdfe dlbage3 disol dlm if msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))
margins, at(disol=(0 0.09)) pwcompare post

qui reghdfe dlbage4 disol dlm if msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))
margins, at(disol=(0 0.09)) pwcompare post

* Margins No Kids vs Kids
qui reghdfe dlbkno disol dlm if msm10==0 [aw=pop],abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))
margins, at(disol=(0 0.09)) pwcompare post

qui reghdfe dlbkye disol dlm if msm10==0 [aw=pop], abs(week codmun##i.month) cluster(codmun)
margins, at(disol=(0.09))
margins, at(disol=(0 0.09)) pwcompare post
