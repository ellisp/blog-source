** Simple Stata demo of survey design and calcualtion, assumes data has been
** previously downloaded in the accompanying R script
clear
cd "C:\repositories\blog-source\_working"

import delimited simulated-survey.csv 

** make a numeric version of province, as over() can't work with strings:
encode province, gen(province_n)

** Set up survey design = complex 2 stages design stratified sample
** first step of sampling design is selection of neighborhoods from each province with finite neighborhoods,
** second step is individuals select from the finite population in each neighborhood
svyset neighborhood [pweight=fweight], singleunit(centered) /// 
  strata(province) fpc(nb_in_province) ///
  || ind_id, fpc(pop_in_neighborhood)		

** Population level design effects
svy: mean likes_cats
estat effect
  
** Strata level design effects
svy: mean likes_cats, over(province_n)   
estat effect                          

** Strata level design effects
estat effect, srssubpop                          
