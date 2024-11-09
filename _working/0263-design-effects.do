


** Set up survey design = complex 2 stages design stratified sample
svyset id4 [pweight=fweight], singleunit(centered) strata(strata) fpc(nb_ea_strata) ||  /// first step of sampling design=selection of EAs (id4) with fpc
ind_id, fpc(pop15)		// second step of survey design: selection of individuals with fpc																						

****************************************************************************************************
  *employment rate=population employed/total population 15+
  gen temp=(ilo_lfs==1)   //temp is the variable that shows 1 if people are in employed and 0 for those not in employment
tab temp, nolab

** statistics at strata level
svy: proportion temp , over(strata)   //sampling error for temp (employment) at the strata level (total 8 stratas)
estat effect                          //design effect for employment rate at the strata level

** statistics at province level    
svy: proportion temp , over(province) //sampling error for temp (employment) at the province level (total 6 provinces)
estat effect                          //design effect for temp (employment) at the province level 

** statistics at urb_rur level
svy: proportion temp , over(urb_rur) //sampling error for temp (employment) at the urban rural level 
estat effect                         //design effect for temp (employment) at the urban rural level 

** statistics at National level
svy: proportion temp   // sampling error for temp (employment rate) at the National level
estat effect           //design effect for temp (employment rate) at the National level

svy: proportion temp, over(sex)
estat effect
estat effect, srssubpop
