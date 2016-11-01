***** comparacion bean normal y bean GCM

cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal" 
use beanNormal.dta, clear 

tab gcm_n

     
***variables originales
*rice__normal__base_2000__broadcast___29feb16_2137
*rice__normal__hadgem2_es__future_rcp8p5_2041_2070__broadcast___29feb16_2245
*rice__normal__gfdl_esm2m__future_rcp8p5_2041_2070__broadcast___29feb16_2211
*rice__normal__ipsl_cm5a_lr__future_rcp8p5_2041_2070__broadcast___29feb16_2319
*rice__normal__miroc_esm_chem__future_rcp8p5_2041_2070__broadcast___29feb16_2348
*rice__normal__noresm1_m__future_rcp8p5_2041_2070__broadcast___01mar16_0016

***********
replace gcm_n="riceBroadBase" if gcm_n=="rice__normal__base_2000__broadcast___29feb16_2137"
replace gcm_n="hadgem2_esBroad" if  gcm_n=="rice__normal__hadgem2_es__future_rcp8p5_2041_2070__broadcast___29feb16_2245"
replace gcm_n="gfdl_esm2mBroad"  if gcm_n=="rice__normal__gfdl_esm2m__future_rcp8p5_2041_2070__broadcast___29feb16_2211"
replace gcm_n="ipsl_cm5aBroad" if gcm_n=="rice__normal__ipsl_cm5a_lr__future_rcp8p5_2041_2070__broadcast___29feb16_2319"
replace gcm_n="miroc_esmBroad" if gcm_n=="rice__normal__miroc_esm_chem__future_rcp8p5_2041_2070__broadcast___29feb16_2348"
replace gcm_n="noresm1_mBroad" if gcm_n=="rice__normal__noresm1_m__future_rcp8p5_2041_2070__broadcast___01mar16_0016"


tab gcm_n

save beanNormal.dta, replace


 *******
 cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal" 
 use  beanNormal.dta, clear
 local v `""beanBase" "gfdl_esm2m" "hadgem2" "ipsl_cm5a" "miroc_esm" "noresm1_m""'
 foreach var of local v {
 keep if gcm_n=="`var'"
 rename yield y_`var'
 save "`var'.dta", replace
 clear
 use beanNormal.dta, clear
}
*end 

clear

****** simplificandola mas
cd"C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal\gcms" 
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal\gcms\ 
foreach name_n in beanBase gfdl_esm2m hadgem2 ipsl_cm5a miroc_esm noresm1_m {
use "`path'\`name_n'.dta",  clear
	drop adopt_area post_adopt_yield post_adopt_prod pre_adopt_yield pre_adopt_prod nonadopt_area nonadopt_yield nonadopt_prod Urban_Pop_ Rural_Pop_ Total_UN20 Urban_UN20 Rural_UN20 Region_Nam New_Region Basin_Name New_Basin SUM_SUM_GA NFPU_INT New_FPU gcm
save "`path'\`name_n'.dta",  replace
}
*end 


**** construir una base de datos completa
set more off
use gfdl_esm2m.dta, clear
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal\gcms\ 
foreach name_n in gfdl_esm2m hadgem2 ipsl_cm5a miroc_esm noresm1_m{
merge 1:1 id sis region  using "`path'\`name_n'.dta"
drop _merge
save "m_`name_n'.dta", replace
}

*end

save "dataBeanNormalcompleta.dta", replace


*****dummy regiones
gen africa=1 if inlist(region, 18, 148, 25, 65, 97,70, 68, 31, 53, 75, 85, 86, 87, 91, 118, 124, 127, 129, 149, 158)
gen asia=1 if inlist(region,12,40,32, 5,41,8,142,9,73,4,17,134, 49, 154, 14, 39, 94, 147, 159, 20,143, 155,119, 22, 28, 34,37, 54, 55,57,	58,	59,	60,	64,	66,	67,	69,	72,	76,	77,	78,	88,	89, 107, 110, 112,	113, 122, 125, 126,	128, 132, 135, 136,	138, 139, 150, 153, 156)
gen lac=1 if inlist(region, 3, 11, 21, 26 ,117, 105, 137,140, 120, 27, 30, 35, 47, 48, 50, 52, 63, 79, 84, 90, 92, 121, 98 , 99 , 103 , 106 , 111 , 116 , 141 , 144, 152,157)

gen pot=. 
replace pot=1 if inlist(region, 18, 148, 25, 65, 97,70, 68, 31, 53, 75, 85, 86, 87, 91, 118, 124, 127, 129, 149, 158)
replace pot=2 if inlist(region,12,40,32, 5,41,8,142,9,73,4,17,134, 49, 154, 14, 39, 94, 147, 159, 20,143, 155,119, 22, 28, 34,37, 54, 55,57,	58,	59,	60,	64,	66,	67,	69,	72,	76,	77,	78,	88,	89, 107, 110, 112,	113, 122, 125, 126,	128, 132, 135, 136,	138, 139, 150, 153, 156)
replace pot=3 if inlist(region, 3, 11, 21, 26 ,117, 105, 137,140, 120, 27, 30, 35, 47, 48, 50, 52, 63, 79, 84, 90, 92, 121, 98 , 99 , 103 , 106 , 111 , 116 , 141 , 144, 152,157)

label define pot 1"Africa" 2"Asia" 3"LAC", modify
label value pot pot 

****** BaseName 
#delimit;
graph bar y_beanBase y_gfdl_esm2m y_hadgem2 y_ipsl_cm5a y_miroc_esm y_noresm1_m if sis==2, 
over(pot, sort(1) label(labsize(large) angle(45)))  
title("{it: Base Vs GCMs Bean,  System Rainfed }")
	ytitle("Yield")					
	ylabel(#10, angle(45) labsize(small)) 		
	scheme(s1mono)	
	bar(1, bcolor(blue)) bar(2, bcolor(red)) bar(3, bcolor(green)) bar(4, bcolor(lime)) bar(5, bcolor(orange))
	bar(4, bcolor(bluelight))
	legend( label(1 "Base") label(2 "gfdl_esm2m") label(3 "hadgem2")label(4 "ipsl_cm5a" )label(5 "miroc_esm")
	label(6 "noresm1_m"));
#delimit cr;
graph save BeanNormalVsGCMs.gph, replace
graph export BeanNormalVsGCMs.png, width(4000) replace 


****creando cambios 
****1gcm
gen cyBaseGdfl= ((y_gfdl_esm2m-y_beanBase)/y_beanBase) *100

#delimit;
graph bar cyBaseGdfl if sis==2, 
over(pot, sort(1) label(labsize(large) angle(45)))  
title("{it:Differences between}" " " "{it: Normal and gfdl_esm2m Bean}")
	ytitle("% change")					
	ylabel(#10, angle(45) labsize(small)) 		
	scheme(s1mono)
	note(Rainfed);	
#delimit cr;
graph save DifferencesBeanNormalVsGCMs.gph, replace
graph export DifferencesBeanNormalVsGCMs.png, width(4000) replace 


****creando cambios 
****3gcm
gen cyBaseHadgem= ((y_hadgem2-y_beanBase)/y_beanBase) *100



****creando cambios 
****4gcm
gen cyBaseIpsl= ((y_ipsl_cm5a-y_beanBase)/y_beanBase) *100



****creando cambios 
****5gcm
gen cyBaseMiroc= ((y_miroc_esm-y_beanBase)/y_beanBase) *100



****creando cambios 
****6gcm
gen cyBaseNores= ((y_noresm1_m-y_beanBase)/y_beanBase) *100


#delimit;
graph bar cyBaseGdfl cyBaseHadgem cyBaseIpsl cyBaseMiroc cyBaseNores if sis==2, 
over(pot, sort(1) label(labsize(large) angle(h)))  
title("{it:Change Between}" " " "{it:Normal and  GCMs Bean  by FPUs}")	ytitle("% Change")					
	ylabel(#10, angle(h) labsize(small)) 		
	scheme(s1mono)	
	bar(1, bcolor(blue)) bar(2, bcolor(red)) bar(3, bcolor(green)) bar(4, bcolor(lime)) bar(5, bcolor(orange))
	bar(4, bcolor(bluelight))
	legend( label(1 "gfdl_esm2m") label(2 "hadgem2")label(3 "ipsl_cm5a" )label(4 "miroc_esm")
	label(5 "noresm1_m"))note(Rainfed Base:2005 GCMs:2050);
#delimit cr;
graph save DifferencesallBeanNormalVsGCMs.gph, replace
graph export DifferencesallBeanNormalVsGCMs.png, width(4000) replace 


****** Map

use dataBeanNormalcompleta.dta, replace
foreach x of varlist cyBaseGdfl cyBaseHadgem cyBaseIpsl cyBaseMiroc cyBaseNores {
spmap `x' using coorfpuall if sis==2, id(id) fcolor(BuRd) legtitle ("{it:Yield Bean}") ///
clnumber(7) ndfcolor(gs8) ndlab("missing") legend(position(7)) ///
title("{it:Differences Between}" " " "{it: Normal and  GCMs Bean  by FPUs}"	///				
 , position(12)) note("{stMono:System Rainfed Base:2005 GCM: 2050}", position (7))
graph save `x'.gph, replace
graph export `x'.png, replace width(4000)  
}

end



*************************************************************************************
*************************************************************************************
*************************************************************************************
*************************************************************************************
*************************************************************************************
*************************************************************************************

*****Drought

cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought" 


