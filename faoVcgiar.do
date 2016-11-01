*********** Análisis diferencia rendimientos  Ricky and FAO

use "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\\BASE.dta", clear
gen pais=.
replace pais=1 if New_Region=="AFG"
replace pais=2 if New_Region=="AGO"
replace pais=3 if New_Region=="ALB"
replace pais=4 if New_Region=="ARG"
replace pais=5 if New_Region=="ARM"
replace pais=6 if New_Region=="AUS"
replace pais=7 if New_Region=="AUT"
replace pais=8 if New_Region=="AZE"
replace pais=9 if New_Region=="BDI"
replace pais=10 if New_Region=="BEN"
replace pais=11 if New_Region=="BFA"
replace pais=12 if New_Region=="BGD"
replace pais=13 if New_Region=="BGR"
replace pais=14 if New_Region=="BLR"
replace pais=15 if New_Region=="BLT"
replace pais=16 if New_Region=="BLX"
replace pais=17 if New_Region=="BLZ"
replace pais=18 if New_Region=="BOL"
replace pais=19 if New_Region=="BRA"
replace pais=20 if New_Region=="BTN"
replace pais=21 if New_Region=="BWA"
replace pais=22 if New_Region=="CAF"
replace pais=23 if New_Region=="CAN"
replace pais=24 if New_Region=="CHL"
replace pais=25 if New_Region=="CHM"
replace pais=26 if New_Region=="CHP"
replace pais=27 if New_Region=="CIV"
replace pais=28 if New_Region=="CMR"
replace pais=29 if New_Region=="COD"
replace pais=30 if New_Region=="COG"
replace pais=31 if New_Region=="COL"
replace pais=32 if New_Region=="CRB"
replace pais=33 if New_Region=="CRI"
replace pais=34 if New_Region=="CUB"
replace pais=35 if New_Region=="CYP"
replace pais=36 if New_Region=="CZE"
replace pais=37 if New_Region=="DEU"
replace pais=38 if New_Region=="DJI"
replace pais=39 if New_Region=="DNK"
replace pais=40 if New_Region=="DOM"
replace pais=41 if New_Region=="DZA"
replace pais=42 if New_Region=="ECU"
replace pais=43 if New_Region=="EGY"
replace pais=44 if New_Region=="ERI"
replace pais=45 if New_Region=="ETH"
replace pais=46 if New_Region=="FJI"
replace pais=47 if New_Region=="FNP"
replace pais=48 if New_Region=="FRP"
replace pais=49 if New_Region=="GAB"
replace pais=50 if New_Region=="GEO"
replace pais=51 if New_Region=="GHA"
replace pais=52 if New_Region=="GIN"
replace pais=53 if New_Region=="GMB"
replace pais=54 if New_Region=="GNB"
replace pais=55 if New_Region=="GNQ"
replace pais=56 if New_Region=="GRC"
replace pais=57 if New_Region=="GRL"
replace pais=58 if New_Region=="GSA"
replace pais=59 if New_Region=="GTM"
replace pais=60 if New_Region=="HND"
replace pais=61 if New_Region=="HRV"
replace pais=62 if New_Region=="HTI"
replace pais=63 if New_Region=="HUN"
replace pais=64 if New_Region=="IDN"
replace pais=65 if New_Region=="IND"
replace pais=66 if New_Region=="IRL"
replace pais=67 if New_Region=="IRN"
replace pais=68 if New_Region=="IRQ"
replace pais=69 if New_Region=="ISL"
replace pais=70 if New_Region=="ISR"
replace pais=71 if New_Region=="ITP"
replace pais=72 if New_Region=="JAM"
replace pais=73 if New_Region=="JOR"
replace pais=74 if New_Region=="JPN"
replace pais=75 if New_Region=="KAZ"
replace pais=76 if New_Region=="KEN"
replace pais=77 if New_Region=="KGZ"
replace pais=78 if New_Region=="KHM"
replace pais=79 if New_Region=="KOR"
replace pais=80 if New_Region=="LAO"
replace pais=81 if New_Region=="LBN"
replace pais=82 if New_Region=="LBR"
replace pais=83 if New_Region=="LBY"
replace pais=84 if New_Region=="LKA"
replace pais=85 if New_Region=="LSO"
replace pais=86 if New_Region=="MDA"
replace pais=87 if New_Region=="MDG"
replace pais=88 if New_Region=="MEX"
replace pais=89 if New_Region=="MLI"
replace pais=90 if New_Region=="MMR"
replace pais=91 if New_Region=="MNG"
replace pais=92 if New_Region=="MOR"
replace pais=93 if New_Region=="MOZ"
replace pais=94 if New_Region=="MRT"
replace pais=95 if New_Region=="MWI"
replace pais=96 if New_Region=="MYS"
replace pais=97 if New_Region=="NAM"
replace pais=98 if New_Region=="NER"
replace pais=99 if New_Region=="NGA"
replace pais=100 if New_Region=="NIC"
replace pais=101 if New_Region=="NLD"
replace pais=102 if New_Region=="NOR"
replace pais=103 if New_Region=="NPL"
replace pais=104 if New_Region=="NZL"
replace pais=105 if New_Region=="OAO"
replace pais=106 if New_Region=="OBN"
replace pais=107 if New_Region=="OIO"
replace pais=108 if New_Region=="OPO"
replace pais=109 if New_Region=="OSA"
replace pais=110 if New_Region=="PAK"
replace pais=111 if New_Region=="PAN"
replace pais=112 if New_Region=="PER"
replace pais=113 if New_Region=="PHL"
replace pais=114 if New_Region=="PNG"
replace pais=115 if New_Region=="POL"
replace pais=116 if New_Region=="PRK"
replace pais=117 if New_Region=="PRT"
replace pais=118 if New_Region=="PRY"
replace pais=119 if New_Region=="PSE"
replace pais=120 if New_Region=="RAP"
replace pais=121 if New_Region=="ROU"
replace pais=122 if New_Region=="RUS"
replace pais=123 if New_Region=="RWA"
replace pais=124 if New_Region=="SAU"
replace pais=125 if New_Region=="SDN"
replace pais=126 if New_Region=="SEN"
replace pais=127 if New_Region=="SLB"
replace pais=128 if New_Region=="SLE"
replace pais=129 if New_Region=="SLV"
replace pais=130 if New_Region=="SOM"
replace pais=131 if New_Region=="SPP"
replace pais=132 if New_Region=="SVK"
replace pais=133 if New_Region=="SVN"
replace pais=134 if New_Region=="SWE"
replace pais=135 if New_Region=="SWZ"
replace pais=136 if New_Region=="SYR"
replace pais=137 if New_Region=="TCD"
replace pais=138 if New_Region=="TGO"
replace pais=139 if New_Region=="THA"
replace pais=140 if New_Region=="TJK"
replace pais=141 if New_Region=="TKM"
replace pais=142 if New_Region=="TLS"
replace pais=143 if New_Region=="TUN"
replace pais=144 if New_Region=="TUR"
replace pais=145 if New_Region=="TZA"
replace pais=146 if New_Region=="UGA"
replace pais=147 if New_Region=="UKP"
replace pais=148 if New_Region=="UKR"
replace pais=149 if New_Region=="URY"
replace pais=150 if New_Region=="USA"
replace pais=151 if New_Region=="UZB"
replace pais=152 if New_Region=="VEN"
replace pais=153 if New_Region=="VNM"
replace pais=154 if New_Region=="VUT"
replace pais=155 if New_Region=="YEM"
replace pais=156 if New_Region=="ZAF"
replace pais=157 if New_Region=="ZMB"
replace pais=158 if New_Region=="ZWE"
replace pais=159 if New_Region=="SSD"

#delimit
;
label define pais  1"AFG" 2"AGO" 3"ALB" 4"ARG" 5"ARM" 6"AUS" 7"AUT" 8"AZE"
9"BDI" 10"BEN" 11"BFA" 12"BGD" 13"BGR" 14"BLR" 15"BLT" 16"BLX"
17"BLZ" 18"BOL" 19"BRA" 20"BTN" 21"BWA" 22"CAF" 23"CAN" 24"CHL"
25"CHM" 26"CHP" 27"CIV" 28"CMR" 29"COD" 30"COG" 31"COL" 32"CRB" 33"CRI"
34"CUB" 35"CYP" 36"CZE" 37"DEU" 38"DJI" 39"DNK" 40"DOM" 41"DZA" 42"ECU"
43"EGY" 44"ERI" 45"ETH" 46"FJI" 47"FNP" 48"FRP" 49"GAB" 50"GEO" 51"GHA"
52"GIN" 53"GMB" 54"GNB" 55"GNQ" 56"GRC" 57"GRL" 58"GSA" 59"GTM" 60"HND"
61"HRV" 62"HTI" 63"HUN" 64"IDN" 65"IND" 66"IRL" 67"IRN" 68"IRQ" 69"ISL"
70"ISR"  71"ITP" 72"JAM" 73"JOR" 74"JPN" 75"KAZ" 76"KEN" 77"KGZ" 78"KHM"
79"KOR" 80"LAO" 81"LBN" 82"LBR" 83"LBY" 84"LKA" 85"LSO" 86"MDA" 87"MDG"
88"MEX" 89"MLI" 90"MMR" 91"MNG" 92"MOR" 93"MOZ" 94"MRT" 95"MWI" 96"MYS"
97"NAM" 98"NER" 99"NGA" 100"NIC" 101"NLD" 102"NOR" 103"NPL" 104"NZL"
105"OAO" 106"OBN" 107"OIO" 108"OPO" 109"OSA" 110"PAK" 111"PAN" 112"PER"
113"PHL" 114"PNG" 115"POL" 116"PRK" 117"PRT" 118"PRY" 119"PSE" 120"RAP"
121"ROU" 122"RUS"123"RWA"  124"SAU" 125"SDN" 126"SEN" 127"SLB" 128"SLE"
129"SLV" 130"SOM" 131"SPP" 132"SVK" 133"SVN" 134"SWE" 135"SWZ" 136"SYR"
137"TCD" 138"TGO" 139"THA" 140"TJK" 141"TKM" 142"TLS" 143"TUN" 144"TUR" 
145"TZA" 146"UGA" 147"UKP" 148"UKR" 149"URY" 150"USA" 151"UZB" 152"VEN"
153"VNM" 154"VUT" 155"YEM" 156"ZAF" 157"ZMB" 158"ZWE"  159"SSD"

;
 #delimit cr 

label value pais pais 
tab pais
drop Rural_UN20 Urban_UN20 Total_UN20 Rural_Pop_ Urban_Pop_
format %30s gcm

*******regiones FAO
gen regionfao=.
replace regionfao=1 if inlist(pais,6,104) 
replace regionfao=2 if inlist(pais, 32, 34, 40, 62, 72 )
replace regionfao=3 if inlist(pais, 17, 33, 59, 60, 88, 100, 111, 129)
replace regionfao=4 if inlist(pais, 75, 77, 140, 141, 151)
replace regionfao=5 if inlist(pais, 9, 38, 44, 45, 76, 87, 93, 95, 123, 130, 145, 146, 157, 158, 159)
replace regionfao=6 if inlist(pais, 25, 74, 79 ,91 ,116)
replace regionfao=7 if inlist(pais, 13, 14,36, 63, 86, 106, 115, 121, 122, 132, 148)
replace regionfao=8 if inlist(pais, 46,  114, 127, 154 )
replace regionfao=9 if inlist(pais, 2, 22, 28, 29, 30, 49, 55, 137)
replace regionfao=10 if inlist(pais,41, 43, 83, 92, 125, 143)
replace regionfao=11 if inlist(pais, 23, 57, 150)
replace regionfao=12 if inlist(pais,15, 39, 47, 66, 69, 102, 134, 147)
replace regionfao=13 if inlist(pais,108) 
replace regionfao=14 if inlist(pais, 4, 18, 19 ,24, 31 ,42 ,58 ,112, 118 ,149 ,152)
replace regionfao=15 if inlist(pais, 64, 78, 80, 90, 96, 109, 113, 139, 142, 153)
replace regionfao=16 if inlist(pais, 21, 85, 97, 135, 156)
replace regionfao=17 if inlist(pais, 1, 12, 20, 65, 67, 84, 103, 107, 110)
replace regionfao=18 if inlist(pais, 3, 56, 61, 71, 117, 131, 133)
replace regionfao=19 if inlist(pais, 10, 11, 27, 51, 52, 53, 54, 82, 89, 94, 98, 99, 126, 128, 138)
replace regionfao=20 if inlist(pais, 5, 8, 35, 50, 68, 70, 73,81, 119, 120, 124, 136, 144, 155)
replace regionfao=21 if inlist(pais, 7, 16, 26, 37, 48, 101, 105)



# delimit
;
label define regionfao 1"Australia and New Zealand" 2"Caribbean" 3"Central America"
4"Central Asia" 5"Eastern Africa" 6"Eastern Asia" 7"Eastern Europe"
8"Melanesia" 9"Middle Africa" 10"Northern Africa" 11"Northern America"
12"Northern Europe" 13"Oceania" 14"South America" 15"South-Eastern Asia"
16"Southern Africa" 17"Southern Asia" 18"Southern Europe" 19"Western Africa"
20"Western Asia" 21"Western Europe" 
;
#delimit cr
label value regionfao regionfao 
drop if regionfao==.
save "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\\BASEpaises.dta", replace


************** calcular la media de rendimientos por pais y por gcm 
****mean por region FAO
use "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\\BASEpaises.dta",clear
collapse (mean) yield if sis==2 & yield!=0 & cccom=="bean", by ( regionfao yrs gcm)
save "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\\CollapseRegionFAO.dta", replace
clear

****mean por pais
use "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\\BASEpaises.dta",clear
collapse (mean) yield if sis==2 & yield!=0 & cccom=="bean", by ( pais  yrs gcm)
save "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\\CollapseRegionPais.dta", replace
clear

*******************************************************************************************
*******************************************************************************************

**** datos FAO
clear
local regiones `""Eastern Africa" "Middle Africa" "Northern Africa" "Southern Africa" "Western Africa" "Northern America" "Central America" "Caribbean" "South America" "Central Asia" "Eastern Asia" "Southern Asia" "South-Eastern Asia" "Western Asia""'
foreach var of local regiones {
import excel "C:\Users\CEGONZALEZ\Documents\GFSF\datafao.xlsx", sheet("`var'") firstrow
rename Value yield
gen y = yield*0.1
drop yield
drop if y==.
reshape wide y, i(zona AreaName) j(Year)
save "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\FAO_`var'.dta", replace 
clear
}
*end

*******************************************************************************
******append
clear
cd "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO"
append using "FAO_Eastern Africa.dta" ///
"FAO_Middle Africa.dta" ///
"FAO_Northern Africa.dta" ///
"FAO_Southern Africa.dta" ///
"FAO_Western Africa.dta" ///
"FAO_Northern America.dta" ///
"FAO_Central America.dta" ///
"FAO_Caribbean.dta" ///
"FAO_South America.dta" ///
"FAO_Central Asia.dta" ///
"FAO_Eastern Asia.dta" ///
"FAO_Southern Asia.dta" ///
"FAO_South-Eastern Asia.dta" ///
"FAO_Western Asia.dta", nolabel  
save "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\TotalFAO.dta", replace 

******
gen regionfao=.
replace regionfao=2 if zona=="Caribbean" 
replace regionfao=3 if zona=="Central America"
replace regionfao=4 if zona=="Central Asia"
replace regionfao=5 if zona=="Eastern Africa"
replace regionfao=6 if zona=="Eastern Asia"
replace regionfao=9 if zona=="Middle Africa"
replace regionfao=10 if zona=="Northern Africa"
replace regionfao=11 if zona=="Northern America"
replace regionfao=14 if zona=="South America"
replace regionfao=15 if zona=="South-Eastern Asia"
replace regionfao=16 if zona=="Southern Africa"
replace regionfao=17 if zona=="Southern Asia"
replace regionfao=19 if zona=="Western Africa"
replace regionfao=20 if zona=="Western Asia "

#delimit 
;
label define regionfao 2"Caribbean" 3"Central America" 4"Central Asia" 5"Eastern Africa"
6"Eastern Asia" 9"Middle Africa" 10"Northern Africa" 11"Northern America" 14"South America"
15"South-Eastern Asia" 16"Southern Africa" 17"Southern Asia" 19"Western Africa" 20"Western Asia"
;
#delimit cr
label value regionfao regionfao 


save "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\TotalFAO.dta", replace 
**************** media entre las filas
egen meany= rmean(y*)
**************** valor mas algo de toda la serie
egen maxy= rmax(y*)
egen miny= rmin(y*)

save "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\TotalFAO.dta", replace 
****************


****collapse

***mean
use "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\TotalFAO.dta", clear
collapse (mean) meany , by (regionfao)
save "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\TotalFAOmeany.dta", replace 

***max
use "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\TotalFAO.dta", clear
collapse (mean) maxy , by (regionfao)
save "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\TotalFAOmaxy.dta", replace 

***min
use "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\TotalFAO.dta", clear
collapse (mean) miny , by (regionfao)
save "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\TotalFAOminy.dta", replace 


*****merge
clear
use "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\TotalFAOmaxy.dta", clear
merge 1:1 regionfao using "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\TotalFAOmeany.dta"
drop _merge
save "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\merge", replace 
merge 1:1 regionfao using "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\TotalFAOminy.dta"
drop _merge
save "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\mergeFAO.dta", replace


**********************************************************************************************
**********************************************************************************************
**********************************************************************************************
**********************************************************************************************
****Merge
*****BaseNormal


cd"C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp"
use "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\\CollapseRegionFAO.dta", clear
reshape wide yield, i(regionfao gcm) j(yrs)
keep if gcm=="BeanNormal"
merge 1:1 regionfao using "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\mergeFAO.dta"
drop if _merge==1
drop yield2050
save "CgiarFAONormalB.dta", replace


*****BaseTech
use "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\\CollapseRegionFAO.dta", clear
reshape wide yield, i(regionfao gcm) j(yrs)
keep if gcm=="BeanTechn"
merge 1:1 regionfao using "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\mergeFAO.dta"
drop if _merge==1
drop yield2050
save "CgiarFAOTechnB.dta", replace


*****Gms 
cd"C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp"
use "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\\CollapseRegionFAO.dta", clear
reshape wide yield, i(regionfao gcm) j(yrs)
keep if gcm=="BeanTechn"
merge 1:1 regionfao using "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\mergeFAO.dta"
drop if _merge==1
drop yield2050
save "BaseCgiarFAOBeanTechn.dta", replace


use "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\\CollapseRegionFAO.dta", clear
local gcm BeanNormalgfdl_esm2m BeanNormalhadgem2_es BeanNormalipsl_cm5a_lr BeanNormalmiroc_esm_chem BeanNormalnoresm1_m BeanTechngfdl_esm2m BeanTechnhadgem2_es BeanTechnipsl_cm5a_lr BeanTechnmiroc_esm_chem BeanTechnnoresm1_m__
foreach x in `gcm' {
reshape wide yield, i(regionfao gcm) j(yrs)
keep if gcm=="`x'"
merge 1:1 regionfao using "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\mergeFAO.dta"
drop if _merge==1
rename yield2050 y`x'
drop yield2005 
drop if regionfao==2
drop _merge
gen cmax`x'= y`x'-maxy
gen cmin`x'= y`x'-miny
gen cmen`x'= y`x'-meany
gen Chcmax`x'= (cmax`x'/maxy)*100
gen Chcmin`x'= (cmin`x'/miny)*100
gen Chcmen`x'= (cmen`x'/meany)*100
drop gcm
save "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\`x'.dta", replace
use "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\\CollapseRegionFAO.dta", clear
}
*end


****** merge
local path C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\
foreach name_n in BeanNormalgfdl_esm2m BeanNormalhadgem2_es BeanNormalipsl_cm5a_lr  /*
			*/ BeanNormalmiroc_esm_chem BeanNormalnoresm1_m BeanTechngfdl_esm2m BeanTechnhadgem2_es /*
			*/BeanTechnipsl_cm5a_lr BeanTechnmiroc_esm_chem BeanTechnnoresm1_m__{
merge 1:1 regionfao maxy meany miny using "`path'\`name_n'.dta"
drop _merge
save "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\m_`name_n'.dta", replace
}
*end
save "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\mergeCgiarFAO.dta", replace
use "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\mergeCgiarFAO.dta", clear

gen pot=.
replace pot=1 if inlist(regionfao,3, 11, 14)
replace pot=2 if inlist(regionfao,4, 6, 15, 17, 20) 
replace pot=3 if inlist(regionfao, 5, 9, 10, 16, 19)
label define pot 1"America" 2"Asia" 3"Africa" 
label value pot pot 
save "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\mergeCgiarFAO.dta", replace
* 1 = America
* 2 = Asia
* 3 = Africa 










******************************************************************************************************
******************************************************************************************************
******************************************************************************************************
******************************************************************************************************
use "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\mergeCgiarFAO.dta", clear

*********** Graficos

*****hadgem2
local t NORMAL
local g Normalhadgem2_es  
forvalue  p= 1/3{
#delimit 
;
graph bar (mean) cmaxBeanNormalhadgem2_es cminBeanNormalhadgem2_es cmenBeanNormalhadgem2_es if pot==`p', 
over(regionfao, sort(1) label(labsize(small) angle(h))) 
bar(1, bcolor(blue)) bar(2, bcolor(red)) 
yline(0, lcolor(black)) bargap(-30)
ytitle("{it: Differences kg/ha  }" )
title( "{stMono: BEAN `t'}" "{it: Distance: FAO yields and CGIAR yields}" "`p'")
ylabel(#10, angle(horizontal) labsize(small)) 
legend(label(1 "MAX") label(2 "MIN")  label(3 "MEAN") rows(1))
note("`g'")
;
#delimit cr
graph save "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\`p'`g'.gph", replace
graph export "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\`p'`g'.png", width(4000) replace 
}
*end




***** Techadgem2
local w TECH
local g Techhadgem2
forvalue  p= 1/3{
#delimit 
;
graph bar (mean) cmaxBeanTechnnoresm1_m__ cminBeanTechnnoresm1_m__ cmenBeanTechnnoresm1_m__ if pot==`p', 
over(regionfao, sort(1) label(labsize(small) angle(h))) 
bar(1, bcolor(blue)) bar(2, bcolor(red)) 
yline(0, lcolor(black)) bargap(-30)
ytitle("{it: Differences kg/ha  }" )
title( "{stMono: BEAN `w'}" "{it: Distance: FAO yields and CGIAR yields}" "`p'")
ylabel(#10, angle(horizontal) labsize(small)) 
legend(label(1 "MAX") label(2 "MIN")  label(3 "MEAN") rows(1))
note("`g'")
;
#delimit cr
graph save "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\`p'`g'.gph", replace
graph export "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\`p'`g'.png", width(4000) replace 
}
*end


*****************************************************************************************
*****************************************************************************************
*****noresm1_m__
local t NORMAL
local g Normalnoresm1_m__ 
forvalue  p= 1/3{
#delimit 
;
graph bar (mean) cmaxBeanNormalnoresm1_m cminBeanNormalnoresm1_m cmenBeanNormalnoresm1_m if pot==`p', 
over(regionfao, sort(1) label(labsize(small) angle(h))) 
bar(1, bcolor(blue)) bar(2, bcolor(red)) 
yline(0, lcolor(black)) bargap(-30)
ytitle("{it: Differences kg/ha  }" )
title( "{stMono: BEAN `t'}" "{it: Distance: FAO yields and CGIAR yields}" "`p'")
ylabel(#10, angle(horizontal) labsize(small)) 
legend(label(1 "MAX") label(2 "MIN")  label(3 "MEAN") rows(1))
note("`g'")
;
#delimit cr
graph save "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\`p'`g'.gph", replace
graph export "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\`p'`g'.png", width(4000) replace 
}
*end
use "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\mergeCgiarFAO.dta", clear

***** Techadgem2
local w TECH
local g Technoresm1_m__
forvalue  p= 1/3{
#delimit 
;
graph bar (mean) cmaxBeanTechnnoresm1_m__ cminBeanTechnnoresm1_m__ cmenBeanTechnnoresm1_m__  if pot==`p', 
over(regionfao, sort(1) label(labsize(small) angle(h))) 
bar(1, bcolor(blue)) bar(2, bcolor(red)) 
yline(0, lcolor(black)) bargap(-30)
ytitle("{it: Differences kg/ha  }" )
title( "{stMono: BEAN `w'}" "{it: Distance: FAO yields and CGIAR yields}" "`p'")
ylabel(#10, angle(horizontal) labsize(small)) 
legend(label(1 "MAX") label(2 "MIN")  label(3 "MEAN") rows(1))
note("`g'")
;
#delimit cr
graph save "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\p'`g'.gph", replace
graph export "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\p'`g'.png", width(4000) replace 
}
*end
*****************************************************************************************
*****************************************************************************************

*****ipsl_cm5a_lr 
local t NORMAL
local g Normalipsl_cm5a_lr 
forvalue  p= 1/3{
#delimit 
;
graph bar (mean) cmaxBeanNormalipsl_cm5a_lr cminBeanNormalipsl_cm5a_lr cmenBeanNormalipsl_cm5a_lr 
 if pot==`p', 
over(regionfao, sort(1) label(labsize(small) angle(h))) 
bar(1, bcolor(blue)) bar(2, bcolor(red)) 
yline(0, lcolor(black)) bargap(-30)
ytitle("{it: Differences kg/ha  }" )
title( "{stMono: BEAN `t'}" "{it: Distance: FAO yields and CGIAR yields}" "`p'")
ylabel(#10, angle(horizontal) labsize(small)) 
legend(label(1 "MAX") label(2 "MIN")  label(3 "MEAN") rows(1))
note("`g'")
;
#delimit cr
graph save "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\`p'`g'.gph", replace
graph export "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\`p'`g'.png", width(4000) replace 
}
*end


local w TECH
local g Techipsl_cm5a_lr
forvalue  p= 1/3{
#delimit 
;
graph bar (mean) cmaxBeanTechnipsl_cm5a_lr cminBeanTechnipsl_cm5a_lr cmenBeanTechnipsl_cm5a_lr  if pot==`p', 
over(regionfao, sort(1) label(labsize(small) angle(h))) 
bar(1, bcolor(blue)) bar(2, bcolor(red)) 
yline(0, lcolor(black)) bargap(-30)
ytitle("{it: Differences kg/ha  }" )
title( "{stMono: BEAN `w'}" "{it: Distance: FAO yields and CGIAR yields}" "`p'")
ylabel(#10, angle(horizontal) labsize(small)) 
legend(label(1 "MAX") label(2 "MIN")  label(3 "MEAN") rows(1))
note("`g'")
;
#delimit cr
graph save "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\`p'`g'.gph", replace
graph export "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\`p'`g'.png", width(4000) replace 
}
*end	

*****************************************************************************************
*****************************************************************************************
*****************************************************************************************
*****************************************************************************************


*****miroc_esm_chem 
local t NORMAL
local g Normalmiroc_esm_chem
forvalue  p= 1/3{
#delimit 
;
graph bar (mean) cmaxBeanNormalmiroc_esm_chem cminBeanNormalmiroc_esm_chem cmenBeanNormalmiroc_esm_chem  if pot==`p', 
over(regionfao, sort(1) label(labsize(small) angle(h))) 
bar(1, bcolor(blue)) bar(2, bcolor(red)) 
yline(0, lcolor(black)) bargap(-30)
ytitle("{it: Differences kg/ha  }" )
title( "{stMono: BEAN `t'}" "{it: Distance: FAO yields and CGIAR yields}" "`p'")
ylabel(#10, angle(horizontal) labsize(small)) 
legend(label(1 "MAX") label(2 "MIN")  label(3 "MEAN") rows(1))
note("`g'")
;
#delimit cr
graph save "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\`p'`g'.gph", replace
graph export "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\`p'`g'.png", width(4000) replace 
}
*end
use "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\mergeCgiarFAO.dta", clear

local w TECH
local g Techmiroc_esm_chem
forvalue  p= 1/3{
#delimit 
;
graph bar (mean) cmaxBeanTechnmiroc_esm_chem cminBeanTechnmiroc_esm_chem cmenBeanTechnmiroc_esm_chem  if pot==`p', 
over(regionfao, sort(1) label(labsize(small) angle(h))) 
bar(1, bcolor(blue)) bar(2, bcolor(red)) 
yline(0, lcolor(black)) bargap(-30)
ytitle("{it: Differences kg/ha  }" )
title( "{stMono: BEAN `w'}" "{it: Distance: FAO yields and CGIAR yields}" "`p'")
ylabel(#10, angle(horizontal) labsize(small)) 
legend(label(1 "MAX") label(2 "MIN")  label(3 "MEAN") rows(1))
note("`g'")
;
#delimit cr
graph save "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\`p'`g'.gph", replace
graph export "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\`p'`g'.png", width(4000) replace 
}
*end	


*****************************************************************************************
*****************************************************************************************
*****************************************************************************************
*****gfdl_esm2m 
use "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\mergeCgiarFAO.dta", clear

local t NORMAL
local g Normalgfdl_esm2m
forvalue  p= 1/3{
#delimit 
;
graph bar (mean)cmaxBeanNormalgfdl_esm2m cminBeanNormalgfdl_esm2m cmenBeanNormalgfdl_esm2m  if pot==`p', 
over(regionfao, sort(1) label(labsize(small) angle(h))) 
bar(1, bcolor(blue)) bar(2, bcolor(red)) 
yline(0, lcolor(black)) bargap(-30)
ytitle("{it: Differences kg/ha  }" )
title( "{stMono: BEAN `t'}" "{it: Distance: FAO yields and CGIAR yields}" "`p'")
ylabel(#10, angle(horizontal) labsize(small)) 
legend(label(1 "MAX") label(2 "MIN")  label(3 "MEAN") rows(1))
note("`g'")
;
#delimit cr
graph save "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\`p'`g'.gph", replace
graph export "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\`p'`g'.png", width(4000) replace 
}
*end

local w TECH
local g Techgfdl_esm2m
forvalue  p= 1/3{
#delimit 
;
graph bar (mean) cmaxBeanTechngfdl_esm2m cminBeanTechngfdl_esm2m cmenBeanTechngfdl_esm2m  if pot==`p', 
over(regionfao, sort(1) label(labsize(small) angle(h))) 
bar(1, bcolor(blue)) bar(2, bcolor(red)) 
yline(0, lcolor(black)) bargap(-30)
ytitle("{it: Differences kg/ha  }" )
title( "{stMono: BEAN `w'}" "{it: Distance: FAO yields and CGIAR yields}" "`p'")
ylabel(#10, angle(horizontal) labsize(small)) 
legend(label(1 "MAX") label(2 "MIN")  label(3 "MEAN") rows(1))
note("`g'")
;
#delimit cr
graph save "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\`p'`g'.gph", replace
graph export "C:\Users\CEGONZALEZ\Documents\GFSF\Graphs\\`p'`g'.png", width(4000) replace 
}
*end	

*************************************************************************************
*************************************************************************************

****datos incrementos
use "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\mergeCgiarFAO.dta", clear
***despues de eliminar las varibles cambios


save "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\ChangeCgiarFAO.dta", replace
use  "C:\Users\CEGONZALEZ\Documents\GFSF\DTATemp\FAO\\ChangeCgiarFAO.dta", clear



/*
ChcminBeanTechngfdl_esm2m	
ChcmenBeanTechngfdl_esm2m	
ChcmaxBeanTechnhadgem2_es
ChcminBeanTechnhadgem2_es
ChcmenBeanTechnhadgem2_es
ChcmaxBeanTechnipsl_cm5a_lr
ChcminBeanTechnipsl_cm5a_lr
ChcmenBeanTechnipsl_cm5a_lr
ChcmaxBeanTechnmiroc_esm_chem
ChcminBeanTechnmiroc_esm_chem
ChcmenBeanTechnmiroc_esm_chem
ChcmaxBeanTechnnoresm1_m__
ChcminBeanTechnnoresm1_m__
ChcmenBeanTechnnoresm1_m__

ChcmaxBeanNormalgfdl_esm2m	
ChcminBeanNormalgfdl_esm2m	
ChcmenBeanNormalgfdl_esm2m	
ChcmaxBeanNormalhadgem2_es	
ChcminBeanNormalhadgem2_es	
ChcmenBeanNormalhadgem2_es	
ChcmaxBeanNormalipsl_cm5a_lr	
ChcminBeanNormalipsl_cm5a_lr	
ChcmenBeanNormalipsl_cm5a_lr	
ChcmaxBeanNormalmiroc_esm_chem	
ChcminBeanNormalmiroc_esm_chem	
ChcmenBeanNormalmiroc_esm_chem	
ChcmaxBeanNormalnoresm1_m	
ChcminBeanNormalnoresm1_m	
ChcmenBeanNormalnoresm1_m	
												


