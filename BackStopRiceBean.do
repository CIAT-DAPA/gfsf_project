*****BackStop

set more off
*****BEANS
******************************************************* PASO 1. 
****abrir datos en excel y grabarlos como STATA
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS" 
local files : dir "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS" files "*.txt"
display `files'
foreach file in `files' {
    import delimited using "`file'", clear
    local noextension=subinstr("`file'",".txt","",.)
    save "`noextension'", replace
	clear
}
end 

********************************************************** PASO 2. 


************************************************************************************************************************

***frijol

******************************************************* PASO 2. 
***  renombrar variables  para la creación de los variables id
local files: dir "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS" files "*.dta" 
foreach file in `files'{
use `file', clear
rename fpu zona
gen fpu=.
rename irrf sys
rename cccom cropbean
save `file', replace
clear
}
end

******************************************************* PASO 3. 
*** Value para las variables  por FPU para lograr identificarlas
local files: dir "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS" files "*.dta" 
foreach file in `files'{
use `file', clear
replace fpu=1 if zona=="ALB_ALB"
replace fpu=2 if zona=="ALK_USA"
replace fpu=3 if zona=="AMA_BOL"
replace fpu=4 if zona=="AMA_BRA"
replace fpu=5 if zona=="AMA_COL"
replace fpu=6 if zona=="AMA_ECU"
replace fpu=7 if zona=="AMA_PER"
replace fpu=8 if zona=="AMD_AFG"
replace fpu=9 if zona=="AMD_KAZ"
replace fpu=10 if zona=="AMD_TJK"
replace fpu=11 if zona=="AMD_TKM"
replace fpu=12 if zona=="AMD_UZB"
replace fpu=13 if zona=="AMR_CHM"
replace fpu=14 if zona=="AMR_RUS"
replace fpu=15 if zona=="ARA_IRQ"
replace fpu=16 if zona=="ARK_USA"
replace fpu=17 if zona=="ARM_ARM"
replace fpu=18 if zona=="AZE_AZE"
replace fpu=19 if zona=="BAL_BLT"
replace fpu=20 if zona=="BAL_RUS"
replace fpu=21 if zona=="BLA_RUS"
replace fpu=22 if zona=="BLA_TUR"
replace fpu=23 if zona=="BLA_UKR"
replace fpu=24 if zona=="BLZ_BLZ"
replace fpu=25 if zona=="BOR_IDN"
replace fpu=26 if zona=="BOR_MYS"
replace fpu=27 if zona=="BRT_BGD"
replace fpu=28 if zona=="BRT_BTN"
replace fpu=29 if zona=="BRT_CHM"
replace fpu=30 if zona=="BRT_IND"
replace fpu=31 if zona=="CAF_AGO"
replace fpu=32 if zona=="CAF_CAF"
replace fpu=33 if zona=="CAF_CMR"
replace fpu=34 if zona=="CAF_COG"
replace fpu=35 if zona=="CAF_GAB"
replace fpu=36 if zona=="CAF_GNQ"
replace fpu=37 if zona=="CAF_NAM"
replace fpu=38 if zona=="CAL_USA"
replace fpu=39 if zona=="CAN_CAN"
replace fpu=40 if zona=="CAU_AUS"
replace fpu=41 if zona=="CAV_IND"
replace fpu=42 if zona=="CHC_CHL"
replace fpu=43 if zona=="CHJ_CHM"
replace fpu=44 if zona=="CHO_IND"
replace fpu=45 if zona=="COB_CAN"
replace fpu=46 if zona=="COB_USA"
replace fpu=47 if zona=="COL_USA"
replace fpu=48 if zona=="CON_AGO"
replace fpu=49 if zona=="CON_CAF"
replace fpu=50 if zona=="CON_COD"
replace fpu=51 if zona=="CON_COG"
replace fpu=52 if zona=="CRB_CRB"
replace fpu=53 if zona=="CRI_CRI"
replace fpu=54 if zona=="CUB_CUB"
replace fpu=55 if zona=="DAN_AUT"
replace fpu=56 if zona=="DAN_BGR"
replace fpu=57 if zona=="DAN_CZE"
replace fpu=58 if zona=="DAN_DEU"
replace fpu=59 if zona=="DAN_HRV"
replace fpu=60 if zona=="DAN_HUN"
replace fpu=61 if zona=="DAN_MDA"
replace fpu=62 if zona=="DAN_OBN"
replace fpu=63 if zona=="DAN_ROU"
replace fpu=64 if zona=="DAN_SVK"
replace fpu=65 if zona=="DAN_SVN"
replace fpu=66 if zona=="DAN_TUR"
replace fpu=67 if zona=="DAN_UKR"
replace fpu=68 if zona=="DNI_BLR"
replace fpu=69 if zona=="DNI_RUS"
replace fpu=70 if zona=="DNI_UKR"
replace fpu=71 if zona=="DOM_DOM"
replace fpu=72 if zona=="EAC_BDI"
replace fpu=73 if zona=="EAC_COD"
replace fpu=74 if zona=="EAC_RWA"
replace fpu=75 if zona=="EAC_TZA"
replace fpu=76 if zona=="EAU_AUS"
replace fpu=77 if zona=="EGH_IND"
replace fpu=78 if zona=="ELB_DEU"
replace fpu=79 if zona=="ELB_DNK"
replace fpu=80 if zona=="EME_CYP"
replace fpu=81 if zona=="EME_EGY"
replace fpu=82 if zona=="EME_ISR"
replace fpu=83 if zona=="EME_JOR"
replace fpu=84 if zona=="EME_LBN"
replace fpu=85 if zona=="EME_PSE"
replace fpu=86 if zona=="EME_SYR"
replace fpu=87 if zona=="EME_TUR"
replace fpu=88 if zona=="FJI_FJI"
replace fpu=89 if zona=="FNP_FNP"
replace fpu=90 if zona=="GAN_BGD"
replace fpu=91 if zona=="GAN_CHM"
replace fpu=92 if zona=="GAN_IND"
replace fpu=93 if zona=="GAN_NPL"
replace fpu=94 if zona=="GBA_USA"
replace fpu=95 if zona=="GEO_GEO"
replace fpu=96 if zona=="GLA_CAN"
replace fpu=97 if zona=="GLA_USA"
replace fpu=98 if zona=="GOD_IND"
replace fpu=99 if zona=="GRC_GRC"
replace fpu=100 if zona=="GRL_GRL"
replace fpu=101 if zona=="GSA_GSA"
replace fpu=102 if zona=="GTM_GTM"
replace fpu=103 if zona=="HAI_CHM"
replace fpu=104 if zona=="HND_HND"
replace fpu=105 if zona=="HOA_ETH"
replace fpu=106 if zona=="HOA_KEN"
replace fpu=107 if zona=="HOA_SOM"
replace fpu=108 if zona=="HTI_HTI"
replace fpu=109 if zona=="HUA_CHM"
replace fpu=110 if zona=="HUN_CHM"
replace fpu=111 if zona=="HWI_USA"
replace fpu=112 if zona=="IEC_IND"
replace fpu=113 if zona=="IND_CHM"
replace fpu=114 if zona=="IND_IND"
replace fpu=115 if zona=="IND_PAK"
replace fpu=116 if zona=="INE_IDN"
replace fpu=117 if zona=="INW_IDN"
replace fpu=118 if zona=="IRL_IRL"
replace fpu=119 if zona=="ISL_ISL"
replace fpu=120 if zona=="ITA_ITP"
replace fpu=121 if zona=="JAM_JAM"
replace fpu=122 if zona=="JAP_JPN"
replace fpu=123 if zona=="KAL_BWA"
replace fpu=124 if zona=="KAL_NAM"
replace fpu=125 if zona=="KAL_ZAF"
replace fpu=126 if zona=="KRI_IND"
replace fpu=127 if zona=="LAJ_CHM"
replace fpu=128 if zona=="LBA_KAZ"
replace fpu=129 if zona=="LBA_KGZ"
replace fpu=130 if zona=="LBO_FRP"
replace fpu=131 if zona=="LCB_CAF"
replace fpu=132 if zona=="LCB_CMR"
replace fpu=133 if zona=="LCB_NER"
replace fpu=134 if zona=="LCB_NGA"
replace fpu=135 if zona=="LCB_TCD"
replace fpu=136 if zona=="LIM_BWA"
replace fpu=137 if zona=="LIM_MOZ"
replace fpu=138 if zona=="LIM_ZAF"
replace fpu=139 if zona=="LIM_ZWE"
replace fpu=140 if zona=="LMO_CHM"
replace fpu=141 if zona=="LMO_MNG"
replace fpu=142 if zona=="LUN_IND"
replace fpu=143 if zona=="MAD_MDG"
replace fpu=144 if zona=="MAT_IND"
replace fpu=145 if zona=="MAU_AUS"
replace fpu=146 if zona=="MCK_CAN"
replace fpu=147 if zona=="MEK_KHM"
replace fpu=148 if zona=="MEK_LAO"
replace fpu=149 if zona=="MEK_MMR"
replace fpu=150 if zona=="MEK_THA"
replace fpu=151 if zona=="MEK_VNM"
replace fpu=152 if zona=="MHN_IND"
replace fpu=153 if zona=="MIM_MEX"
replace fpu=154 if zona=="MIS_USA"
replace fpu=155 if zona=="MOU_USA"
replace fpu=156 if zona=="NAC_DZA"
replace fpu=157 if zona=="NAC_EGY"
replace fpu=158 if zona=="NAC_LBY"
replace fpu=159 if zona=="NAC_TUN"
replace fpu=160 if zona=="NEB_BRA"
replace fpu=161 if zona=="NER_RUS"
replace fpu=162 if zona=="NIC_NIC"
replace fpu=163 if zona=="NIG_BEN"
replace fpu=164 if zona=="NIG_BFA"
replace fpu=165 if zona=="NIG_CIV"
replace fpu=166 if zona=="NIG_CMR"
replace fpu=167 if zona=="NIG_GIN"
replace fpu=168 if zona=="NIG_MLI"
replace fpu=169 if zona=="NIG_NER"
replace fpu=170 if zona=="NIG_NGA"
replace fpu=171 if zona=="NIG_TCD"
replace fpu=172 if zona=="NKP_PRK"
replace fpu=173 if zona=="NLL_DJI"
replace fpu=174 if zona=="NLL_EGY"
replace fpu=175 if zona=="NLL_ERI"
replace fpu=176 if zona=="NLL_ETH"
replace fpu=177 if zona=="NLL_SDN"
replace fpu=178 if zona=="NLL_SSD"
replace fpu=179 if zona=="NLL_UGA"
replace fpu=180 if zona=="NOR_NOR"
replace fpu=181 if zona=="NWA_MOR"
replace fpu=182 if zona=="NWA_MRT"
replace fpu=183 if zona=="NWS_COL"
replace fpu=184 if zona=="NWS_ECU"
replace fpu=185 if zona=="NZE_NZL"
replace fpu=186 if zona=="OAO_OAO"
replace fpu=187 if zona=="OBB_CHM"
replace fpu=188 if zona=="OBB_KAZ"
replace fpu=189 if zona=="OBB_RUS"
replace fpu=190 if zona=="ODE_DEU"
replace fpu=191 if zona=="ODE_POL"
replace fpu=192 if zona=="ODE_RUS"
replace fpu=193 if zona=="OHI_USA"
replace fpu=194 if zona=="OIO_OIO"
replace fpu=195 if zona=="OPO_OPO"
replace fpu=196 if zona=="ORA_LSO"
replace fpu=197 if zona=="ORA_NAM"
replace fpu=198 if zona=="ORA_ZAF"
replace fpu=199 if zona=="ORI_COL"
replace fpu=200 if zona=="ORI_VEN"
replace fpu=201 if zona=="PAN_PAN"
replace fpu=202 if zona=="PAO_PNG"
replace fpu=203 if zona=="PAR_ARG"
replace fpu=204 if zona=="PAR_BOL"
replace fpu=205 if zona=="PAR_BRA"
replace fpu=206 if zona=="PAR_PRY"
replace fpu=207 if zona=="PEC_PER"
replace fpu=208 if zona=="PHI_PHL"
replace fpu=209 if zona=="PRT_PRT"
replace fpu=210 if zona=="RAP_RAP"
replace fpu=211 if zona=="RHI_BLX"
replace fpu=212 if zona=="RHI_CHP"
replace fpu=213 if zona=="RHI_DEU"
replace fpu=214 if zona=="RHI_FRP"
replace fpu=215 if zona=="RHI_NLD"
replace fpu=216 if zona=="RHO_FRP"
replace fpu=217 if zona=="RIC_ARG"
replace fpu=218 if zona=="RIG_MEX"
replace fpu=219 if zona=="RIG_USA"
replace fpu=220 if zona=="RRS_RUS"
replace fpu=221 if zona=="RVE_VEN"
replace fpu=222 if zona=="RVN_VNM"
replace fpu=223 if zona=="RWI_CAN"
replace fpu=224 if zona=="RWI_USA"
replace fpu=225 if zona=="SAC_SWZ"
replace fpu=226 if zona=="SAC_ZAF"
replace fpu=227 if zona=="SAF_MOZ"
replace fpu=228 if zona=="SAF_TZA"
replace fpu=229 if zona=="SAF_ZWE"
replace fpu=230 if zona=="SAH_DZA"
replace fpu=231 if zona=="SAH_EGY"
replace fpu=232 if zona=="SAH_LBY"
replace fpu=233 if zona=="SAH_MLI"
replace fpu=234 if zona=="SAH_MOR"
replace fpu=235 if zona=="SAH_MRT"
replace fpu=236 if zona=="SAH_NER"
replace fpu=237 if zona=="SAH_SDN"
replace fpu=238 if zona=="SAH_TCD"
replace fpu=239 if zona=="SAL_ARG"
replace fpu=240 if zona=="SAN_BRA"
replace fpu=241 if zona=="SAU_SAU"
replace fpu=242 if zona=="SAY_IND"
replace fpu=243 if zona=="SEI_FRP"
replace fpu=244 if zona=="SEN_GIN"
replace fpu=245 if zona=="SEN_MLI"
replace fpu=246 if zona=="SEN_MRT"
replace fpu=247 if zona=="SEN_SEN"
replace fpu=248 if zona=="SEU_USA"
replace fpu=249 if zona=="SKP_KOR"
replace fpu=250 if zona=="SLB_SLB"
replace fpu=251 if zona=="SLV_SLV"
replace fpu=252 if zona=="SON_CHM"
replace fpu=253 if zona=="SPP_SPP"
replace fpu=254 if zona=="SRL_LKA"
replace fpu=255 if zona=="SWE_SWE"
replace fpu=256 if zona=="SYD_KAZ"
replace fpu=257 if zona=="SYD_KGZ"
replace fpu=258 if zona=="SYD_UZB"
replace fpu=259 if zona=="TIE_ARG"
replace fpu=260 if zona=="TIG_IRN"
replace fpu=261 if zona=="TIG_IRQ"
replace fpu=262 if zona=="TIG_SYR"
replace fpu=263 if zona=="TIG_TUR"
replace fpu=264 if zona=="TLS_TLS"
replace fpu=265 if zona=="TMM_BGD"
replace fpu=266 if zona=="TMM_MMR"
replace fpu=267 if zona=="TMM_MYS"
replace fpu=268 if zona=="TMM_OSA"
replace fpu=269 if zona=="TMM_THA"
replace fpu=270 if zona=="TOC_BRA"
replace fpu=271 if zona=="TWN_CHM"
replace fpu=272 if zona=="UKP_UKP"
replace fpu=273 if zona=="UME_MEX"
replace fpu=274 if zona=="UMO_MNG"
replace fpu=275 if zona=="UMO_RUS"
replace fpu=276 if zona=="URA_KAZ"
replace fpu=277 if zona=="URA_RUS"
replace fpu=278 if zona=="URA_TKM"
replace fpu=279 if zona=="URU_BRA"
replace fpu=280 if zona=="URU_URY"
replace fpu=281 if zona=="USN_USA"
replace fpu=282 if zona=="VOG_KAZ"
replace fpu=283 if zona=="VOG_RUS"
replace fpu=284 if zona=="VOT_BEN"
replace fpu=285 if zona=="VOT_BFA"
replace fpu=286 if zona=="VOT_CIV"
replace fpu=287 if zona=="VOT_GHA"
replace fpu=288 if zona=="VOT_MLI"
replace fpu=289 if zona=="VOT_TGO"
replace fpu=290 if zona=="VUT_VUT"
replace fpu=291 if zona=="WAC_CIV"
replace fpu=292 if zona=="WAC_GIN"
replace fpu=293 if zona=="WAC_GMB"
replace fpu=294 if zona=="WAC_GNB"
replace fpu=295 if zona=="WAC_LBR"
replace fpu=296 if zona=="WAC_SEN"
replace fpu=297 if zona=="WAC_SLE"
replace fpu=298 if zona=="WAI_AFG"
replace fpu=299 if zona=="WAI_IRN"
replace fpu=300 if zona=="WAI_PAK"
replace fpu=301 if zona=="WAI_TKM"
replace fpu=302 if zona=="WAU_AUS"
replace fpu=303 if zona=="WGM_USA"
replace fpu=304 if zona=="YEM_YEM"
replace fpu=305 if zona=="YEN_RUS"
replace fpu=306 if zona=="YHE_CHM"
replace fpu=307 if zona=="YHE_KAZ"
replace fpu=308 if zona=="YRD_CHM"
replace fpu=309 if zona=="YRD_VNM"
replace fpu=310 if zona=="YUC_MEX"
replace fpu=311 if zona=="ZAM_AGO"
replace fpu=312 if zona=="ZAM_BWA"
replace fpu=313 if zona=="ZAM_COD"
replace fpu=314 if zona=="ZAM_MOZ"
replace fpu=315 if zona=="ZAM_MWI"
replace fpu=316 if zona=="ZAM_NAM"
replace fpu=317 if zona=="ZAM_TZA"
replace fpu=318 if zona=="ZAM_ZMB"
replace fpu=319 if zona=="ZAM_ZWE"
replace fpu=320 if zona=="ZHJ_CHM"
replace fpu=321 if zona=="SYD_TJK"
replace fpu=322 if zona=="LTZ_KAZ" 
replace fpu=323 if zona=="CHU_KAZ" 
replace fpu=324 if zona=="CHU_KGZ"

#delimit; 
label define fpu 1"ALB_ALB" 2"ALK_USA" 3"AMA_BOL" 4"AMA_BRA" 5"AMA_COL" 6"AMA_ECU" 7"AMA_PER" 8"AMD_AFG" 9"AMD_KAZ" 
10"AMD_TJK"  11"AMD_TKM" 12"AMD_UZB" 13"AMR_CHM" 14"AMR_RUS" 15"ARA_IRQ" 16"ARK_USA" 17"ARM_ARM" 18"AZE_AZE" 19"BAL_BLT" 
20"BAL_RUS"  21"BLA_RUS" 22"BLA_TUR" 23"BLA_UKR" 24"BLZ_BLZ" 25"BOR_IDN" 26"BOR_MYS" 27"BRT_BGD" 28"BRT_BTN" 29"BRT_CHM" 
30"BRT_IND" 31"CAF_AGO" 32"CAF_CAF" 33"CAF_CMR" 34"CAF_COG" 35"CAF_GAB" 36"CAF_GNQ"  37"CAF_NAM" 38"CAL_USA" 39"CAN_CAN" 
40"CAU_AUS" 41"CAV_IND" 42"CHC_CHL" 43"CHJ_CHM" 44"CHO_IND" 45"COB_CAN" 46"COB_USA" 47"COL_USA" 48"CON_AGO" 49"CON_CAF" 
50"CON_COD" 51"CON_COG" 52"CRB_CRB" 53"CRI_CRI" 54"CUB_CUB" 55"DAN_AUT" 56"DAN_BGR" 57"DAN_CZE" 58"DAN_DEU" 59"DAN_HRV" 
60"DAN_HUN" 61"DAN_MDA" 62"DAN_OBN" 63"DAN_ROU" 64"DAN_SVK" 65"DAN_SVN" 66"DAN_TUR" 67"DAN_UKR" 68"DNI_BLR" 69"DNI_RUS" 
70"DNI_UKR" 71"DOM_DOM" 72"EAC_BDI" 73"EAC_COD" 74"EAC_RWA" 75"EAC_TZA" 76"EAU_AUS" 77"EGH_IND" 78"ELB_DEU" 79"ELB_DNK" 
80"EME_CYP" 81"EME_EGY" 82"EME_ISR" 83"EME_JOR" 84"EME_LBN" 85"EME_PSE" 86"EME_SYR" 87"EME_TUR" 88"FJI_FJI" 89"FNP_FNP" 
90"GAN_BGD" 91"GAN_CHM" 92"GAN_IND" 93"GAN_NPL" 94"GBA_USA" 95"GEO_GEO" 96"GLA_CAN" 97"GLA_USA" 98"GOD_IND" 99"GRC_GRC" 
100"GRL_GRL" 101"GSA_GSA" 102"GTM_GTM" 103"HAI_CHM" 104"HND_HND" 105"HOA_ETH" 106"HOA_KEN" 107"HOA_SOM" 108"HTI_HTI" 
109"HUA_CHM" 110"HUN_CHM" 111"HWI_USA" 112"IEC_IND" 113"IND_CHM" 114"IND_IND" 115"IND_PAK" 116"INE_IDN" 117"INW_IDN" 
118"IRL_IRL" 119"ISL_ISL" 120"ITA_ITP" 121"JAM_JAM" 122"JAP_JPN" 123"KAL_BWA" 124"KAL_NAM" 125"KAL_ZAF" 126"KRI_IND" 
127"LAJ_CHM" 128"LBA_KAZ" 129"LBA_KGZ" 130"LBO_FRP" 131"LCB_CAF" 132"LCB_CMR" 133"LCB_NER" 134"LCB_NGA" 135"LCB_TCD" 136"LIM_BWA" 
137"LIM_MOZ" 138"LIM_ZAF" 139"LIM_ZWE" 140"LMO_CHM" 141"LMO_MNG" 142"LUN_IND" 143"MAD_MDG" 144"MAT_IND" 145"MAU_AUS" 146"MCK_CAN"
147"MEK_KHM" 148"MEK_LAO" 149"MEK_MMR" 150"MEK_THA" 151"MEK_VNM" 152"MHN_IND" 153"MIM_MEX" 154"MIS_USA" 155"MOU_USA" 156"NAC_DZA" 
157"NAC_EGY"  158"NAC_LBY" 159"NAC_TUN" 160"NEB_BRA" 161"NER_RUS" 162"NIC_NIC" 163"NIG_BEN" 164"NIG_BFA" 165"NIG_CIV" 166"NIG_CMR" 
167"NIG_GIN" 168"NIG_MLI" 169"NIG_NER" 170"NIG_NGA" 171"NIG_TCD" 172"NKP_PRK" 173"NLL_DJI" 174"NLL_EGY" 175"NLL_ERI" 176"NLL_ETH" 
177"NLL_SDN" 178"NLL_SSD" 179"NLL_UGA" 180"NOR_NOR" 181"NWA_MOR" 182"NWA_MRT" 183"NWS_COL" 184"NWS_ECU" 185"NZE_NZL" 186"OAO_OAO" 
187"OBB_CHM" 188"OBB_KAZ" 189"OBB_RUS" 190"ODE_DEU" 191"ODE_POL" 192"ODE_RUS" 193"OHI_USA" 194"OIO_OIO" 195"OPO_OPO" 196"ORA_LSO" 
197"ORA_NAM" 198"ORA_ZAF" 199"ORI_COL" 200"ORI_VEN" 201"PAN_PAN" 202"PAO_PNG" 203"PAR_ARG" 204"PAR_BOL" 205"PAR_BRA" 206"PAR_PRY" 
207"PEC_PER" 208"PHI_PHL" 209"PRT_PRT" 210"RAP_RAP" 211"RHI_BLX" 212"RHI_CHP" 213"RHI_DEU" 214"RHI_FRP" 215"RHI_NLD" 216"RHO_FRP" 
217"RIC_ARG" 218"RIG_MEX" 219"RIG_USA" 220"RRS_RUS" 221"RVE_VEN" 222"RVN_VNM" 223"RWI_CAN" 224"RWI_USA" 225"SAC_SWZ" 226"SAC_ZAF" 
227"SAF_MOZ" 228"SAF_TZA" 229"SAF_ZWE" 230"SAH_DZA" 231"SAH_EGY" 232"SAH_LBY" 233"SAH_MLI" 234"SAH_MOR" 235"SAH_MRT" 236"SAH_NER" 
237"SAH_SDN" 238"SAH_TCD" 239"SAL_ARG" 240"SAN_BRA" 241"SAU_SAU" 242"SAY_IND" 243"SEI_FRP" 244"SEN_GIN" 245"SEN_MLI" 246"SEN_MRT" 
247"SEN_SEN" 248"SEU_USA" 249"SKP_KOR" 250"SLB_SLB" 251"SLV_SLV" 252"SON_CHM" 253"SPP_SPP" 254"SRL_LKA" 255"SWE_SWE" 256"SYD_KAZ" 
257"SYD_KGZ" 258"SYD_UZB" 259"TIE_ARG" 260"TIG_IRN" 261"TIG_IRQ" 262"TIG_SYR" 263"TIG_TUR" 264"TLS_TLS" 265"TMM_BGD" 266"TMM_MMR" 
267"TMM_MYS" 268"TMM_OSA" 269"TMM_THA" 270"TOC_BRA" 271"TWN_CHM" 272"UKP_UKP" 273"UME_MEX" 274"UMO_MNG" 275"UMO_RUS" 276"URA_KAZ" 
277"URA_RUS" 278"URA_TKM" 279"URU_BRA" 280"URU_URY" 281"USN_USA" 282"VOG_KAZ" 283"VOG_RUS" 284"VOT_BEN" 285"VOT_BFA" 286"VOT_CIV" 
287"VOT_GHA" 288"VOT_MLI" 289"VOT_TGO" 290"VUT_VUT" 291"WAC_CIV" 292"WAC_GIN" 293"WAC_GMB" 294"WAC_GNB" 295"WAC_LBR" 296"WAC_SEN" 
297"WAC_SLE" 298"WAI_AFG" 299"WAI_IRN" 300"WAI_PAK" 301"WAI_TKM" 302"WAU_AUS" 303"WGM_USA" 304"YEM_YEM" 305"YEN_RUS" 306"YHE_CHM" 
307"YHE_KAZ" 308"YRD_CHM" 309"YRD_VNM" 310"YUC_MEX" 311"ZAM_AGO" 312"ZAM_BWA" 313"ZAM_COD" 314"ZAM_MOZ" 315"ZAM_MWI" 316"ZAM_NAM" 
317"ZAM_TZA" 318"ZAM_ZMB" 319"ZAM_ZWE" 320"ZHJ_CHM" 321"SYD_TJK" 322"LTZ_KAZ" 323"CHU_KAZ" 324"CHU_KGZ";
#delimit cr 
label value fpu fpu 
tab fpu
save `file', replace 
clear
}
end




*** para visualizar los archivos
local files: dir "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS" files "*.dta" 
display `files'
foreach file in `files' {
use `file', clear
 edit
 }
end 


cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS" 
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\
foreach name_n in dssat__379__bean__drought__base_2000__boring___29feb16_2120  ///
dssat__379__bean__drought__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2138 ///
dssat__379__bean__drought__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2157 ///
dssat__379__bean__drought__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2215 ///
dssat__379__bean__drought__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2234 ///
dssat__379__bean__drought__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2253 ///
dssat__379__bean__normal__base_2000__boring___29feb16_2129 ///
dssat__379__bean__normal__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2148 ///
dssat__379__bean__normal__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2206 ///
dssat__379__bean__normal__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2225 ///
dssat__379__bean__normal__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2244 ///
dssat__379__bean__normal__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2304 {
use "`path'\`name_n'.dta",  clear
	foreach x in yield {
	gen sis=.
	replace sis=1 if sys=="IR"
	replace sis=2 if sys=="RF"
	replace sis=3 if sys=="IR_on_cross"
	replace sis=4 if sys=="RF_on_cross"
	label define sis 1"IR" 2"RF" 3"IR_on_cross" 4"RF_on_cross"
	label value sis sis 
	drop sys
	tab sis
	}
save `file', replace
clear 
} 
end



********************************************************************************************
********************************************************************************************
********************************************************************************************
********************************************************************************************
********************************************************************************************
local files: dir "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS" files "*.dta" 
foreach file in `files'{
use `file', clear
gen gcm= substr("`file'",13,.) 
replace gcm= subinstr(gcm,".dta","",.) 
save `file', replace
clear
}
end


*********************************************************************************************
****NORMAL BASE
**** organizando la variable que identifique los gcm (acortandola) bean normal BASE
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal\
foreach name_n in dssat__379__bean__normal__base_2000__boring___29feb16_2129 {
use "`path'\`name_n'.dta",  clear
gen gcm_n= substr(gcm,1,.) 
replace gcm_n= subinstr(gcm_n, "__boring___29feb16_2129", " ", .) 
save "`path'\`name_n'.dta", replace 
clear
}
end



**** NORMAL GCM
**** organizando la variable que identifique los gcm (acortandola) bean normal/GCMs
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal\
foreach name_n in dssat__379__bean__normal__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2148 ///
dssat__379__bean__normal__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2206 ///
dssat__379__bean__normal__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2225 ///
dssat__379__bean__normal__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2244 ///
dssat__379__bean__normal__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2304  {
use "`path'\`name_n'.dta",  clear
gen gcm_n= substr(gcm,1,.) 
replace gcm_n= subinstr(gcm_n,"__future_rcp8p5_2041_2070__boring___29feb16_", " ", .) 
save "`path'\`name_n'.dta", replace 
clear
}
end

**********************************************************************************
****DROUGHT BASE
**** organizando la variable que identifique los gcm (acortandola) bean Drought/base
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought\
foreach name_n in dssat__379__bean__drought__base_2000__boring___29feb16_2120 {
use "`path'\`name_n'.dta",  clear
gen gcm_n= substr(gcm,1,.) 
replace gcm_n= subinstr(gcm_n,"__boring___29feb16_2120", " ", .) 
save "`path'\`name_n'.dta", replace 
clear
}
end

****DROUGHT GCM
**** organizando la variable que identifique los gcm (acortandola) bean Drought/GCM
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought\
foreach name_n in dssat__379__bean__drought__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2138 ///
dssat__379__bean__drought__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2157 ///
dssat__379__bean__drought__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2215 ///
dssat__379__bean__drought__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2234 ///
dssat__379__bean__drought__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2253 {
use "`path'\`name_n'.dta",  clear
gen gcm_n= substr(gcm,1,.) 
replace gcm_n= subinstr(gcm_n,"__future_rcp8p5_2041_2070__boring___29feb16_21", " ", .) 
save "`path'\`name_n'.dta", replace 
clear
}
end

****************************************************************************************
****************************************************************************************
****************************************************************************************
****************************************************************************************
****************************************************************************************






****COROPLETAS
***NORMAL BASE 
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal\
foreach name_n in dssat__379__bean__normal__base_2000__boring___29feb16_2129 {
use "`path'\`name_n'.dta",  clear
local y bean__normal__base_2000
foreach x in yield {
	spmap `x' using coorfpuall if sis==2, id(fpu) fcolor(BuRd) ///
	ocolor(white..) osize(vthin ..) legtitle("{it:Yield Bean Year 2005}")  clnumber(6) ///
	title ("{stSans: Bean Rainfed Yield by FPU's}" "{stMono:`x'}"  "{stMono:  base}") ///
	note("`y'") , 
	graph export `x'_`name_n'.png, replace width(4000)  
	}
save `file', replace
clear
}
end 

*****NORMAL GCM
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal\
foreach name_n  in dssat__379__bean__normal__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2148 ///
dssat__379__bean__normal__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2206 ///
dssat__379__bean__normal__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2225 ///
dssat__379__bean__normal__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2244 ///
dssat__379__bean__normal__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2304 {
use "`path'\`name_n'.dta",  clear
foreach x in yield {
	spmap `x' using coorfpuall if sis==2, id(fpu) fcolor(BuRd) ///
	ocolor(white..) osize(vthin ..) legtitle("{it:Yield Bean }")  clnumber(6) ///
	title ("{stSans: Bean Rainfed Yield by FPU's}" "{stMono:`x' Year 2050}" ) ///
	note("`name_n'") , 
	graph export `x'_`name_n'.png, replace width(4000)  
	}
save `file', replace
clear
}
end


**********************************************************************************************
**********************************************************************************************
**********************************************************************************************
**********************************************************************************************
**********************************************************************************************
**** rename fpu id NORMAL 
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal\
foreach name_n in dssat__379__bean__normal__base_2000__boring___29feb16_2129 ///
dssat__379__bean__normal__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2148 ///
dssat__379__bean__normal__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2206 ///
dssat__379__bean__normal__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2225 ///
dssat__379__bean__normal__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2244 ///
dssat__379__bean__normal__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2304 {
use "`path'\`name_n'.dta",  clear
rename fpu id
save "`path'\`name_n'.dta", replace 
clear
}
end


**** rename fpu id drought 
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought\
foreach name_n in dssat__379__bean__drought__base_2000__boring___29feb16_2120 ///
dssat__379__bean__drought__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2138 ///
dssat__379__bean__drought__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2157 ///
dssat__379__bean__drought__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2215 ///
dssat__379__bean__drought__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2234 ///
dssat__379__bean__drought__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2253 {
use "`path'\`name_n'.dta",  clear
rename fpu id
save "`path'\`name_n'.dta", replace 
clear
}
end
***************************************************************************************

****** MERGE NORMAL
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal\
foreach name_n in dssat__379__bean__normal__base_2000__boring___29feb16_2129 ///
dssat__379__bean__normal__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2148 ///
dssat__379__bean__normal__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2206 ///
dssat__379__bean__normal__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2225 ///
dssat__379__bean__normal__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2244 ///
dssat__379__bean__normal__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2304 {
use "`path'\`name_n'.dta",  clear
merge m:m id using fpudata
save "`path'\`name_n'.dta", replace 
clear 
}
end



****** MERGE DROUGHT
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought\
foreach name_n in dssat__379__bean__drought__base_2000__boring___29feb16_2120 ///
dssat__379__bean__drought__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2138 ///
dssat__379__bean__drought__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2157 ///
dssat__379__bean__drought__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2215 ///
dssat__379__bean__drought__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2234 ///
dssat__379__bean__drought__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2253 {
use "`path'\`name_n'.dta",  clear
merge m:m id using fpudata
save "`path'\`name_n'.dta", replace 
clear 
}
end


*************************************************************************************************
**************************************************************************************************

******BEAN NORMAL
**** basin_name
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal\
foreach name_n in dssat__379__bean__normal__base_2000__boring___29feb16_2129 ///
dssat__379__bean__normal__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2148 ///
dssat__379__bean__normal__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2206 ///
dssat__379__bean__normal__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2225 ///
dssat__379__bean__normal__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2244 ///
dssat__379__bean__normal__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2304 {
use "`path'\`name_n'.dta",  clear
gen region=.
replace region=1 if Basin_Name=="Alaska"
replace region=2 if Basin_Name=="Albania"
replace region=3 if Basin_Name=="Amazon"
replace region=4 if Basin_Name=="Amudarja"
replace region=5 if Basin_Name=="Amur"
replace region=6 if Basin_Name=="Arabian_Peninsula"
replace region=7 if Basin_Name=="Arkansas"
replace region=8 if Basin_Name=="Armenia"
replace region=9 if Basin_Name=="Azerbiajan"
replace region=10 if Basin_Name=="Baltic"
replace region=11 if Basin_Name=="Belize"
replace region=12 if Basin_Name=="Black_Sea"
replace region=13 if Basin_Name=="Borneo"
replace region=14 if Basin_Name=="Brahmaputra"
replace region=15 if Basin_Name=="California"
replace region=16 if Basin_Name=="Canada_Arctic"
replace region=17 if Basin_Name=="Cauvery"
replace region=18 if Basin_Name=="Central_African"
replace region=19 if Basin_Name=="Central_Australia"
replace region=20 if Basin_Name=="Chang_Jiang"
replace region=21 if Basin_Name=="Chile_Coast"
replace region=22 if Basin_Name=="Chotanagpui"
replace region=23 if Basin_Name=="Colorado"
replace region=24 if Basin_Name=="Columbia"
replace region=25 if Basin_Name=="Congo"
replace region=26 if Basin_Name=="Costa Rica"
replace region=27 if Basin_Name=="Cuba"
replace region=28 if Basin_Name=="Danube"
replace region=29 if Basin_Name=="Dnieper"
replace region=30 if Basin_Name=="Dominican_Republic"
replace region=31 if Basin_Name=="East_African_Coa"
replace region=32 if Basin_Name=="Easten_Ghats"
replace region=33 if Basin_Name=="Eastern_Australi"
replace region=34 if Basin_Name=="Eastern_Mediterranean"
replace region=35 if Basin_Name=="El_Salvador"
replace region=36 if Basin_Name=="Elbe"
replace region=37 if Basin_Name=="Fiji"
replace region=38 if Basin_Name=="Finland_plus"
replace region=39 if Basin_Name=="Ganges"
replace region=40 if Basin_Name=="Georgia"
replace region=41 if Basin_Name=="Godavari"
replace region=42 if Basin_Name=="Great_Basin"
replace region=43 if Basin_Name=="Great_Britain_plus"
replace region=44 if Basin_Name=="Great_Lakes"
replace region=45 if Basin_Name=="Greece"
replace region=46 if Basin_Name=="Greenland"
replace region=47 if Basin_Name=="Guatemala"
replace region=48 if Basin_Name=="Guyanas_South_America"
replace region=49 if Basin_Name=="Hail_He"
replace region=50 if Basin_Name=="Haiti"
replace region=51 if Basin_Name=="Hawaii"
replace region=52 if Basin_Name=="Honduras"
replace region=53 if Basin_Name=="Horn_of_Africa"
replace region=54 if Basin_Name=="Hual_He"
replace region=55 if Basin_Name=="Huang_He"
replace region=56 if Basin_Name=="Iceland"
replace region=57 if Basin_Name=="India_East_Coast"
replace region=58 if Basin_Name=="Indonesia_East"
replace region=59 if Basin_Name=="Indonesia_West"
replace region=60 if Basin_Name=="Indus"
replace region=61 if Basin_Name=="Ireland"
replace region=62 if Basin_Name=="Italy"
replace region=63 if Basin_Name=="Jamaica"
replace region=64 if Basin_Name=="Japan"
replace region=65 if Basin_Name=="Kalahari"
replace region=66 if Basin_Name=="Krishna"
replace region=67 if Basin_Name=="Lake_Balkhash"
replace region=68 if Basin_Name=="Lake_Chad_Basin"
replace region=69 if Basin_Name=="Langcang_Jiang"
replace region=70 if Basin_Name=="Limpopo"
replace region=71 if Basin_Name=="Loire_Bordeaux"
replace region=72 if Basin_Name=="Lower_Mongolia"
replace region=73 if Basin_Name=="Luni"
replace region=74 if Basin_Name=="Mackenzie"
replace region=75 if Basin_Name=="Madagascar"
replace region=76 if Basin_Name=="Mahanadi"
replace region=77 if Basin_Name=="Mahi_Tapti"
replace region=78 if Basin_Name=="Mekong"
replace region=79 if Basin_Name=="Middle_Mexico"
replace region=80 if Basin_Name=="Mississippi"
replace region=81 if Basin_Name=="Missouri"
replace region=82 if Basin_Name=="Murray_Australia"
replace region=83 if Basin_Name=="New_Zealand"
replace region=84 if Basin_Name=="Nicaruagua"
replace region=85 if Basin_Name=="Niger"
replace region=86 if Basin_Name=="Nile"
replace region=87 if Basin_Name=="North_African_Coast"
replace region=88 if Basin_Name=="North_Euro_Russia"
replace region=89 if Basin_Name=="North_Korea_Peninsula"
replace region=90 if Basin_Name=="Northeast_Brazil"
replace region=91 if Basin_Name=="Northwest_Africa"
replace region=92 if Basin_Name=="Northwest_South_America"
replace region=93 if Basin_Name=="Norway"
replace region=94 if Basin_Name=="Ob"
replace region=95 if Basin_Name=="Oder"
replace region=96 if Basin_Name=="Ohio"
replace region=97 if Basin_Name=="Orange"
replace region=98 if Basin_Name=="Orinoco"
replace region=99 if Basin_Name=="Other Caribbean"
replace region=100 if Basin_Name=="Other Indian Ocean"
replace region=101 if Basin_Name=="Other Pacific Ocean"
replace region=102 if Basin_Name=="Other_Atlantic_Ocean"
replace region=103 if Basin_Name=="Panama"
replace region=104 if Basin_Name=="Papau_Oceania"
replace region=105 if Basin_Name=="Parana"
replace region=106 if Basin_Name=="Peru_Coastal"
replace region=107 if Basin_Name=="Philippines"
replace region=108 if Basin_Name=="Portugal"
replace region=109 if Basin_Name=="Red_Winnipeg"
replace region=110 if Basin_Name=="Rest of Russia"
replace region=111 if Basin_Name=="Rest of Venezuela"
replace region=112 if Basin_Name=="Rest of Vietnam"
replace region=113 if Basin_Name=="Rest_of_Arab_Peninsula"
replace region=114 if Basin_Name=="Rhine"
replace region=115 if Basin_Name=="Rhone"
replace region=116 if Basin_Name=="Rio Grande"
replace region=117 if Basin_Name=="Rio_Colorado"
replace region=118 if Basin_Name=="Sahara"
replace region=119 if Basin_Name=="Sahyada"
replace region=120 if Basin_Name=="Salada_Tierra"
replace region=121 if Basin_Name=="San_Francisco"
replace region=122 if Basin_Name=="Saudi Arabia"
replace region=123 if Basin_Name=="Seine"
replace region=124 if Basin_Name=="Senegal"
replace region=125 if Basin_Name=="Solomon_Islands"
replace region=126 if Basin_Name=="Songhua"
replace region=127 if Basin_Name=="South_African_Coast"
replace region=128 if Basin_Name=="South_Korea_Peninsula"
replace region=129 if Basin_Name=="Southeast_Africa"
replace region=130 if Basin_Name=="Southeast_US"
replace region=131 if Basin_Name=="Spain_plus"
replace region=132 if Basin_Name=="Sri_Lanka"
replace region=133 if Basin_Name=="Sweden"
replace region=134 if Basin_Name=="Syrdarja"
replace region=135 if Basin_Name=="Taiwan"
replace region=136 if Basin_Name=="Thai_Myan_Malay"
replace region=137 if Basin_Name=="Tierra"
replace region=138 if Basin_Name=="Tigris_Euphrates"
replace region=139 if Basin_Name=="Timor-L'este"
replace region=140 if Basin_Name=="Toc"
replace region=141 if Basin_Name=="Upper_Mexico"
replace region=142 if Basin_Name=="Upper_Mongolia"
replace region=143 if Basin_Name=="Ural"
replace region=144 if Basin_Name=="Uruguay"
replace region=145 if Basin_Name=="US_Northeast"
replace region=146 if Basin_Name=="Vanuatu"
replace region=147 if Basin_Name=="Volga"
replace region=148 if Basin_Name=="Volta"
replace region=149 if Basin_Name=="West_African_Coast"
replace region=150 if Basin_Name=="Western_Asia_Iran"
replace region=151 if Basin_Name=="Western_Australia"
replace region=152 if Basin_Name=="Western_Gulf_Mexico"
replace region=153 if Basin_Name=="Yemen"
replace region=154 if Basin_Name=="Yenisey"
replace region=155 if Basin_Name=="Yili_He"
replace region=156 if Basin_Name=="Yuan Jiang-Red River"
replace region=157 if Basin_Name=="Yucatan"
replace region=158 if Basin_Name=="Zambezi"
replace region=159 if Basin_Name=="Zhu_Jiang"
replace region=160 if Basin_Name=="United States"
label define region 1"Alaska" 2"Albania" 3"Amazon" 4"Amudarja" 5"Amur" 6"Arabian_Peninsula" 7"Arkansas" 8"Armenia"  ///
9"Azerbiajan" 10"Baltic" 11"Belize" 12"Black_Sea" 13"Borneo" 14"Brahmaputra" 15"California" 16"Canada_Arctic" 17"Cauvery" ///
18"Central_African" 19"Central_Australia" 20"Chang_Jiang" 21"Chile_Coast" 22"Chotanagpui" 23"Colorado" 24"Columbia" ///
25"Congo" 26"Costa Rica" 27"Cuba" 28"Danube" 29"Dnieper" 30"Dominican_Republic" 31"East_African_Coa" 32"Easten_Ghats" ///
33"Eastern_Australi" 34"Eastern_Mediterranean" 35"El_Salvador" 36"Elbe" 37"Fiji" 38"Finland_plus" 39"Ganges" 40"Georgia" ///
41"Godavari" 42"Great_Basin" 43"Great_Britain_plus" 44"Great_Lakes" 45"Greece" 46"Greenland" 47"Guatemala" 48"Guyanas_South_America" ///
49"Hail_He" 50"Haiti" 51"Hawaii" 52"Honduras" 53"Horn_of_Africa" 54"Hual_He" 55"Huang_He" 56"Iceland" 57"India_East_Coast" 58"Indonesia_East" ///
59"Indonesia_West" 60"Indus" 61"Ireland" 62"Italy" 63"Jamaica" 64"Japan" 65"Kalahari" 66"Krishna" 67"Lake_Balkhash" 68"Lake_Chad_Basin" 69"Langcang_Jiang" ///
70"Limpopo" 71"Loire_Bordeaux" 72"Lower_Mongolia" 73"Luni" 74"Mackenzie" 75"Madagascar" 76"Mahanadi" 77"Mahi_Tapti" 78"Mekong" 79"Middle_Mexico" ///
80"Mississippi" 81"Missouri" 82"Murray_Australia" 83"New_Zealand" 84"Nicaruagua" 85"Niger" 86"Nile" 87"North_African_Coast" 88"North_Euro_Russia" 89"North_Korea_Peninsula" ///
90"Northeast_Brazil" 91"Northwest_Africa" 92"Northwest_South_America" 93"Norway" 94"Ob" 95"Oder" 96"Ohio" 97"Orange" 98"Orinoco" 99"Other Caribbean" 100"Other Indian Ocean" ///
101"Other Pacific Ocean" 102"Other_Atlantic_Ocean" 103"Panama" 104"Papau_Oceania" 105"Parana" 106"Peru_Coastal" 107"Philippines" 108"Portugal" ///
109"Red_Winnipeg" 110"Rest of Russia" 111"Rest of Venezuela" 112"Rest of Vietnam" 113"Rest_of_Arab_Peninsula" 114"Rhine" 115"Rhone" 116"Rio Grande" 117"Rio_Colorado" ///
118"Sahara" 119"Sahyada" 120"Salada_Tierra" 121"San_Francisco" 122"Saudi Arabia" 123"Seine" 124"Senegal" 125"Solomon_Islands" 126"Songhua" 127"South_African_Coast" ///
128"South_Korea_Peninsula" 129"Southeast_Africa" 130"Southeast_US" 131"Spain_plus" 132"Sri_Lanka" 133"Sweden" 134"Syrdarja" 135"Taiwan" 136"Thai_Myan_Malay" 137"Tierra" ///
138"Tigris_Euphrates" 139"Timor-L'este" 140"Toc" 141"Upper_Mexico" 142"Upper_Mongolia" 143"Ural" 144"Uruguay" 145"US_Northeast" 146"Vanuatu" 147"Volga" 148"Volta" ///
149"West_African_Coast" 150"Western_Asia_Iran" 151"Western_Australia" 152"Western_Gulf_Mexico" 153"Yemen" 154"Yenisey" 155"Yili_He" 156"Yuan Jiang-Red River" 157"Yucatan" ///
158"Zambezi" 159"Zhu_Jiang" 160"United States"
label value  region region
save "`path'\`name_n'.dta", replace 
}
end 

*****borrar merge
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal\
foreach name_n in dssat__379__bean__normal__base_2000__boring___29feb16_2129 ///
dssat__379__bean__normal__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2148 ///
dssat__379__bean__normal__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2206 ///
dssat__379__bean__normal__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2225 ///
dssat__379__bean__normal__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2244 ///
dssat__379__bean__normal__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2304 {
use "`path'\`name_n'.dta",  clear
drop _merge
save "`path'\`name_n'.dta", replace 
clear 
}
end


*****Basin Name Drought
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought\
foreach name_n in dssat__379__bean__drought__base_2000__boring___29feb16_2120 ///
dssat__379__bean__drought__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2138 ///
dssat__379__bean__drought__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2157 ///
dssat__379__bean__drought__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2215 ///
dssat__379__bean__drought__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2234 ///
dssat__379__bean__drought__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2253 {
use "`path'\`name_n'.dta",  clear
gen region=.
replace region=1 if Basin_Name=="Alaska"
replace region=2 if Basin_Name=="Albania"
replace region=3 if Basin_Name=="Amazon"
replace region=4 if Basin_Name=="Amudarja"
replace region=5 if Basin_Name=="Amur"
replace region=6 if Basin_Name=="Arabian_Peninsula"
replace region=7 if Basin_Name=="Arkansas"
replace region=8 if Basin_Name=="Armenia"
replace region=9 if Basin_Name=="Azerbiajan"
replace region=10 if Basin_Name=="Baltic"
replace region=11 if Basin_Name=="Belize"
replace region=12 if Basin_Name=="Black_Sea"
replace region=13 if Basin_Name=="Borneo"
replace region=14 if Basin_Name=="Brahmaputra"
replace region=15 if Basin_Name=="California"
replace region=16 if Basin_Name=="Canada_Arctic"
replace region=17 if Basin_Name=="Cauvery"
replace region=18 if Basin_Name=="Central_African"
replace region=19 if Basin_Name=="Central_Australia"
replace region=20 if Basin_Name=="Chang_Jiang"
replace region=21 if Basin_Name=="Chile_Coast"
replace region=22 if Basin_Name=="Chotanagpui"
replace region=23 if Basin_Name=="Colorado"
replace region=24 if Basin_Name=="Columbia"
replace region=25 if Basin_Name=="Congo"
replace region=26 if Basin_Name=="Costa Rica"
replace region=27 if Basin_Name=="Cuba"
replace region=28 if Basin_Name=="Danube"
replace region=29 if Basin_Name=="Dnieper"
replace region=30 if Basin_Name=="Dominican_Republic"
replace region=31 if Basin_Name=="East_African_Coa"
replace region=32 if Basin_Name=="Easten_Ghats"
replace region=33 if Basin_Name=="Eastern_Australi"
replace region=34 if Basin_Name=="Eastern_Mediterranean"
replace region=35 if Basin_Name=="El_Salvador"
replace region=36 if Basin_Name=="Elbe"
replace region=37 if Basin_Name=="Fiji"
replace region=38 if Basin_Name=="Finland_plus"
replace region=39 if Basin_Name=="Ganges"
replace region=40 if Basin_Name=="Georgia"
replace region=41 if Basin_Name=="Godavari"
replace region=42 if Basin_Name=="Great_Basin"
replace region=43 if Basin_Name=="Great_Britain_plus"
replace region=44 if Basin_Name=="Great_Lakes"
replace region=45 if Basin_Name=="Greece"
replace region=46 if Basin_Name=="Greenland"
replace region=47 if Basin_Name=="Guatemala"
replace region=48 if Basin_Name=="Guyanas_South_America"
replace region=49 if Basin_Name=="Hail_He"
replace region=50 if Basin_Name=="Haiti"
replace region=51 if Basin_Name=="Hawaii"
replace region=52 if Basin_Name=="Honduras"
replace region=53 if Basin_Name=="Horn_of_Africa"
replace region=54 if Basin_Name=="Hual_He"
replace region=55 if Basin_Name=="Huang_He"
replace region=56 if Basin_Name=="Iceland"
replace region=57 if Basin_Name=="India_East_Coast"
replace region=58 if Basin_Name=="Indonesia_East"
replace region=59 if Basin_Name=="Indonesia_West"
replace region=60 if Basin_Name=="Indus"
replace region=61 if Basin_Name=="Ireland"
replace region=62 if Basin_Name=="Italy"
replace region=63 if Basin_Name=="Jamaica"
replace region=64 if Basin_Name=="Japan"
replace region=65 if Basin_Name=="Kalahari"
replace region=66 if Basin_Name=="Krishna"
replace region=67 if Basin_Name=="Lake_Balkhash"
replace region=68 if Basin_Name=="Lake_Chad_Basin"
replace region=69 if Basin_Name=="Langcang_Jiang"
replace region=70 if Basin_Name=="Limpopo"
replace region=71 if Basin_Name=="Loire_Bordeaux"
replace region=72 if Basin_Name=="Lower_Mongolia"
replace region=73 if Basin_Name=="Luni"
replace region=74 if Basin_Name=="Mackenzie"
replace region=75 if Basin_Name=="Madagascar"
replace region=76 if Basin_Name=="Mahanadi"
replace region=77 if Basin_Name=="Mahi_Tapti"
replace region=78 if Basin_Name=="Mekong"
replace region=79 if Basin_Name=="Middle_Mexico"
replace region=80 if Basin_Name=="Mississippi"
replace region=81 if Basin_Name=="Missouri"
replace region=82 if Basin_Name=="Murray_Australia"
replace region=83 if Basin_Name=="New_Zealand"
replace region=84 if Basin_Name=="Nicaruagua"
replace region=85 if Basin_Name=="Niger"
replace region=86 if Basin_Name=="Nile"
replace region=87 if Basin_Name=="North_African_Coast"
replace region=88 if Basin_Name=="North_Euro_Russia"
replace region=89 if Basin_Name=="North_Korea_Peninsula"
replace region=90 if Basin_Name=="Northeast_Brazil"
replace region=91 if Basin_Name=="Northwest_Africa"
replace region=92 if Basin_Name=="Northwest_South_America"
replace region=93 if Basin_Name=="Norway"
replace region=94 if Basin_Name=="Ob"
replace region=95 if Basin_Name=="Oder"
replace region=96 if Basin_Name=="Ohio"
replace region=97 if Basin_Name=="Orange"
replace region=98 if Basin_Name=="Orinoco"
replace region=99 if Basin_Name=="Other Caribbean"
replace region=100 if Basin_Name=="Other Indian Ocean"
replace region=101 if Basin_Name=="Other Pacific Ocean"
replace region=102 if Basin_Name=="Other_Atlantic_Ocean"
replace region=103 if Basin_Name=="Panama"
replace region=104 if Basin_Name=="Papau_Oceania"
replace region=105 if Basin_Name=="Parana"
replace region=106 if Basin_Name=="Peru_Coastal"
replace region=107 if Basin_Name=="Philippines"
replace region=108 if Basin_Name=="Portugal"
replace region=109 if Basin_Name=="Red_Winnipeg"
replace region=110 if Basin_Name=="Rest of Russia"
replace region=111 if Basin_Name=="Rest of Venezuela"
replace region=112 if Basin_Name=="Rest of Vietnam"
replace region=113 if Basin_Name=="Rest_of_Arab_Peninsula"
replace region=114 if Basin_Name=="Rhine"
replace region=115 if Basin_Name=="Rhone"
replace region=116 if Basin_Name=="Rio Grande"
replace region=117 if Basin_Name=="Rio_Colorado"
replace region=118 if Basin_Name=="Sahara"
replace region=119 if Basin_Name=="Sahyada"
replace region=120 if Basin_Name=="Salada_Tierra"
replace region=121 if Basin_Name=="San_Francisco"
replace region=122 if Basin_Name=="Saudi Arabia"
replace region=123 if Basin_Name=="Seine"
replace region=124 if Basin_Name=="Senegal"
replace region=125 if Basin_Name=="Solomon_Islands"
replace region=126 if Basin_Name=="Songhua"
replace region=127 if Basin_Name=="South_African_Coast"
replace region=128 if Basin_Name=="South_Korea_Peninsula"
replace region=129 if Basin_Name=="Southeast_Africa"
replace region=130 if Basin_Name=="Southeast_US"
replace region=131 if Basin_Name=="Spain_plus"
replace region=132 if Basin_Name=="Sri_Lanka"
replace region=133 if Basin_Name=="Sweden"
replace region=134 if Basin_Name=="Syrdarja"
replace region=135 if Basin_Name=="Taiwan"
replace region=136 if Basin_Name=="Thai_Myan_Malay"
replace region=137 if Basin_Name=="Tierra"
replace region=138 if Basin_Name=="Tigris_Euphrates"
replace region=139 if Basin_Name=="Timor-L'este"
replace region=140 if Basin_Name=="Toc"
replace region=141 if Basin_Name=="Upper_Mexico"
replace region=142 if Basin_Name=="Upper_Mongolia"
replace region=143 if Basin_Name=="Ural"
replace region=144 if Basin_Name=="Uruguay"
replace region=145 if Basin_Name=="US_Northeast"
replace region=146 if Basin_Name=="Vanuatu"
replace region=147 if Basin_Name=="Volga"
replace region=148 if Basin_Name=="Volta"
replace region=149 if Basin_Name=="West_African_Coast"
replace region=150 if Basin_Name=="Western_Asia_Iran"
replace region=151 if Basin_Name=="Western_Australia"
replace region=152 if Basin_Name=="Western_Gulf_Mexico"
replace region=153 if Basin_Name=="Yemen"
replace region=154 if Basin_Name=="Yenisey"
replace region=155 if Basin_Name=="Yili_He"
replace region=156 if Basin_Name=="Yuan Jiang-Red River"
replace region=157 if Basin_Name=="Yucatan"
replace region=158 if Basin_Name=="Zambezi"
replace region=159 if Basin_Name=="Zhu_Jiang"
replace region=160 if Basin_Name=="United States"
label define region 1"Alaska" 2"Albania" 3"Amazon" 4"Amudarja" 5"Amur" 6"Arabian_Peninsula" 7"Arkansas" 8"Armenia"  ///
9"Azerbiajan" 10"Baltic" 11"Belize" 12"Black_Sea" 13"Borneo" 14"Brahmaputra" 15"California" 16"Canada_Arctic" 17"Cauvery" ///
18"Central_African" 19"Central_Australia" 20"Chang_Jiang" 21"Chile_Coast" 22"Chotanagpui" 23"Colorado" 24"Columbia" ///
25"Congo" 26"Costa Rica" 27"Cuba" 28"Danube" 29"Dnieper" 30"Dominican_Republic" 31"East_African_Coa" 32"Easten_Ghats" ///
33"Eastern_Australi" 34"Eastern_Mediterranean" 35"El_Salvador" 36"Elbe" 37"Fiji" 38"Finland_plus" 39"Ganges" 40"Georgia" ///
41"Godavari" 42"Great_Basin" 43"Great_Britain_plus" 44"Great_Lakes" 45"Greece" 46"Greenland" 47"Guatemala" 48"Guyanas_South_America" ///
49"Hail_He" 50"Haiti" 51"Hawaii" 52"Honduras" 53"Horn_of_Africa" 54"Hual_He" 55"Huang_He" 56"Iceland" 57"India_East_Coast" 58"Indonesia_East" ///
59"Indonesia_West" 60"Indus" 61"Ireland" 62"Italy" 63"Jamaica" 64"Japan" 65"Kalahari" 66"Krishna" 67"Lake_Balkhash" 68"Lake_Chad_Basin" 69"Langcang_Jiang" ///
70"Limpopo" 71"Loire_Bordeaux" 72"Lower_Mongolia" 73"Luni" 74"Mackenzie" 75"Madagascar" 76"Mahanadi" 77"Mahi_Tapti" 78"Mekong" 79"Middle_Mexico" ///
80"Mississippi" 81"Missouri" 82"Murray_Australia" 83"New_Zealand" 84"Nicaruagua" 85"Niger" 86"Nile" 87"North_African_Coast" 88"North_Euro_Russia" 89"North_Korea_Peninsula" ///
90"Northeast_Brazil" 91"Northwest_Africa" 92"Northwest_South_America" 93"Norway" 94"Ob" 95"Oder" 96"Ohio" 97"Orange" 98"Orinoco" 99"Other Caribbean" 100"Other Indian Ocean" ///
101"Other Pacific Ocean" 102"Other_Atlantic_Ocean" 103"Panama" 104"Papau_Oceania" 105"Parana" 106"Peru_Coastal" 107"Philippines" 108"Portugal" ///
109"Red_Winnipeg" 110"Rest of Russia" 111"Rest of Venezuela" 112"Rest of Vietnam" 113"Rest_of_Arab_Peninsula" 114"Rhine" 115"Rhone" 116"Rio Grande" 117"Rio_Colorado" ///
118"Sahara" 119"Sahyada" 120"Salada_Tierra" 121"San_Francisco" 122"Saudi Arabia" 123"Seine" 124"Senegal" 125"Solomon_Islands" 126"Songhua" 127"South_African_Coast" ///
128"South_Korea_Peninsula" 129"Southeast_Africa" 130"Southeast_US" 131"Spain_plus" 132"Sri_Lanka" 133"Sweden" 134"Syrdarja" 135"Taiwan" 136"Thai_Myan_Malay" 137"Tierra" ///
138"Tigris_Euphrates" 139"Timor-L'este" 140"Toc" 141"Upper_Mexico" 142"Upper_Mongolia" 143"Ural" 144"Uruguay" 145"US_Northeast" 146"Vanuatu" 147"Volga" 148"Volta" ///
149"West_African_Coast" 150"Western_Asia_Iran" 151"Western_Australia" 152"Western_Gulf_Mexico" 153"Yemen" 154"Yenisey" 155"Yili_He" 156"Yuan Jiang-Red River" 157"Yucatan" ///
158"Zambezi" 159"Zhu_Jiang" 160"United States"
label value  region region
save "`path'\`name_n'.dta", replace 
}
end 


****Borrar merge
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought\
foreach name_n in dssat__379__bean__drought__base_2000__boring___29feb16_2120 ///
dssat__379__bean__drought__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2138 ///
dssat__379__bean__drought__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2157 ///
dssat__379__bean__drought__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2215 ///
dssat__379__bean__drought__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2234 ///
dssat__379__bean__drought__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2253 {
use "`path'\`name_n'.dta",  clear
drop _merge
save "`path'\`name_n'.dta", replace 
clear 
}
end
**************************************************************************************************
*** UNA SOLA BASE DE DATOS NORMAL 
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal"
use dssat__379__bean__normal__base_2000__boring___29feb16_2129.dta, clear 
append using dssat__379__bean__normal__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2148.dta ///
dssat__379__bean__normal__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2206.dta ///
dssat__379__bean__normal__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2225.dta ///
dssat__379__bean__normal__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2244.dta ///
dssat__379__bean__normal__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2304.dta, nolabel 
save "beanNormal.dta", replace
clear





/*
*** no funciona bien  los nimbres sin muchos
use beanNormal, clear
#delimit;
graph hbox yield if sis==2 , over(id, sort(1) label(labsize(vsmall)))  
	title("{it:Rice Yield Rainfed}" "{stMono:BASE}")
	box(1, bfcolor(ltbluishgray) blcolor(black)) 
	legend(label(1 "normal") )  
	marker(1, mcolor(red) msymbol(Sh)msize(vsmall))
	ytitle("Yield")
	ylabel(0(1000)10000, angle(45) labsize(small))
	boxgap(30) medline(lcolor(blue) lwidth(medium)) scheme(s1mono)
	box(1,color(midblue))
	xsize(6.5) ysize(7);
#delimit cr 
graph save box_`name'.gph, replace
graph export box_`name'.png, width(4000) replace 
clear
}
end


*/
*** UNA SOLA BASE DE DATOS DROUGHT 
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought"
use dssat__379__bean__drought__base_2000__boring___29feb16_2120.dta
append using dssat__379__bean__drought__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2138.dta ///
dssat__379__bean__drought__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2157.dta ///
dssat__379__bean__drought__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2215.dta ///
dssat__379__bean__drought__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2234.dta ///
dssat__379__bean__drought__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2253.dta, nolabel 
save "beanDrought.dta", replace
clear

**************************************************************************************
**************************************************************************************
**************************************************************************************

****** Estadisticas para DROUGHT 
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought\
foreach name_n in dssat__379__bean__drought__base_2000__boring___29feb16_2120 ///
dssat__379__bean__drought__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2138 ///
dssat__379__bean__drought__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2157 ///
dssat__379__bean__drought__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2215 ///
dssat__379__bean__drought__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2234 ///
dssat__379__bean__drought__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2253 {
use "`path'\`name_n'.dta",  clear
foreach y in yield {
sum `y' if sis==4 & inlist(region, 3, 11, 21, 26 ,117, 105, 137,140, 120, 27, 30, 35, 47, 48, 50, 52, 63, 79, 84, 90, 92, 121, 98 , 99 , 103 , 106 , 111 , 116 , 141 , 144, 152,157),
foreach  sheet in varlist {
#delimit;
putexcel A1=("`name_n'") 
		 A2=("variable") B2=("data") 
		 A3=("Obs")	B3=(r(N))
		 A4=("Mean") B4=(r(mean)) 
		 A5=("Std. Dev.") B5=(r(sd))
		 A6=("Min") B6=(r(min))
	     A7=("Max") B7=(r(max)) using RF_on_cross`name_n'.xls, sheet("`sheet'") replace;
#delimit cr

clear
}
}
}
end


**** Estadisticas para Normal 
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal\
foreach name_n in dssat__379__bean__normal__base_2000__boring___29feb16_2129 ///
dssat__379__bean__normal__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2148 ///
dssat__379__bean__normal__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2206 ///
dssat__379__bean__normal__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2225 ///
dssat__379__bean__normal__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2244 ///
dssat__379__bean__normal__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2304 {
use "`path'\`name_n'.dta",  clear
foreach y in yield {
sum `y' if sis==4 & inlist(region, 3, 11, 21, 26 ,117, 105, 137,140, 120, 27, 30, 35, 47, 48, 50, 52, 63, 79, 84, 90, 92, 121, 98 , 99 , 103 , 106 , 111 , 116 , 141 , 144, 152,157),
foreach  sheet in varlist {
#delimit;
putexcel A1=("`name_n'") 
		 A2=("variable") B2=("data") 
		 A3=("Obs")	B3=(r(N))
		 A4=("Mean") B4=(r(mean)) 
		 A5=("Std. Dev.") B5=(r(sd))
		 A6=("Min") B6=(r(min))
	     A7=("Max") B7=(r(max)) using RF_on_cross`name_n'.xls, sheet("`sheet'") replace;
#delimit cr

clear
}
}
}
end

*************************************************************************************************



dssat__379__bean__drought__base_2000__boring___29feb16_2120

**** Graficos por zonas Frijol Normal
**** mapa para ALC 
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal\
foreach name_n in dssat__379__bean__normal__base_2000__boring___29feb16_2129 ///
dssat__379__bean__normal__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2148 ///
dssat__379__bean__normal__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2206 ///
dssat__379__bean__normal__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2225 ///
dssat__379__bean__normal__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2244 ///
dssat__379__bean__normal__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2304 {
use "`path'\`name_n'.dta",  clear
foreach x in yield {
	spmap `x' using coorfpuall if sis==2 & inlist(region, 3, 11, 21, 26 ,117, 105, 137,140, 120, 27, 30, 35, 47, 48, 50, 52, 63, 79, 84, 90, 92, 121, 98 , 99 , 103 , 106 , 111 , 116 , 141 , 144, 152,157), id(id) fcolor(BuRd) legtitle ("{it:Yield Bean}") ///
	clnumber(7) ndfcolor(gs8) ndlab("missing") ///
	title ("{stSans: Rainfed Yield by FPU's}" "{stMono:`x'}" "{stSans:ALC}")note("{stMono:`name_n'}") 
	graph save `x'_`name_n'.gph, replace  
	graph export `x'_`name_n'.png, replace width(4000) 
	}
clear
}
*end



cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought\
foreach name_n in dssat__379__bean__drought__base_2000__boring___29feb16_2120 ///
dssat__379__bean__drought__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2138 ///
dssat__379__bean__drought__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2157 ///
dssat__379__bean__drought__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2215 ///
dssat__379__bean__drought__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2234 ///
dssat__379__bean__drought__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2253 {
use "`path'\`name_n'.dta",  clear
foreach x in yield {
	spmap `x' using coorfpuall if sis==2 , id(id) fcolor(BuRd) legtitle ("{it:Yield BeanTech}") ///
	clnumber(7) ndfcolor(gs8) ndlab("missing") ///
	title ("{stSans: Rainfed Yield by FPU's}" "{stMono:`x'}" "{stSans:ALC}")note("{stMono:`name_n'}") 
	graph save world`x'_`name_n'.gph, replace  
	graph export world`x'_`name_n'.png, replace width(4000) 
	
	}
clear
}

**** Graficos por zonas Frijol Drought
**** mapa para ALC 
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought\
foreach name_n in dssat__379__bean__drought__base_2000__boring___29feb16_2120 ///
dssat__379__bean__drought__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2138 ///
dssat__379__bean__drought__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2157 ///
dssat__379__bean__drought__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2215 ///
dssat__379__bean__drought__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2234 ///
dssat__379__bean__drought__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2253 {
use "`path'\`name_n'.dta",  clear
foreach x in yield {
	spmap `x' using coorfpuall if sis==2 & inlist(region, 3, 11, 21, 26 ,117, 105, 137,140, 120, 27, 30, 35, 47, 48, 50, 52, 63, 79, 84, 90, 92, 121, 98 , 99 , 103 , 106 , 111 , 116 , 141 , 144, 152,157), id(id) fcolor(BuRd) legtitle ("{it:Yield Bean}") ///
	clnumber(7) ndfcolor(gs8) ndlab("missing") ///
	title ("{stSans: Rainfed Yield by FPU's}" "{stMono:`x'}" "{stSans:ALC}")note("{stMono:`name_n'}") 
	graph save lac`x'_`name_n'.gph, replace  
	graph export lac`x'_`name_n'.png, replace width(4000) 
	
	}
clear
}
*end


**********************************************************************************************

**** mapa para Africa normal
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal\
foreach name_n in dssat__379__bean__normal__base_2000__boring___29feb16_2129 ///
dssat__379__bean__normal__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2148 ///
dssat__379__bean__normal__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2206 ///
dssat__379__bean__normal__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2225 ///
dssat__379__bean__normal__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2244 ///
dssat__379__bean__normal__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2304 {
use "`path'\`name_n'.dta",  clear
foreach x in yield {
	spmap `x' using coorfpuall if sis==2 & inlist(region, 18, 148, 25, 65, 97,70, 68, 31, 53, 75, 85, 86, 87, 91, 118, 124, 127, 129, 149, 158), id(id) fcolor(BuRd) legtitle ("{it:Yield Bean}") ///
	clnumber(7) ndfcolor(gs8) ndlab("missing") ///
	title ("{stSans: Rainfed Yield by FPU's}" "{stMono:`x'}" "{stSans:AFRICA}")note("{stMono:`name_n'}") 
	graph save `x'_`name_n'.gph, replace 
	graph export `x'_`name_n'.png, replace width(4000) 
	}
clear
}

*end


**** mapas bean con tecnología  africa
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought\
foreach name_n in dssat__379__bean__drought__base_2000__boring___29feb16_2120 ///
dssat__379__bean__drought__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2138 ///
dssat__379__bean__drought__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2157 ///
dssat__379__bean__drought__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2215 ///
dssat__379__bean__drought__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2234 ///
dssat__379__bean__drought__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2253 {
use "`path'\`name_n'.dta",  clear
foreach x in yield {
	spmap `x' using coorfpuall if sis==2 & inlist(region, 18, 148, 25, 65, 97,70, 68, 31, 53, 75, 85, 86, 87, 91, 118, 124, 127, 129, 149, 158), id(id) fcolor(BuRd) legtitle ("{it:Yield Bean}") ///
	clnumber(7) ndfcolor(gs8) ndlab("missing") ///
	title ("{stSans: Rainfed Yield by FPU's}" "{stMono:`x'}" "{stSans:AFRICA}")note("{stMono:`name_n'}") 
	graph save Africa`x'_`name_n'.gph, replace 
	graph export Africa`x'_`name_n'.png, replace width(4000) 
		
	}
clear
}

*end

**********************************************************************************************
**** mapa para Asia drought
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought\
foreach name_n in dssat__379__bean__drought__base_2000__boring___29feb16_2120 ///
dssat__379__bean__drought__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2138 ///
dssat__379__bean__drought__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2157 ///
dssat__379__bean__drought__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2215 ///
dssat__379__bean__drought__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2234 ///
dssat__379__bean__drought__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2253 {
use "`path'\`name_n'.dta",  clear
foreach x in yield {
	spmap `x' using coorfpuall if sis==2 & inlist(region,12,40,32, 5,41,8,142,9,73,4,17,134, 49, 154, 14, 39, 94, 147, 159, 20,143, 155,119, 22, 28, 34,37, 54, 55,57,	58,	59,	60,	64,	66,	67,	69,	72,	76,	77,	78,	88,	89, 107, 110, 112,	113, 122, 125, 126,	128, 132, 135, 136,	138, 139, 150, 153, 156), id(id) fcolor(BuRd) legtitle ("{it:Yield Bean}") ///
	clnumber(7) ndfcolor(gs8) ndlab("missing") legend(position(6)) ///
	title ("{stSans: Rainfed Yield by FPU's}" "{stMono:`x'}" "{stSans:ASIA}", position(12)) note("{stMono:`name_n'}", position (5)) 
	graph save asia`x'_`name_n'.gph, replace 
	graph export asia`x'_`name_n'.png, replace width(4000) 
	}
clear
}

*end

**** mapa para Asia normal
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal\
foreach name_n in dssat__379__bean__normal__base_2000__boring___29feb16_2129 ///
dssat__379__bean__normal__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2148 ///
dssat__379__bean__normal__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2206 ///
dssat__379__bean__normal__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2225 ///
dssat__379__bean__normal__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2244 ///
dssat__379__bean__normal__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2304 {
use "`path'\`name_n'.dta",  clear
foreach x in yield {
	spmap `x' using coorfpuall if sis==2 & inlist(region,12,40,32, 5,41,8,142,9,73,4,17,134, 49, 154, 14, 39, 94, 147, 159, 20,143, 155,119, 22, 28, 34,37, 54, 55,57,	58,	59,	60,	64,	66,	67,	69,	72,	76,	77,	78,	88,	89, 107, 110, 112,	113, 122, 125, 126,	128, 132, 135, 136,	138, 139, 150, 153, 156), id(id) fcolor(BuRd) legtitle ("{it:Yield Bean}") ///
	clnumber(7) ndfcolor(gs8) ndlab("missing") legend(position(6)) ///
	title ("{stSans: Rainfed Yield by FPU's}" "{stMono:`x'}" "{stSans:ASIA}", position(12)) note("{stMono:`name_n'}", position (5)) 
	graph save `x'_`name_n'.gph, replace   
	graph export `x'_`name_n'.png, replace width(4000) 
	}
clear
}

*end


***************************************************************************************
	


*******Bean Drought
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought"
use beanDrought.dta, clear
local d Technology
#delimit;
graph hbox yield  if sis==2 & yield!=0 & gcm_n!="bean__drought__base_2000" & inlist(region, 3, 11, 21, 26 ,117, 105, 137,140, 120, 27, 30, 35, 47, 48, 50, 52, 63, 79, 84, 90, 92, 121, 98 , 99 , 103 , 106 , 111 , 116 , 141 , 144, 152,157), over(id, sort(1) label(labsize(vsmall)))  
	title("{it:Bean Yield Rainfed}" "{stMono:All GCM's}")
	subtitle ("RCP8.5""`d'")
	box(1, bfcolor(ltbluishgray) blcolor(black)) 
	legend(label(1 "Bean") )  
	marker(1, mcolor(red) msymbol(Sh)msize(vsmall))
	ytitle("Yield")
	ylabel(0(500)3500, angle(45) labsize(small))
	boxgap(30) medline(lcolor(blue) lwidth(medium)) scheme(s1mono)
	box(1,color(midblue))
	xsize(6.5) ysize(10)
	note("{it:America Latina y el Caribe FPU's}");
#delimit cr 
graph save beanDrought.gph, replace
graph export beanDrought.png, width(4000) replace 




*****Bean Normal
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal"
use beanNormal.dta, clear
 local d NoTechnology
#delimit;
graph hbox yield  if sis==2 & yield!=0 & gcm_n!="bean__normal__base_2000" & inlist(region, 3, 11, 21, 26 ,117, 105, 137,140, 120, 27, 30, 35, 47, 48, 50, 52, 63, 79, 84, 90, 92, 121, 98 , 99 , 103 , 106 , 111 , 116 , 141 , 144, 152,157), over(id, sort(1) label(labsize(vsmall)))  
	title("{it:Bean Yield Rainfed}" "{stMono:All GCM's}")
	subtitle ("RCP8.5""`d'")
	box(1, bfcolor(ltbluishgray) blcolor(black)) 
	legend(label(1 "Bean") )  
	marker(1, mcolor(red) msymbol(Sh)msize(vsmall))
	ytitle("Yield")
	ylabel(0(500)3500, angle(45) labsize(small))
	boxgap(30) medline(lcolor(blue) lwidth(medium)) scheme(s1mono)
	box(1,color(midblue))
	xsize(6.5) ysize(10)
	note("{it:America Latina y el Caribe FPU's}");
#delimit cr 
graph save beanNormal.gph, replace
graph export beanNormal.png, width(4000) replace 




*************************************************************************



cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS" 
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\
foreach name_n in dssat__379__bean__drought__base_2000__boring___29feb16_2120  ///
dssat__379__bean__drought__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2138 ///
dssat__379__bean__drought__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2157 ///
dssat__379__bean__drought__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2215 ///
dssat__379__bean__drought__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2234 ///
dssat__379__bean__drought__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2253 ///
dssat__379__bean__normal__base_2000__boring___29feb16_2129 ///
dssat__379__bean__normal__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2148 ///
dssat__379__bean__normal__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2206 ///
dssat__379__bean__normal__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2225 ///
dssat__379__bean__normal__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2244 ///
dssat__379__bean__normal__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2304  {
use "`path'\`name_n'.dta",  clear
local y sis
foreach x in yield {
	spmap `x' using coorfpuall if sis==2, id(fpu) fcolor(Reds) legtitle ("{it:Yield Bean}") ///
	title ("{stSans: Bean Irrigated Yield by FPU's}" "{stMono:`x'}"  "{stMono: GCM: `y'}")
	note(subinstr(`name_n', "_es__future_rcp8p5_2041_2070__boring___29feb16_2157", " ", .) , 
	graph export `x'_`name_n'.png, replace width(4000)  
	}
save `file', replace
clear
}
end 


//// GRAPHS ////






*********************************************PROOF
****** graph proof1
sysuse dssat__379__bean__normal__base_2000__boring___29feb16_2129.dta, clear
#delimit;
graph hbox yield, over(zona, sort(1) label(labsize(vsmall)))  
	title("{it:Bean Yield}" "{stMono:BASE}")
	box(1, bfcolor(ltbluishgray) blcolor(black)) 
	legend(label(1 "BeanNormal") )  
	marker(1, mcolor(red) msymbol(Sh)msize(vsmall))
	ytitle("Yield")
	ylabel(0(1000)10000, angle(45) labsize(small))
	boxgap(30) medline(lcolor(blue) lwidth(medium)) scheme(s1mono)
	box(1,color(midblue))
	xsize(6.5) ysize(7);
#delimit cr 



*******elimiar variables que no se necesitan solo trabajamos con Yield 
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS" 
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\
foreach name_n in dssat__379__bean__drought__base_2000__boring___29feb16_2120  ///
dssat__379__bean__drought__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2138 ///
dssat__379__bean__drought__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2157 ///
dssat__379__bean__drought__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2215 ///
dssat__379__bean__drought__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2234 ///
dssat__379__bean__drought__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2253 ///
dssat__379__bean__normal__base_2000__boring___29feb16_2129 ///
dssat__379__bean__normal__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2148 ///
dssat__379__bean__normal__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2206 ///
dssat__379__bean__normal__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2225 ///
dssat__379__bean__normal__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2244 ///
dssat__379__bean__normal__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2304 {
use "`path'\`name_n'.dta",  clear
keep yrs yield fpu gcm sis
save "`path'\`name_n'.dta", replace 
clear
}
end


****bean drought
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS" 
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\
foreach name_n in dssat__379__bean__drought__base_2000__boring___29feb16_2120  ///
dssat__379__bean__drought__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2138 ///
dssat__379__bean__drought__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2157 ///
dssat__379__bean__drought__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2215 ///
dssat__379__bean__drought__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2234 ///
dssat__379__bean__drought__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2253 {
use "`path'\`name_n'.dta",  clear
keep yrs yield fpu gcm sis
save "`path'\`name_n'.dta", replace 
clear
}
end


**** organizando la variable que identifique los gcm (acortandola)
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\
foreach name_n in dssat__379__bean__normal__base_2000__boring___29feb16_2129 ///
dssat__379__bean__normal__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2148 ///
dssat__379__bean__normal__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2206 ///
dssat__379__bean__normal__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2225 ///
dssat__379__bean__normal__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2244 ///
dssat__379__bean__normal__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2304 {
gen gcm_n= substr(gcm,7,.) 
replace gcm_n = subinstr(gcm_n, "_es__future_rcp8p5_2041_2070__boring___29feb16_2157", " ", .) 
save "`path'\`name_n'.dta", replace 
clear
}
end


****************************************************************************************
****************************************************************************************
****************************************************************************************
****************************************************************************************
****************************************************************************************
****************************************************************************************
use beanDrought.dta, clear

replace gcm_n="beanBaseTechn" if gcm_n=="bean__drought__base_2000 "
replace gcm_n="hadgem2_es" if  gcm_n=="bean__drought__hadgem2_es 57"
replace gcm_n="gfdl_esm2m"  if gcm_n=="bean__drought__gfdl_esm2m 38"
replace gcm_n="ipsl_cm5a" if gcm_n=="bean__drought__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2215"
replace gcm_n="miroc_esm" if gcm_n=="bean__drought__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2234"
replace gcm_n="noresm1_m" if gcm_n=="bean__drought__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2253"

save "beanDrought.dta", replace

cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought"
use beanDrought.dta, clear
 local v `""beanBaseTechn" "gfdl_esm2m" "hadgem2_es" "ipsl_cm5a" "miroc_esm" "noresm1_m""'
 foreach var of local v {
 keep if gcm_n=="`var'"
 rename yield y_`var'
 save "`var'.dta", replace
 clear
 use beanDrought.dta, clear
}
*end 


****** simplificandola mas
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanNormal\gcms\ 
foreach name_n in gfdl_esm2m hadgem2_es ipsl_cm5a miroc_esm noresm1_m {
use "`path'\`name_n'.dta",  clear
	drop  gcm
save "`path'\`name_n'.dta",  replace
}
*end 


**** construir una base de datos completa
use beanBaseTechn.dta, clear
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought\ 
foreach name_n in gfdl_esm2m hadgem2_es ipsl_cm5a miroc_esm noresm1_m{
merge 1:1 id sis region using "`path'\`name_n'.dta"
drop _merge
save "m_`name_n'.dta", replace
}

*end
save "dataBeanTechncompleta.dta", replace
recast str gcm_n

********************************************
use beanDrought.dta, clear
*** dummies by regions
gen africa=1 if inlist(region, 18, 148, 25, 65, 97,70, 68, 31, 53, 75, 85, 86, 87, 91, 118, 124, 127, 129, 149, 158)
gen asia=1 if inlist(region,12,40,32, 5,41,8,142,9,73,4,17,134, 49, 154, 14, 39, 94, 147, 159, 20,143, 155,119, 22, 28, 34,37, 54, 55,57,	58,	59,	60,	64,	66,	67,	69,	72,	76,	77,	78,	88,	89, 107, 110, 112,	113, 122, 125, 126,	128, 132, 135, 136,	138, 139, 150, 153, 156)
gen lac=1 if inlist(region, 3, 11, 21, 26 ,117, 105, 137,140, 120, 27, 30, 35, 47, 48, 50, 52, 63, 79, 84, 90, 92, 121, 98 , 99 , 103 , 106 , 111 , 116 , 141 , 144, 152,157)
gen pot=. 
replace pot=1 if inlist(region, 18, 148, 25, 65, 97,70, 68, 31, 53, 75, 85, 86, 87, 91, 118, 124, 127, 129, 149, 158)
replace pot=2 if inlist(region,12,40,32, 5,41,8,142,9,73,4,17,134, 49, 154, 14, 39, 94, 147, 159, 20,143, 155,119, 22, 28, 34,37, 54, 55,57,	58,	59,	60,	64,	66,	67,	69,	72,	76,	77,	78,	88,	89, 107, 110, 112,	113, 122, 125, 126,	128, 132, 135, 136,	138, 139, 150, 153, 156)
replace pot=3 if inlist(region, 3, 11, 21, 26 ,117, 105, 137,140, 120, 27, 30, 35, 47, 48, 50, 52, 63, 79, 84, 90, 92, 121, 98 , 99 , 103 , 106 , 111 , 116 , 141 , 144, 152,157)
label define pot 1"Africa" 2"Asia" 3"LAC", modify
label value pot pot 
save"dataBeanTechncompleta.dta", replace


****** Graph Bar  
#delimit;
graph bar y_beanBaseTechn y_gfdl_esm2m y_hadgem2_es y_ipsl_cm5a y_miroc_esm y_noresm1_m if sis==2, 
over(pot, sort(1) label(labsize(large) angle(45)))  
title("{it: BaseTechn Vs GCMs BeanTechn,  System Rainfed }")
	ytitle("Yield")					
	ylabel(#10, angle(45) labsize(small)) 		
	scheme(s1mono)	
	bar(1, bcolor(blue)) bar(2, bcolor(red)) bar(3, bcolor(green)) bar(4, bcolor(lime)) bar(5, bcolor(orange))
	bar(4, bcolor(bluelight))
	legend( label(1 "BaseTechn") label(2 "gfdl_esm2m") label(3 "hadgem2")label(4 "ipsl_cm5a" )label(5 "miroc_esm")
	label(6 "noresm1_m"));
#delimit cr;
graph save BeanTechnVsGCMs.gph, replace
graph export BeanTechnVsGCMs.png, width(4000) replace 


****creando cambios 
****1gcm
gen cyBaseGdfl= ((y_gfdl_esm2m-y_beanBaseTechn)/y_beanBaseTechn) *100
****creando cambios 
****3gcm
gen cyBaseHadgem= ((y_hadgem2_es-y_beanBaseTechn)/y_beanBaseTechn) *100

****creando cambios 
****4gcm
gen cyBaseIpsl= ((y_ipsl_cm5a-y_beanBaseTechn)/y_beanBaseTechn) *100

****creando cambios 
****5gcm
gen cyBaseMiroc= ((y_miroc_esm-y_beanBaseTechn)/y_beanBaseTechn) *100

****creando cambios 
****6gcm
gen cyBaseNores= ((y_noresm1_m-y_beanBaseTechn)/y_beanBaseTechn) *100

save"dataBeanTechncompleta.dta", replace
use dataBeanTechncompleta.dta, clear


yBeanNormal yBeanNormalgfdl_esm2m yBeanNormalhadgem2_es yBeanNormalipsl_cm5a_lr yBeanNormalmiroc_esm_chem yBeanNormalnoresm1_m
yBeanTechn yBeanTechngfdl_esm2m yBeanTechnhadgem2_es yBeanTechnipsl_cm5a_lr yBeanTechnmiroc_esm_chem yBeanTechnnoresm1_m__

#delimit;
graph bar cyBaseGdfl cyBaseHadgem cyBaseIpsl cyBaseMiroc cyBaseNores if sis==2, 
over(pot, sort(1) label(labsize(large) angle(h)))  
title("{it:Change Between}" " " "{it:BaseTechn and  GCMs Bean  by Regions}")	ytitle("% Change")					
	ylabel(#10, angle(h) labsize(small)) 		
	scheme(s1mono)	
	bar(1, bcolor(blue)) bar(2, bcolor(red)) bar(3, bcolor(green)) bar(4, bcolor(lime)) bar(5, bcolor(orange))
	bar(4, bcolor(bluelight))
	legend( label(1 "gfdl_esm2m") label(2 "hadgem2")label(3 "ipsl_cm5a" )label(4 "miroc_esm")
	label(5 "noresm1_m"))note(Rainfed Base:2005 GCMs:2050)
	blabel(bar, position(outside) format(%9.1f) color(black));
#delimit cr;
graph save DifferencesallBeanTechnVsGCMs.gph, replace
graph export DifferencesallBeanTechnVsGCMs.png, width(4000) replace 


use dataBeanTechncompleta.dta, clear
collapse (mean)`name_n' if gcm!=1, by(commodity year  region subregionBID zona )
collapse (sum) `name_n', by (year subregionBID region zona )
rename `name_n' `name_n'GCM 
save "`name_n'GCM.dta", replace


****** Map

use dataBeanTechncompleta.dta, replace
foreach x of varlist cyBaseGdfl cyBaseHadgem cyBaseIpsl cyBaseMiroc cyBaseNores {
spmap `x' using coorfpuall if sis==2, id(id) fcolor(BuRd) legtitle ("{it:Yield Bean}") ///
clnumber(7) ndfcolor(gs8) ndlab("missing") legend(position(7)) ///
title("{it:Differences Between}" " " "{it: BaseTechn and  GCMs Bean  by Regions}"	///				
 , position(12)) note("{stMono:System Rainfed Base:2005 GCM: 2050}", position (7))
graph save `x'.gph, replace
graph export `x'.png, replace width(4000)  
}

*end


********map 

****** MERGE DROUGHT
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought"
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought\
foreach name_n in dssat__379__bean__drought__base_2000__boring___29feb16_2120 ///
dssat__379__bean__drought__gfdl_esm2m__future_rcp8p5_2041_2070__boring___29feb16_2138 ///
dssat__379__bean__drought__hadgem2_es__future_rcp8p5_2041_2070__boring___29feb16_2157 ///
dssat__379__bean__drought__ipsl_cm5a_lr__future_rcp8p5_2041_2070__boring___29feb16_2215 ///
dssat__379__bean__drought__miroc_esm_chem__future_rcp8p5_2041_2070__boring___29feb16_2234 ///
dssat__379__bean__drought__noresm1_m__future_rcp8p5_2041_2070__boring___29feb16_2253 {
use "`path'\`name_n'.dta",  clear
merge m:m id using fpudata
save "`path'\`name_n'.dta", replace 
clear 
}
*end

