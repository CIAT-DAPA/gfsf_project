****BEAN DROUGHT

*****BackStop

set more off
****BEAN DROUGHT
******************************************************* PASO 1. 
****abrir datos en excel y grabarlos como STATA
cd "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought" 
local files : dir "C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought" files "*.txt"
display `files'
foreach file in `files' {
    import delimited using "`file'", clear
    local noextension=subinstr("`file'",".txt","",.)
    save "`noextension'", replace
	clear
}
end 

****** coropletas BEANTECH
local path C:\Users\CEGONZALEZ\Documents\GFSF\ciat_rice_beansRicky_379\BEANS\beanDrought\
foreach name_n in dssat__379__bean__drought__base_2000__boring___29feb16_2120 {
use "`path'\`name_n'.dta",  clear
local y bean__drought__base_2000
foreach x in yield {
	spmap `x' using coorfpuall if sis==2, id(id) fcolor(BuRd) ///
	ocolor(white..) osize(vthin ..) legtitle("{it:Yield Bean Year 2005}")  clnumber(6) ///
	title ("{stSans: Bean Rainfed Yield by FPU's}" "{stMono:`x'}"  "{stMono:  base}") ///
	note("`y'") , 
	graph export `x'_`name_n'.png, replace width(4000)  
	}
save `file', replace
clear
}
end 



