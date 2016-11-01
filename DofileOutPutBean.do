*******text dynamic 
cd "C:\Users\cegonzalez\Dropbox\DatosRicky\Ricky03_08_2016\BEANS"
qui log using gfsfbeantechn, replace smcl

/***

---
title: "BackStopBeansRainfed"
author: Carlos Eduardo Gonzalez R.
date: April 2016

output:
  beamer_presentation:
    fig_caption: true
    theme: "Boadilla"
    colortheme: "dolphin"
    fonttheme: "structurebold"
---

#Techn Bean Yield, *System Rainfed "NoCC"*

![BaseTechn](dssat__379__bean__drought__base_2000__boring___29feb16_2120.png)



----

##Box of Normal and GCM Beans Yield by FPUs 

![BaseTechn](beanDrought.png)

----

# Latin America and the Caribbean, _LAC_

![LAC Average All GCM's](ALCMapbeanNormal.png)

----


# Latin America and the Caribbean, _LAC_


##BOX LAC 

![LAC Average All GCM's](LACbox_beanNormal.png)


----

#_AFRICA_

![AFRICA Average AllCM's](AFRICAMapbeanNormal.png)


----


#_AFRICA_



##BOX AFRICA 

![AFRICA AllCM's](Africabox_beanNormal.png)


----

#_ASIA_

![ASIA Average All GCM's](ASIAMapbeanNormal2.png)

----


#_ASIA_



##BOX ASIA 

![ASIA Average All GCM's](ASIAbox_beanNormal.png)


----


# Comparison of Bean Yields


## Yields


![Yields](BeanNormalVsGCMs.png)

----

#Differences Between *Normal*  and  *GCMs* Bean 


##All Regions

![Differences](DifferencesallBeanNormalVsGCMs.png)


-----


#_Change_  Map between *Normal*  and  *GCMs* Bean 



##Base_Gdfl

![Change Base 2005 GCM 2050](cyBaseGdfl.png)

---


#_Change_  Map between *Normal*  and  *GCMs* Bean 



##Base_Hadgem

![Change Base 2005 GCM 2050](cyBaseHadgem.png)

----


#_Change_  Map between *Normal*  and  *GCMs* Bean 


##Base_Ipsl

![Change Base 2005 GCM 2050](cyBaseIpsl.png)

----


#_Change_  Map between *Normal*  and  *GCMs* Bean 



##BaseMiroc

![Change Base 2005 GCM 2050](cyBaseMiroc.png)

----


#_Change_  Map between *Normal*  and  *GCMs* Bean 


##Base_Nores

![Change Base 2005 GCM 2050](cyBaseNores.png)


***/


qui log c
capture prog drop markdoc
markdoc gfsfbeantechn, export(slide) replace  



