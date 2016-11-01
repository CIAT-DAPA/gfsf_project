*******text dynamic 
cd "C:\Users\cegonzalez\Dropbox\DatosRicky\Ricky03_08_2016\BEANS"
qui log using gfsf, replace smcl

/***

---
title: "BackStopBeansRainfed"
author: Carlos Eduardo Gonzalez R.
date: February 2016

output:
  beamer_presentation:
    fig_caption: true
    theme: "Boadilla"
    colortheme: "dolphin"
    fonttheme: "structurebold"
---

#Yield Bean Normal Rainfed NoCC

![BaseNormal](Worldyield_dssat__379__bean__normal__base_2000__boring___29feb16_2129.png)


----


#Yield Bean GCM's by FPU"s 

![BaseNormal](box_beanNormal.png)

----

#AverageGCMS


![AverageGCMS](MapbeanNormal.png)

----

#LAC

![LAC Average All GCM's](ALCMapbeanNormal.png)

----

##LAC box

![LAC Average All GCM's](LACbox_beanNormal.png)


----

#AFRICA

![AFRICA Average AllCM's](AFRICAMapbeanNormal.png)


----

##AFRICA BOX

![AFRICA AllCM's](Africabox_beanNormal.png)


----

#ASIA

![ASIA Average All GCM's](ASIAMapbeanNormal2.png)

----

##ASIA BOX

![ASIA Average All GCM's](ASIAbox_beanNormal.png)


----

#Differences Between Bean Normal and Bean GCms 

##All Regions

![(BeanNormalVsGCMs.png)


----
#All Regions

![(DifferencesallBeanNormalVsGCMs.png)


----

#Maps diferences between Base and GCMs
![Base 2005 GCM 2050(cyBaseGdfl.png)
![Base 2005 GCM 2050(cyBaseHadgem.png)
![Base 2005 GCM 2050(cyBaseIpsl.png)
![Base 2005 GCM 2050(cyBaseMiroc.png)
![Base 2005 GCM 2050(cyBaseNores.png)

----

# Bean Drought

![Base 2005 GCM 2050(yield_dssat__379__bean__drought__base_2000__boring___29feb16_2120.png)


***/


qui log c
capture prog drop markdoc
markdoc gfsf, export(slide) replace  




