## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----guineapig P-value--------------------------------------------------------
 pt(-2.67,10+10-2)

## ----guineapig----------------------------------------------------------------
 library(ecostats)
 data(guineapig)
 t.test(errors~treatment, data=guineapig, var.equal=TRUE, alternative="less")

## ----guineapig qqplots--------------------------------------------------------
qqenvelope(guineapig$errors[guineapig$treatment=="N"])
qqenvelope(guineapig$errors[guineapig$treatment=="C"])
by(guineapig$errors,guineapig$treatment,sd)

## ----waterQuality scatterplot-------------------------------------------------
 data(waterQuality)
 plot(quality~logCatchment, data=waterQuality)

## ----waterQuality lm----------------------------------------------------------
 data(waterQuality)
 fit_qual=lm(quality~logCatchment, data = waterQuality)
 summary(fit_qual)

## ----waterQuality approxCI----------------------------------------------------
 -11.042+c(-2,2)*1.780

## ----waterQuality CI----------------------------------------------------------
 confint(fit_qual)

## ----waterQuality resplots----------------------------------------------------
 plot(fit_qual, which=1:2)

## ----waterQuality simenvelope-------------------------------------------------
 library(ecostats)
 plotenvelope(fit_qual, which=1:2)

## ----guineapigs ttest 2sided--------------------------------------------------
 t.test(errors~treatment, data=guineapig, var.equal=TRUE)

## ----guineapigs lm------------------------------------------------------------
 ft_guineapig=lm(errors~treatment,data=guineapig)
 summary(ft_guineapig)

## ----globalPlants lm----------------------------------------------------------
 library(ecostats)
 data(globalPlants)
 plot(height~lat,data=globalPlants)
 ft_height=lm(height~lat,data=globalPlants)
 summary(ft_height)
 plotenvelope(ft_height,which=1:2)

## ----globalPlants loglm-------------------------------------------------------
 plot(height~lat,data=globalPlants,log="y")
 globalPlants$logHeight=log10(globalPlants$height) 
 ft_logHeight=lm(logHeight~lat,data=globalPlants)
 summary(ft_logHeight)
 plotenvelope(ft_logHeight,which=1:2)

## ----guineapigs ttest log-transform-------------------------------------------
 data(guineapig)
 guineapig$logErrors=log(guineapig$errors) 
 ft_logGuineapigs = lm(logErrors~treatment, data=guineapig)
 summary(ft_logGuineapigs)
 plotenvelope(ft_logGuineapigs)
 by(guineapig$logErrors,guineapig$treatment,sd)

## ----guineapigs ttest log-transform slplot------------------------------------
 plotenvelope(ft_logGuineapigs,which=3)

## ----waterQuality outliers----------------------------------------------------
 data(waterQuality)
 ft_water = lm(quality~logCatchment,data=waterQuality)
 summary(ft_water)
 ft_water2 = lm(quality~logCatchment,data=waterQuality, subset=waterQuality$logCatchment>2)
 summary(ft_water2)

