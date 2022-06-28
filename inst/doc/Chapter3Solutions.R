## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----heightLat----------------------------------------------------------------
 library(ecostats)
 data(globalPlants)
 ft_heightLat=lm(height~lat, data=globalPlants)
 summary(ft_heightLat)

## ----heightRainLat summary----------------------------------------------------
 ft_heightRainLat=lm(height~rain+lat, data=globalPlants)
 summary(ft_heightRainLat)

## ----avPlots, fig.width=4, fig.height=4---------------------------------------
library(car)
avPlots(ft_heightLat, terms = ~lat, xlab="Latitude", ylab="Height",
grid=FALSE) ##left plot
avPlots(ft_heightRainLat, terms = ~lat, xlab="Latitude|rain",
ylab="Height|rain", grid=FALSE) ## right plot

## ----anovaClim----------------------------------------------------------------
 ft_Lat=lm(height~lat,data=globalPlants)
 ft_LatClim=lm(height~lat+rain+temp,data=globalPlants)
 anova(ft_Lat,ft_LatClim)

## ----anovaClim probs----------------------------------------------------------
 ft_climproblems=lm(height~rain+rain.wetm+lat,data=globalPlants)
 summary(ft_climproblems)

## ----heightRainLat, fig.width=8, fig.height=4---------------------------------
 par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,0.75,0))
 globalPlants$loght = log(globalPlants$height)
 ft_logheightRainLat=lm(loght~rain+lat, data=globalPlants)
 plotenvelope(ft_logheightRainLat)
 summary(ft_logheightRainLat)

## ----logRain, fig.width=8, fig.height=4---------------------------------------
 par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,0.75,0))
 globalPlants$logRain = log(globalPlants$rain)
 hist(globalPlants$rain)
 hist(globalPlants$logRain)
 ft_logheightlogRainLat=lm(loght~logRain+lat, data=globalPlants)
 plotenvelope(ft_logheightlogRainLat)
 summary(ft_logheightlogRainLat)

## ----vi-----------------------------------------------------------------------
 require(car)
 vif(ft_heightRainLat)
 vif(ft_climproblems)

## ----heightPairs, fig.width=5, fig.height=5-----------------------------------
X = data.frame(globalPlants$lat,globalPlants$rain,globalPlants$rain.wetm)
cor(X)
pairs(X)

## ----seaweed anova------------------------------------------------------------
 data(seaweed)
 seaweed$Dist = factor(seaweed$Dist)
 ft_seaweed=lm(Total~Dist,data=seaweed)
 anova(ft_seaweed)

## ----seaweed confint----------------------------------------------------------
 confint(ft_seaweed)

## ----seaweed multcomp, fig.width=4, fig.height=4------------------------------
 library(multcomp)
 contDist = mcp(Dist="Tukey") # telling R to compare on the Dist factor
 compDist = glht(ft_seaweed, linfct=contDist) # run multiple comparisions
 summary(compDist)
 plot(compDist)

## ----logClimate, fig.width=8, fig.height=4------------------------------------
 par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,0.75,0))
 globalPlants$logRain = log(globalPlants$rain)
 globalPlants$logHt = log(globalPlants$height)
 ft_logheightClim=lm(loght~logRain+temp+lat, data=globalPlants)
 plotenvelope(ft_logheightClim)
 summary(ft_logheightClim)

## ----Climate pairs, fig.width=5, fig.height=5---------------------------------
X = data.frame(globalPlants$lat,globalPlants$rain,globalPlants$temp)
cor(X)
pairs(X)

## ----seaweed box, fig.width=5, fig.height=5-----------------------------------
 boxplot(Total~Dist,data=seaweed)
 by(seaweed$Total,seaweed$Dist,sd)

## ----seaweed logre plot, fig.width=8, fig.height=4----------------------------
 par(mfrow=c(1,3),mar=c(3,3,1,1),mgp=c(1.75,0.75,0))
 plotenvelope(ft_seaweed,which=1:3)

## ----seaweed trans, fig.width=4, fig.height=4---------------------------------
 seaweed$logTot = log(seaweed$Total)
 boxplot(logTot~Dist,data=seaweed)
 by(seaweed$logTot,seaweed$Dist,sd)

## ----seaweed logre, fig.width=8, fig.height=4---------------------------------
 par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,0.75,0))
 ft_loghabconf=lm(logTot~Dist,data=seaweed)
 plotenvelope(ft_loghabconf,which=1:2)
 anova(ft_loghabconf)
 library(multcomp)
 contDist = mcp(Dist="Tukey") # telling R to compare on the Dist factor
 compDistlog = glht(ft_loghabconf, linfct=contDist) # run multiple comparisions
 summary(compDistlog)
 plot(compDistlog)

## ----seaweed SMALL, fig.width=4, fig.height=4---------------------------------
 habSmall=seaweed[seaweed$Size=="SMALL",]
 ft_smhabconf=lm(logTot~Dist,data=habSmall)
 plotenvelope(ft_smhabconf,which=1:2)
 anova(ft_smhabconf)
 library(multcomp)
 contDist = mcp(Dist="Tukey") # telling R to compare on the Dist factor
 compDistsm = glht(ft_smhabconf, linfct=contDist) # run multiple comparisons
 summary(compDistsm)
 plot(compDistsm)

## ----seaweed n----------------------------------------------------------------
dim(seaweed)
dim(habSmall)

