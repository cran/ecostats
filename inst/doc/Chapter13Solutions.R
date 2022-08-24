## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----code13.1-----------------------------------------------------------------
library(MASS)
data(Animals)
ftBrainBody=lm(log(brain)~log(body),data=Animals)
confint(ftBrainBody)

## ----code13.1Flipped----------------------------------------------------------
ftBodyBrain=lm(log(body)~log(brain),data=Animals)
confint(ftBodyBrain)

## ----box13.2------------------------------------------------------------------
library(smatr)
sma_brainBody = sma(brain~body, data=Animals,log="xy",slope.test=2/3)
sma_brainBody

## ----code13.2flipped----------------------------------------------------------
sma(body~brain, data=Animals,log="xy",slope.test=3/2)

## ----code13.3, fig.width=6, fig.height=4--------------------------------------
data(leaflife)
leafSlopes = sma(longev~lma*site, log="xy", data=leaflife)
summary(leafSlopes)
plot(leafSlopes)

## ----code13.4-----------------------------------------------------------------
leaf_low_soilp = subset(leaflife, soilp == "low")
leafElev = sma(longev~lma+rain, log="xy", data=leaf_low_soilp)
leafElev

## ----code13.5,fig.width=8,fig.height=4----------------------------------------
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(1.75,0.75,0))
{plot(sma_brainBody,which="residual") # residual plot
abline(a=0,b=0,col="red")}
qqnorm(residuals(sma_brainBody))
qqline(residuals(sma_brainBody), col="red")

## ----code13.6, fig.width=6, fig.height=6--------------------------------------
sma_brainBodyRobust = sma(brain~body, data=Animals,log="xy",
slope.test=2/3,robust=TRUE)
sma_brainBodyRobust
plot(brain~body,data=Animals,log="xy")
abline(sma_brainBody, col="red")
abline(sma_brainBodyRobust, col="blue")

## ----ex13.3-------------------------------------------------------------------
AnimalsSnipped=Animals[-c(6,16,26),]
sma_brainBody = sma(brain~body, data=AnimalsSnipped,log="xy",slope.test=2/3)
sma_brainBody

## ----ex13.3robust-------------------------------------------------------------
sma_brainBodyRobust = sma(brain~body, data=AnimalsSnipped,log="xy",
slope.test=2/3,robust=TRUE)
sma_brainBodyRobust

## ----ex13.4Slope--------------------------------------------------------------
leafSlopes = sma(longev~lma*site, log="xy", data=leaflife, robust=TRUE)
summary(leafSlopes)

## ----ex13.4elev---------------------------------------------------------------
leaf_low_soilp = subset(leaflife, soilp == "low")
leafElev = sma(longev~lma+rain, log="xy", data=leaf_low_soilp, robust=TRUE)
leafElev

