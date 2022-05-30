## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----box10.1.1----------------------------------------------------------------
library(ecostats)
data(seaweed)
seaweed$CrabPres = seaweed$Crab>0
ft_crab = glm(CrabPres~Time*Dist, family=binomial("cloglog"),
             data=seaweed)

## ----box10.1.2----------------------------------------------------------------
data(windFarms)
eels = windFarms$abund[,16]
ft_eels = glm(eels~Station+Year*Zone,family="poisson",
             data=windFarms$X)

## ----box10.1.3----------------------------------------------------------------
data(reveg)
Haplotaxida=reveg$abund[,12]
library(mvabund)
worms = reveg$abund$Haplotaxida
ft_worms = manyglm(worms~treatment,family="negative.binomial", data=reveg)

## ----fig10.5,fig.height=8,fig.width=8-----------------------------------------
data(reveg)
library(mvabund)
revegMV=mvabund(reveg$abund)
treatment=reveg$treatment
meanvar.plot(revegMV~treatment,legend=T,col=c("darkorange","darkgreen"),main="Poisson")
x=10^(seq(-1,3.8,length=100))
lines(x,x,type="l",col="red")

meanvar.plot(revegMV~treatment,legend=T,col=c("darkorange","darkgreen"), main="Negative binomial")
x=10^(seq(-1,3.8,length=100))
points(x,x+x^2,type="l",col="darkblue")

## ----box 10.2-----------------------------------------------------------------
seaweed$Dist = as.factor(seaweed$Dist)
ft_crab = glm(CrabPres~Time*Dist, family=binomial("cloglog"),
             data=seaweed)
summary(ft_crab)

## ----box10.3------------------------------------------------------------------
library(mvabund)
ftMany_crab = manyglm(CrabPres~Time*Dist,family=binomial("cloglog"),
             data=seaweed)
plotenvelope(ftMany_crab)

## ----fig10.8------------------------------------------------------------------
plotenvelope(ftMany_crab, which=1)
plotenvelope(ftMany_crab, which=1)

## ----ex10.4,fig.width=6,fig.height=6------------------------------------------
with(seaweed,boxplot(Ost~Dist))

## ----box10.4, fig.width=8-----------------------------------------------------
seaweed$logWmass = log(seaweed$Wmass)
ft_countOst=manyglm(Ost~logWmass+Time*Dist,data=seaweed,
             family="poisson")
par(mfrow=c(1,3),mar=c(3,3,1.5,0.5),mgp=c(1.75,0.75,0))
plotenvelope(ft_countOst,which=1:3) # for a scale-location plot as well

## ----box10.4b, fig.width=8----------------------------------------------------
library(MASS) #this line is currently needed to deal with a bug but prob redundant soon
par(mfrow=c(1,3),mar=c(3,3,1.5,0.5),mgp=c(1.75,0.75,0))
ft.countOstNB=manyglm(Ost~logWmass+Time*Dist,data=seaweed)
plotenvelope(ft.countOstNB,which=1:3)

## ----ex10.5, fig.width=8, warning=FALSE, eval=FALSE---------------------------
#  data(windFarms)
#  eels = windFarms$abund[,16]
#  ft_eels = manyglm(eels~Station+Year*Zone,family="poisson",
#               data=windFarms$X)
#  par(mfrow=c(1,3),mar=c(3,3,1.5,0.5),mgp=c(1.75,0.75,0))
#  plotenvelope(ft_eels, which=1:3)

## ----box 10.6Poisson, fig.width=8---------------------------------------------
data(reveg)
Haplotaxida=reveg$abund[,12]
worms = reveg$abund$Haplotaxida
ft_worms = manyglm(worms~treatment,family=poisson(), data=reveg)
par(mfrow=c(1,2),mar=c(3,3,1.5,0.5),mgp=c(1.75,0.75,0))
plotenvelope(ft_worms, which=1:2)

## ----box 10.6NB, fig.width=8--------------------------------------------------
ft_wormsNB = manyglm(worms~treatment,family="negative.binomial", data=reveg)
par(mfrow=c(1,2),mar=c(3,3,1.5,0.5),mgp=c(1.75,0.75,0))
plotenvelope(ft_wormsNB, which=1:2)

## ----ex10.6 bic---------------------------------------------------------------
BIC(ft_worms, ft_wormsNB)

## ----box10.5------------------------------------------------------------------
anova(ft_crab, test="Chisq")

## ----ex10.7-------------------------------------------------------------------
ft_eelsSt = glm(eels~Station+Year+Year:Zone,family="poisson",
             data=windFarms$X)

## ----ex10.7anova--------------------------------------------------------------
anova(ft_eelsSt,test='Chisq')

## ----box10.6------------------------------------------------------------------
ftmany_Hap=manyglm(Haplotaxida~treatment,family="negative.binomial",
              data=reveg)
anova(ftmany_Hap)

## ----box10.7------------------------------------------------------------------
ftMany_crab = manyglm(CrabPres~Time*Dist, family=binomial("cloglog"),
                 data=seaweed)
anova(ftMany_crab)

## ----box10.8glm---------------------------------------------------------------
ft_wormPois = glm(Haplotaxida~treatment, family="poisson", data=reveg)
anova(ft_wormPois,test="Chisq")

## ----box10.8manyglm-----------------------------------------------------------
ft_wormmanyPois = manyglm(Haplotaxida~treatment,family="poisson",
                     data=reveg)
anova(ft_wormmanyPois)

## ----box10.9Load,fig.width=6--------------------------------------------------
data(seedsTemp)
seedsTemp$propGerm = seedsTemp$NumGerm / seedsTemp$NumSown
plot(propGerm/(1-propGerm)~Test.Temp,data=seedsTemp,log="y")

## ----box10.9fit---------------------------------------------------------------
library(lme4)
seedsTemp$ID = 1:length(seedsTemp$NumGerm)
ft_temp = glmer(cbind(NumGerm,NumSown-NumGerm)~poly(Test.Temp,2)+
               (1|ID),data=seedsTemp,family="binomial")
summary(ft_temp)

## ----ex10.8P,fig.width=8,fig.height=3.5---------------------------------------
ants = reveg$abund$Formicidae
ants_Poisson = glm(ants~treatment,data=reveg,family=poisson())
par(mfrow=c(1,3),mar=c(3,3,1.5,0.5),mgp=c(1.75,0.75,0))
plotenvelope(ants_Poisson,which=1:3)

## ----ex10.8plot, fig.width=6,fig.height=6-------------------------------------
with(reveg,boxplot(abund$Formicidae~treatment))

## ----ex10.8anova--------------------------------------------------------------
anova(ants_Poisson,test='Chisq')

## ----ex10.8NB,fig.width=8,fig.height=3.5--------------------------------------
ants = reveg$abund$Formicidae
ants_NB = manyglm(ants~treatment,data=reveg,family="negative.binomial")
anova(ants_NB)

## ----ex10.8NBplots, fig.width=8-----------------------------------------------
par(mfrow=c(1,3),mar=c(3,3,1.5,0.5),mgp=c(1.75,0.75,0))
plotenvelope(ants_NB,which=1:3)

## ----box10.10-----------------------------------------------------------------
ftmany_hapoffset = manyglm(Haplotaxida~treatment+offset(log(pitfalls)),
                            family="negative.binomial", data=reveg)
anova(ftmany_hapoffset)

## ----ex10.10, fig.height=3.5, fig.width=8-------------------------------------
roaches = reveg$abund$Blattodea
ftMany_roaches = manyglm(roaches~treatment,offset=log(pitfalls), family="poisson", data=reveg)
par(mfrow=c(1,3),mar=c(3,3,1.5,0.5),mgp=c(1.75,0.75,0))
plotenvelope(ftMany_roaches,which=1:3)
anova(ftMany_roaches)

