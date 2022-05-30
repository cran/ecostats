## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----code14.1-----------------------------------------------------------------
library(ecostats)
library(mvabund)
data(reveg)
reveg$abundMV=mvabund(reveg$abund)
ft_reveg=manyglm(abundMV~treatment+offset(log(pitfalls)),
family="negative.binomial", data=reveg) # offset included as in Exercise 10.9
anova(ft_reveg)

## ----ex14.4an-----------------------------------------------------------------
data(seaweed)
seaweed$Dist = as.factor(seaweed$Dist)
# set up presence-absence response:
seaweed$PA = mvabund(seaweed[,6:21])
seaweed$PA[seaweed$PA>1] = 1

#fit model
ft_epiAlt = manyglm(PA~(Wmass+Size+Time)*Dist,family="cloglog", data=seaweed)
anova(ft_epiAlt,nBoot=99)

## ----ex14.4anNull-------------------------------------------------------------
ft_epiNull = manyglm(PA~Wmass+Size+Time,family="cloglog", data=seaweed)
anova(ft_epiNull, ft_epiAlt, nBoot=99)

## ----code14.2, fig.width=9,fig.height=3.3-------------------------------------
par(mfrow=c(1,3),mar=c(3,3,2,1),mgp=c(1.75,0.75,0))
ft_reveg=manyglm(abundMV~treatment,offset=log(pitfalls),family="negative.binomial", data=reveg)
plotenvelope(ft_reveg, which=1:3)

## ----code14.3, fig.width=9, fig.height=3.3------------------------------------
ft_revegP=manyglm(abundMV~treatment, offset=log(pitfalls), family="poisson", data=reveg)
par(mfrow=c(1,3),mar=c(3,3,1,1),mgp=c(1.75,0.75,0))
plotenvelope(ft_revegP, which=1:3, sim.method="stand.norm")

## ----code14.3MV, fig.width=6, fig.height=5------------------------------------
meanvar.plot(reveg$abundMV~reveg$treatment)
abline(a=0,b=1,col="darkgreen")

## ----ex14.5, fig.width=9, fig.height=3.3, warning=FALSE, eval=FALSE-----------
#  par(mfrow=c(1,3),mar=c(3,3,1,1),mgp=c(1.75,0.75,0))
#  ft_epiAlt = manyglm(PA~(Wmass+Size+Time)*Dist,family=binomial("cloglog"), data=seaweed)
#  plotenvelope(ft_epiAlt, which=1:3)

## ----ex14.6-------------------------------------------------------------------
data(windFarms)
ft_wind=manyglm(mvabund(windFarms$abund)~Station+Year+Year:Zone, family="poisson", data=windFarms$X)

## ----ex14.6assumptions, fig.width=9, fig.height=3.3, warning=FALSE, eval=FALSE----
#  par(mfrow=c(1,3),mar=c(3,3,1,1),mgp=c(1.75,0.75,0))
#  plotenvelope(ft_wind, which=1:3, sim.method="stand.norm")

## ----ex14.6refit, fig.width=9, fig.height=3.3, warning=FALSE, eval=FALSE------
#  par(mfrow=c(1,3),mar=c(3,3,1,1),mgp=c(1.75,0.75,0))
#  plotenvelope(ft_wind, which=1:3, n.sim=39)

## ----code 14.4----------------------------------------------------------------
anova(ft_reveg,test="wald",cor.type="shrink")

## ----ex14.7-------------------------------------------------------------------
windMV = mvabund(windFarms$abund)
ft_wind=manyglm(windMV~Station+Year+Year:Zone, family="poisson", data=windFarms$X)
ft_windNull=manyglm(windMV~Station+Year, family="poisson", data=windFarms$X)
anova(ft_windNull, ft_wind, nBoot=19)

## ----ex14.7Singletons---------------------------------------------------------
windMV1 = mvabund(windFarms$abund[,colSums(windFarms$abund>0)>1])
ft_wind1=manyglm(windMV1~Station+Year+Year:Zone, family="poisson", data=windFarms$X)
ft_windNull1=manyglm(windMV1~Station+Year, family="poisson", data=windFarms$X)
anova(ft_windNull1, ft_wind1, nBoot=19)

## ----ex14.7ColSums------------------------------------------------------------
colSums(windFarms$abund>0)

## ----ex14.7Tripletons---------------------------------------------------------
windMV3 = mvabund(windFarms$abund[,colSums(windFarms$abund>0)>3])
ft_wind3=manyglm(windMV3~Station+Year+Year:Zone, family="poisson", data=windFarms$X)
ft_windNull3=manyglm(windMV3~Station+Year, family="poisson", data=windFarms$X)
anova(ft_windNull3, ft_wind3, nBoot=19)

## ----code14.5, warning=FALSE--------------------------------------------------
habOrd = counts = as.matrix( round(seaweed[,6:21]*seaweed$Wmass))
 habOrd[counts>0 & counts<10] = 1
habOrd[counts>=10] = 2
library(ordinal)
summary(habOrd) # Amphipods are all "2" which would return an error in clm
habOrd=habOrd[,-1] #remove Amphipods
manyOrd=manyany(habOrd~Dist*Time*Size,"clm",data=seaweed)
manyOrdNull=manyany(habOrd~Time*Size,"clm",data=seaweed)
anova(manyOrdNull, manyOrd)

## ----detach ordinal-----------------------------------------------------------
detach("package:ordinal", unload=TRUE)

## ----code 14.6----------------------------------------------------------------
ft_comp=manyglm(abundMV~treatment+offset(log(pitfalls)), data=reveg, composition=TRUE)
anova(ft_comp,nBoot=99)

## ----code 14.7----------------------------------------------------------------
ft_null = manyglm(abundMV~cols+rows+offset(log(pitfalls)),
data=ft_comp$data)
ft_alt = manyglm(abundMV~cols+rows+treatment:cols
+offset(log(pitfalls)), data=ft_comp$data)
anova(ft_null,ft_alt,nBoot=99,block=ft_comp$rows)

## ----code14.8-----------------------------------------------------------------
ft_reveg0 = manyglm(abundMV~1+offset(log(pitfalls)), data=reveg)
QDrows0 = log(rowSums(reveg$abundMV)) - log( rowSums(fitted(ft_reveg0)) )
ft_row0=manyglm(abundMV~1+offset(log(pitfalls))+offset(QDrows0), data=reveg)
ft_reveg = manyglm(abundMV~treatment+offset(log(pitfalls)), data=reveg)
QDrows = log(rowSums(reveg$abundMV)) - log( rowSums(fitted(ft_reveg)) )
ft_row=manyglm(abundMV~treatment+offset(log(pitfalls))+offset(QDrows), data=reveg)
anova(ft_row0,ft_row)

## ----code14.9-----------------------------------------------------------------
an_reveg = anova(ft_reveg,p.uni="adjusted")
an_reveg

## ----code4.10-----------------------------------------------------------------
sortedRevegStats = sort(an_reveg$uni.test[2,],decreasing=T,
index.return=T)
sortedRevegStats$x[1:5]
sum(sortedRevegStats$x[1:5])/an_reveg$table[2,3]
coef(ft_reveg)[,sortedRevegStats$ix[1:5]]
ft_reveg$stderr[,sortedRevegStats$ix[1:5]]

## ----ex14.8, eval=FALSE-------------------------------------------------------
#  data(windFarms)
#  windMV1 = mvabund(windFarms$abund[,colSums(windFarms$abund>0)>1])
#  ft_wind1=manyglm(windMV1~Station+Year+Year:Zone, family="poisson", data=windFarms$X)
#  an_wind1 = anova(ft_wind1,p.uni="adjusted",nBoot=99)
#  sortedWindStats = sort(an_wind1$uni.test[4,], decreasing=T, index.return=T)
#  sortedWindStats$x[1:5]
#  an_wind1$uni.p[4,sortedWindStats$ix[1:5]]

## ----ex14.8prop, eval=FALSE---------------------------------------------------
#  sum(sortedWindStats$x[1:2])/an_wind1$table[4,3]

## ----ex14.8plot,fig.width=5,fig.height=8, eval=FALSE--------------------------
#  plot(windMV1~interaction(windFarms$X$Zone,windFarms$X$Year),var.subset=sortedWindStats$ix[1:4])

