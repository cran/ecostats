## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----box11.1leaf--------------------------------------------------------------
library(smatr)
data(leaflife)
Yleaf=cbind(leaflife$lma,leaflife$longev)
colnames(Yleaf)=c("lma","longev")
var(Yleaf)

## ----box11.1iris--------------------------------------------------------------
data("iris")
var(iris[,1:4])

## ----box11.2------------------------------------------------------------------
library(smatr)
data(leaflife)
Yleaf = cbind(leaflife$lma,leaflife$longev)
ft_leaf = lm(Yleaf~rain*soilp, data=leaflife)
anova(ft_leaf, test="Wilks")

## ----box11.2plot, fig.width=6,fig.height=5------------------------------------
plot(leaflife$lma~leaflife$longev, xlab="Leaf longevity (years)",
                     ylab="Leaf mass per area (mg/mm^2)",
                     col=interaction(leaflife$rain,leaflife$soilp))
legend("bottomright",legend=c("high rain, high soilp",
         "low rain, high soilp", "high rain, low soilp",
         "low rain, low soilp"), col=1:4, pch=1)

## ----box11.3,fig.width=8,fig.height=3.5---------------------------------------
par(mfrow=c(1,3),mar=c(3,3,1.5,0.5),mgp=c(1.75,0.75,0))
library(ecostats)
plotenvelope(ft_leaf,which=1:3)

## ----box11.4------------------------------------------------------------------
library(mvabund)
ftmany_leaf = manylm(Yleaf~rain*soilp, data=leaflife)
anova(ftmany_leaf,cor.type="R",test="LR")

## ----ex11.4, fig.width=8, fig.height=3.5--------------------------------------
YleafLog = log(Yleaf)
ft_leafLog = lm(YleafLog~rain*soilp, data=leaflife)
anova(ft_leafLog, test="Wilks")
par(mfrow=c(1,3),mar=c(3,3,1.5,0.5),mgp=c(1.75,0.75,0))
plotenvelope(ft_leafLog,which=1:3)

## ----ex11.5Un,fig.width=8,fig.height=3.5--------------------------------------
data(iris)
iris$Yflower = as.matrix(iris[,1:4])
ft_iris = lm(Yflower~Species,data=iris)
par(mfrow=c(1,3),mar=c(3,3,1.5,0.5),mgp=c(1.75,0.75,0))
plotenvelope(ft_iris,which=1:3)

## ----ex11.5Log, fig.width=8,fig.height=3.5------------------------------------
iris$YfLog = as.matrix(log(iris[,1:4]))
ft_irisLog = lm(YfLog~Species,data=iris)
par(mfrow=c(1,3),mar=c(3,3,1.5,0.5),mgp=c(1.75,0.75,0))
plotenvelope(ft_irisLog,which=1:3)

## ----code11.5short------------------------------------------------------------
library(mvabund)
library(reshape2)
data(spider)
Alop=apply(spider$abund[,1:3],1,sum)
Pard=apply(spider$abund[,7:10],1,sum)
Troc = spider$abund[,11]
spidGeneraWide = data.frame(rows=1:28,scale(spider$x[,c(1,4)]), Alop,Pard,Troc)
head(spidGeneraWide)

## ----code11.5long-------------------------------------------------------------
spiderGeneraLong = melt(spidGeneraWide,id=c("rows","soil.dry","moss"))
names(spiderGeneraLong)[4:5] = c("genus","abundance")
head(spiderGeneraLong)

## ----code11.6-----------------------------------------------------------------
library(glmmTMB)
spid_glmm = glmmTMB(abundance~genus+soil.dry:genus+moss:genus
 +(0+genus|rows), family="poisson",data=spiderGeneraLong)
summary(spid_glmm)

## ----code 11.7----------------------------------------------------------------
library(MCMCglmm)
set.seed(1)
ft_MCMC = MCMCglmm(cbind(Alop,Pard,Troc)~trait+soil.dry:trait+moss:trait,
   rcov=~us(trait):units,data=spidGeneraWide, family=rep("poisson",3))
summary(ft_MCMC)

## ----code 11.7corr------------------------------------------------------------
mean(ft_MCMC$VCV[,2]/sqrt(ft_MCMC$VCV[,1]*ft_MCMC$VCV[,5]))

## ----code 11.7hists,fig.width=9,fig.height=3----------------------------------
par(mfrow=c(1,3),mgp=c(1.75,0.75,0),mar=c(3,3,1,1))
hist(ft_MCMC$VCV[,1],breaks=15,xlab="Alop variance",main="")
abline(v=summary(spid_glmm)$varcor$cond$rows[1,1],col="red")
hist(ft_MCMC$VCV[,5],breaks=15,xlab="Pard variance",main="")
abline(v=summary(spid_glmm)$varcor$cond$rows[2,2],col="red")
hist(ft_MCMC$VCV[,9],breaks=15,xlab="Troc variance",main="")
abline(v=summary(spid_glmm)$varcor$cond$rows[3,3],col="red")

## ----commonSlope--------------------------------------------------------------
spid_sameResponse = glmmTMB(abundance~genus+soil.dry+moss
 +(0+genus|rows), family="poisson",data=spiderGeneraLong)
summary(spid_sameResponse)

## ----anovaSlope---------------------------------------------------------------
anova(spid_sameResponse,spid_glmm)

## ----fig11.4, fig.width=6, fig.height=6---------------------------------------
  spid_glmm0 = glmmTMB(abundance~0+genus+soil.dry:genus+moss:genus
   +(0+genus|rows), family="poisson",data=spiderGeneraLong)

  par(mgp=c(2,0.75,0),mar=c(3,3,0.5,0.5))
  plot(log(abundance)~soil.dry,data=spiderGeneraLong,type="n",yaxt="n",
       ylab="Abundance[log scale]",xlab="Soil dryness [standardised]")
  yTicks = c(1,2,5,10,20,50,100)
  axis(2,at=log(yTicks),labels=yTicks)
  for(iVar in 1:nlevels(spiderGeneraLong$genus))
  {
    points(log(abundance)~soil.dry,
           data=spiderGeneraLong[spiderGeneraLong$genus==levels(spiderGeneraLong$genus)[iVar],],col=iVar)
    abline(spid_glmm0$fit$par[iVar], spid_glmm0$fit$par[3+iVar],col=iVar)
  }
  legend("topleft",levels(spiderGeneraLong$genus),col=1:3,pch=1,cex=0.9,y.intersp=1.0)


## ----code11.8, fig.width=8, fig.height=4--------------------------------------
par(mfrow=c(1,2),mgp=c(2,0.75,0),mar=c(3,3,1,1))
library(DHARMa)
spidFits = predict(spid_glmm,re.form=NA)
res_spid = qnorm( simulateResiduals(spid_glmm)$scaledResiduals )
plot(spidFits,res_spid,col=spiderGeneraLong$genus, xlab="Fitted values", ylab="Dunn-Smyth residuals")
abline(h=0,col="olivedrab")
addSmooth(spidFits,res_spid) # a function in ecostats package to add smoother and confidence band
qqenvelope(res_spid,col=spiderGeneraLong$genus)

## ----code11.9, fig.width=8, fig.height=9--------------------------------------
set.seed(2)
ft_MCMC2=MCMCglmm(cbind(Alop,Pard,Troc)~trait+soil.dry:trait+moss:trait,
rcov=~us(trait):units,data=spidGeneraWide, family=rep("poisson",3))
set.seed(3)
ft_MCMC3=MCMCglmm(cbind(Alop,Pard,Troc)~trait+soil.dry:trait+moss:trait,
rcov=~us(trait):units,data=spidGeneraWide, family=rep("poisson",3))
whichPlot=c(1:3,5:6,9) # indices of unique variance-covariance parameters
par(mfrow=c(length(whichPlot),1),mar=c(2,0.5,1.5,0))
for(iPlot in whichPlot)
{
plot.default(ft_MCMC$VCV[,iPlot],type="l",lwd=0.3,yaxt="n")
lines(ft_MCMC2$VCV[,iPlot],col=2,lwd=0.3)
lines(ft_MCMC3$VCV[,iPlot],col=3,lwd=0.3)
mtext(colnames(ft_MCMC$VCV)[iPlot])
}
gelman.diag(mcmc.list(ft_MCMC$VCV[,whichPlot],ft_MCMC2$VCV[,whichPlot],
ft_MCMC3$VCV[,whichPlot]))

## ----ex11.7 tmb---------------------------------------------------------------
spider3Wide = data.frame(rows=1:28, scale(spider$x[,c(1,4)]), spider$abund[,1:3])
spider3Long = melt(spider3Wide,id=c("rows","soil.dry","moss"))
names(spider3Long)[4:5] = c("species","abundance")
head(spider3Long)

spid_glmm3 = glmmTMB(abundance~species+soil.dry:species+moss:species
   +(0+species|rows), family="poisson",data=spider3Long)


## ----ex11.7 mcmc, fig.width=8, fig.height=9-----------------------------------
set.seed(1)
ft_MCMC2=try(MCMCglmm(cbind(Alopacce,Alopcune,Alopfabr)~trait+soil.dry:trait+moss:trait,
rcov=~us(trait):units,data=spider3Wide, family=rep("poisson",3))) #this one returns an error
set.seed(3)
ft_MCMC=MCMCglmm(cbind(Alopacce,Alopcune,Alopfabr)~trait+soil.dry:trait+moss:trait,
rcov=~us(trait):units,data=spider3Wide, family=rep("poisson",3))
set.seed(5)
ft_MCMC2=MCMCglmm(cbind(Alopacce,Alopcune,Alopfabr)~trait+soil.dry:trait+moss:trait,
rcov=~us(trait):units,data=spider3Wide, family=rep("poisson",3))
set.seed(6)
ft_MCMC3=MCMCglmm(cbind(Alopacce,Alopcune,Alopfabr)~trait+soil.dry:trait+moss:trait,
rcov=~us(trait):units,data=spider3Wide, family=rep("poisson",3))
whichPlot=c(1:3,5:6,9) # indices of unique variance-covariance parameters
par(mfrow=c(length(whichPlot),1),mar=c(2,0.5,1.5,0))
for(iPlot in whichPlot)
{
plot.default(ft_MCMC$VCV[,iPlot],type="l",lwd=0.3,yaxt="n")
lines(ft_MCMC2$VCV[,iPlot],col=2,lwd=0.3)
lines(ft_MCMC3$VCV[,iPlot],col=3,lwd=0.3)
mtext(colnames(ft_MCMC$VCV)[iPlot])
}
gelman.diag(mcmc.list(ft_MCMC$VCV[,whichPlot],ft_MCMC2$VCV[,whichPlot],
ft_MCMC3$VCV[,whichPlot]))

