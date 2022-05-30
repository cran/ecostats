## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----code12.1,fig.width=6,fig.height=7----------------------------------------
library(mvabund)
library(ecostats)
data(reveg)
reveg$abundMV=mvabund(reveg$abund) #to treat data as multivariate
plot(abundMV~treatment, data=reveg)

## ----code12.2, fig.width=6, fig.height=6--------------------------------------
data("iris")
pc = princomp(iris[,1:4],cor=TRUE)
pc
loadings(pc)
biplot( pc, xlabs=rep("\u00B0",dim(iris)[1]) )

## ----code12.3-----------------------------------------------------------------
library(psych)
fa_iris <- fa(iris[,1:4], nfactors=2, fm="ml", rotate="varimax")
loadings(fa_iris)

## ----code 12.4,fig.width=7,fig.height=7---------------------------------------
par(mfrow=c(2,2),mar=c(3,3,2,1),mgp=c(1.75,0.75,0))
for(iVar in 1:4)
{
  irisIvar = iris[,iVar]
  plotenvelope(lm(irisIvar~fa_iris$scores), which=1, col=iris$Species, main=print(names(iris)[iVar]))
}

## ----code 12.4matrix,fig.width=7,fig.height=7---------------------------------
plot(iris[,1:4],col=iris$Species)

## ----ex12.4-------------------------------------------------------------------
data(reveg)
library(psych)
fa_reveg <- try(fa(reveg$abund, nfactors=2, fm="ml", rotate="varimax"))

## ----ex12.4Resid--------------------------------------------------------------
fa_reveg <- fa(reveg$abund, nfactors=2)

## ----ex12.4plots, fig.width=8,fig.height=6------------------------------------
par(mfrow=c(3,3),mar=c(3,3,2,1),mgp=c(1.75,0.75,0))
for(iVar in 1:9)
{
  y=reveg$abund[,iVar]
  plotenvelope(lm(y~fa_reveg$scores), which=2,main=names(reveg$abund)[iVar])
}
for(iVar in 1:9)
{
  y=reveg$abund[,iVar]
  plotenvelope(lm(y~fa_reveg$scores), which=3,main=names(reveg$abund)[iVar])
}

## ----code12.5scree, fig.width=5, fig.height=4---------------------------------
plot(fa_iris$values,type="l")

## ----code12.5BICs-------------------------------------------------------------
nFactors=3 # to compare models with up to 3 factors
BICs = rep(NA,nFactors) # define the vector that BIC values go in
names(BICs) = 1:nFactors # name its values according to #factors
for(iFactors in 1:nFactors) {
  fa_iris <- fa(iris[,1:4], nfactors=iFactors, fm="ml", rotate="varimax")
  BICs[iFactors] = fa_iris$objective - log(fa_iris$nh) * fa_iris$dof
}
BICs

## ----code12.6-----------------------------------------------------------------
data(reveg)
library(gllvm)
reveg_LVM = gllvm(reveg$abund, num.lv=2, family="negative.binomial", trace=TRUE, jitter.var=0.2)
logLik(reveg_LVM)

## ----code12.6bi, fig.width=6, fig.height=6------------------------------------
ordiplot(reveg_LVM, col=as.numeric(reveg$treatment), biplot=TRUE,
ind.spp=12)

## ----code12.6plot, fig.width=9------------------------------------------------
par(mfrow=c(1,3),mar=c(3,3,1,1),mgp=c(1.75,0.75,0))
plot(reveg_LVM,which=c(1,2,5))

## ----ex12.5bic----------------------------------------------------------------
reveg_LVM1 = gllvm(reveg$abund, num.lv=1, family="negative.binomial", trace=TRUE, jitter.var=0.2)
reveg_LVM2 = gllvm(reveg$abund, num.lv=2, family="negative.binomial", trace=TRUE, jitter.var=0.2)
reveg_LVM3 = gllvm(reveg$abund, num.lv=3, family="negative.binomial", trace=TRUE, jitter.var=0.2)
reveg_LVM4 = gllvm(reveg$abund, num.lv=4, family="negative.binomial", trace=TRUE, jitter.var=0.2)
reveg_LVM5 = gllvm(reveg$abund, num.lv=5, family="negative.binomial", trace=TRUE, jitter.var=0.2)
BIC(reveg_LVM1,reveg_LVM2,reveg_LVM3,reveg_LVM4,reveg_LVM5)

## ----ex12.5, fig.width=9------------------------------------------------------
reveg_LVM1 = gllvm(reveg$abund, num.lv=2, family="poisson", trace=TRUE, jitter.var=0.2)
par(mfrow=c(1,3))
plot(reveg_LVM1,which=c(1,2,5))

## ----code12.7, fig.height=6, fig.width=6--------------------------------------
library(vegan)
ord_mds=metaMDS(reveg$abund)
plot(ord_mds$points,pch=as.numeric(reveg$treatment),col=reveg$treatment)

## ----ex12.6load---------------------------------------------------------------
library(mvabund)
data(tikus)
tikusAbund = tikus$abund[1:20,] # for 1981 and 1983 data only
tikusAbund = tikusAbund[,apply(tikusAbund,2,sum)>0] # remove zerotons

## ----ex12.6ordination, fig.height=6, fig.width=6------------------------------
tikus_mds=metaMDS(tikusAbund)
plot(tikus_mds$points,pch=as.numeric(tikus$x$time),col=tikus$x$time)

## ----ex12.6EucOrdination, fig.height=6, fig.width=6---------------------------
tikus_mdsEuc=metaMDS(log(tikusAbund+1),distance="euclidean")
plot(tikus_mdsEuc$points,pch=as.numeric(tikus$x$time),col=tikus$x$time)

## ----ex12.6plot, fig.width=7,fig.height=8-------------------------------------
tikusMV = mvabund(tikusAbund)
plot(tikusMV~tikus$x$time[1:20])

## ----ex12.6PA, fig.height=6, fig.width=6--------------------------------------
tikusPA = tikusAbund
tikusPA[tikusPA>1]=1
tikus_LVM = gllvm(tikusPA, num.lv=2, family="binomial", trace=TRUE, jitter.var=0.2)
ordiplot.gllvm(tikus_LVM, s.col=as.numeric(tikus$x$time), biplot=TRUE, ind.spp=12)

## ----ex12.6 assumptions, fig.width=9,fig.height=3.5---------------------------
par(mfrow=c(1,3))
plot(tikus_LVM,which=c(1,2,5))

## ----code12.8, fig.width=7,fig.height=5---------------------------------------
by(iris, iris$Species, function(dat){ apply(dat[,1:4],2,mean) } )
par(mfrow=c(2,2),mar=c(3,3,1,1),mgp=c(1.75,0.75,0))
plot(Sepal.Length~Species,data=iris,xlab="")
plot(Sepal.Width~Species,data=iris,xlab="")
plot(Petal.Length~Species,data=iris,xlab="")
plot(Petal.Width~Species,data=iris,xlab="")

## ----bugfixForGllvm, echo=FALSE-----------------------------------------------
detach("package:gllvm", unload=TRUE)
library(stats) #to get old predict function up and running again (I hope)

