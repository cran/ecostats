## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----code16.1 loadecoMix,eval=FALSE-------------------------------------------
#  library(devtools)
#  install_github("skiptoniam/ecomix")

## ----code16.1-----------------------------------------------------------------
is_ecomix_installed = requireNamespace("ecomix", quietly = TRUE)
library(mvabund)
data(spider)
SpiderDF=spider$x
SpiderDF$abund=as.matrix(spider$abund)
spiderFormula = abund ~ soil.dry + bare.sand + fallen.leaves + moss +
herb.layer + reflection
if(is_ecomix_installed)
{
  ft_Mix = ecomix::species_mix(spiderFormula, data=SpiderDF, family="negative.binomial", nArchetypes=2, control=list(init_method='kmeans',ecm_refit=5, ecm_steps=2) )
  coef(ft_Mix)$beta
}

## ----code16.2, fig.width=8,fig.height=4---------------------------------------
if(is_ecomix_installed)
  plot(ft_Mix, type="link")

## ----ex16.2-------------------------------------------------------------------
library(ecostats)
data(reveg)
revegDF=data.frame(reveg)
revegDF$abundMat=as.matrix(reveg$abund)
if(is_ecomix_installed)
{
  ft_revegMix = ecomix::species_mix(abundMat~treatment, data=revegDF, family="negative.binomial", nArchetypes=2, control=list(init_method='kmeans',ecm_refit=5, ecm_steps=2) )
  coef(ft_revegMix)$beta
}

## ----code16.3, message=FALSE, eval=FALSE--------------------------------------
#  if(is_ecomix_installed)
#  {
#    nClust=rep(2:6,3)
#    bics = rep(NA, length(nClust))
#    for(iClust in 1:length(nClust))
#    {
#      fti_Mix = ecomix::species_mix(spiderFormula, data=SpiderDF,
#        family="negative.binomial", nArchetypes=nClust[iClust],
#        control=list(init_method='kmeans',ecm_refit=5, ecm_steps=2))
#      bics[iClust] = BIC(fti_Mix)
#    }
#    plot(bics~nClust, ylab="BIC", xlab="# archetypes")
#  }

## ----ex16.3, message=FALSE, eval=FALSE----------------------------------------
#  if(is_ecomix_installed)
#  {
#    nClust=rep(1:5,3)
#    bics = rep(NA, length(nClust))
#    for(iClust in 1:length(nClust))
#    {
#      fti_Mix = ecomix::species_mix(abundMat~treatment, data=revegDF,
#        family="negative.binomial", nArchetypes=nClust[iClust])
#      bics[iClust] = BIC(fti_Mix)
#    }
#    plot(bics~nClust, ylab="BIC", xlab="# archetypes")
#  }

## ----code16.4-----------------------------------------------------------------
library(gllvm)
data(spider)
X = spider$x[,c("soil.dry","herb.layer")]
ft_trait = gllvm(spider$abund, X, spider$trait, randomX=~soil.dry+herb.layer, family="negative.binomial")
logLik(ft_trait)
library(lattice)
a = max( abs(ft_trait$fourth.corner) )
colort = colorRampPalette(c("blue","white","red"))
plot.4th = levelplot(ft_trait$fourth.corner, col.regions=colort(100),
           at=seq(-a, a, length=100), scales = list( x=list(rot = 45)) )
print(plot.4th)
coefplot(ft_trait)

## ----ex16.5data---------------------------------------------------------------
data(antTraits)
envMat = as.matrix(antTraits$env)

## ----ex16.5, eval=FALSE-------------------------------------------------------
#  ft_anttrait = gllvm(antTraits$abund, envMat, antTraits$traits, randomX=~envMat, family="negative.binomial", starting.val="zero")
#  logLik(ft_anttrait)
#  library(lattice)
#  a = max( abs(ft_anttrait$fourth.corner) )
#  colort = colorRampPalette(c("blue","white","red"))
#  plot_4th = levelplot(t(as.matrix(ft_anttrait$fourth.corner)),
#    col.regions=colort(100), at=seq(-a, a, length=100),
#    scales = list( x= list(rot = 45)) )
#  print(plot_4th)
#  coefplot(ft_anttrait)

## ----ex16.5assumptions, fig.width=8,fig.height=5, eval=FALSE------------------
#  par(mfrow=c(1,2),mgp=c(1.75,0.75,0),mar=c(3,3,1,1))
#  plot(ft_anttrait, which=1:2)

## ----code16.5, fig.width=5,fig.height=5---------------------------------------
nVars = dim(spider$abund)[2]
newTraits = spider$trait
# set factors not of interest here to be a constant value
newTraits$length= mean(spider$trait$length) #set length to its mean
newTraits$colour=factor(rep(levels(spider$trait$colour)[1],nVars),
   levels=levels(spider$trait$colour)) #set to first level of factor
# set starting rows of ’marks’ to take all possible values
nMarks = nlevels(spider$trait$marks)
newTraits$marks[1:nMarks]=levels(spider$trait$marks)
# create a new env dataset where the only thing that varies is soil:
newEnv = spider$x[1:2,c("soil.dry","herb.layer")]
newEnv[,"soil.dry"]=range(scale(spider$x[,"soil.dry"]))
newEnv[,"herb.layer"]=0
#make predictions and plot:
newPreds = predict(ft_trait,newX=newEnv,newTR=newTraits,type="response", level=0)
matplot(newEnv[,1], newPreds[,1:nMarks],type="l", log="y")
legend("topright",levels(newTraits$marks),lty=1:nMarks,col=1:nMarks)

## ----ex16.6, fig.width=5,fig.height=5, eval=FALSE-----------------------------
#  nVars = dim(antTraits$abund)[2]
#  newTraits = antTraits$traits
#  #set length vars to their mean
#  newTraits$Femur.length= mean(antTraits$traits$Femur.length)
#  newTraits$Webers.length= mean(antTraits$traits$Webers.length)
#  # set factors not of interest here to be a constant value
#  newTraits$No.spines=rep(0,nVars)
#  newTraits$Pilosity = factor(rep(levels(antTraits$traits$Pilosity)[1],nVars),
#  # set first few levels of Polymorphism to all possible values
#  levels=levels(antTraits$traits$Pilosity)) #set to first level of factor
#  nPoly = nlevels(antTraits$traits$Polymorphism)
#  newTraits$Polymorphism[1:nPoly]=levels(antTraits$traits$Polymorphism)
#  # create a new env dataset where the only thing that varies is Canopy.cover:
#  newEnv = antTraits$env
#  newEnv$Canopy.cover=range(scale(newEnv$Canopy.cover))
#  newEnv$Bare.ground = mean(newEnv$Bare.ground)
#  newEnv$Shrub.cover = mean(newEnv$Shrub.cover)
#  newEnv$Volume.lying.CWD = mean(newEnv$Volume.lying.CWD)
#  newEnv$Feral.mammal.dung = mean(newEnv$Feral.mammal.dung)
#  newEnv=as.matrix(newEnv)
#  #make predictions and plot:
#  newPreds = predict(ft_anttrait,newX=newEnv,newTR=newTraits,type="response", level=0)
#  matplot(newEnv[,"Canopy.cover"], newPreds[,1:nPoly],type="l", ylab="Mean abundance", xlab="Canopy cover", log="y")
#  legend("topright",levels(newTraits$Polymorphism),lty=1:nPoly,col=1:nPoly)

## ----code16.6-----------------------------------------------------------------
ft_spp = gllvm(spider$abund, X, family="negative.binomial")
ft_trait = gllvm(spider$abund, X, spider$trait, family="negative.binomial")
ft_main = gllvm(spider$abund, X, spider$trait, family="negative.binomial", formula=~soil.dry+herb.layer)
an_spider4th = anova(ft_main, ft_trait, ft_spp)
an_spider4th
an_spider4th$D[2]/sum(an_spider4th$D)

## ----ex16.7-------------------------------------------------------------------
ft_antspp = gllvm(antTraits$abund, envMat, family="negative.binomial")
ft_anttrait = gllvm(antTraits$abund, envMat, antTraits$traits, family="negative.binomial")
mainFormula=as.formula(paste("~", paste(colnames(envMat),collapse="+")))
ft_antmain = gllvm(antTraits$abund, envMat, antTraits$traits, family="negative.binomial", formula=mainFormula)
an_ant4th = anova(ft_antmain, ft_anttrait, ft_antspp)
an_ant4th
an_ant4th$D[2]/sum(an_ant4th$D)

## ----bugfixForGllvm, echo=FALSE-----------------------------------------------
detach("package:gllvm", unload=TRUE)
library(stats)

