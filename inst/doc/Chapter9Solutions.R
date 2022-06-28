## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fig91, fig.width=6, fig.height=5-----------------------------------------
library(ecostats)
data(guineapig)
ft_guinea = lm(errors~treatment,data=guineapig)
tObs = summary(ft_guinea)$coef[2,3] #store observed t-statistic

nPerm = 1000
tStats = rep(NA,nPerm)
tStats[1] = tObs
for(iPerm in 2:nPerm)
{
  guineapig$treatPerm = sample(guineapig$treatment) #permute treatment labels
  ft_guineaPerm = lm(errors~treatPerm,data=guineapig) #re-fit model
  tStats[iPerm] = summary(ft_guineaPerm)$coef[2,3] #store t-stat
}
par(cex=1.2,lwd=1.5)
hist(tStats,main="Null distribution of t under permutation",xlab="t")
abline(v=tObs,col="red") #put a red line on plot for observed t-stat
p = mean( tStats >= tObs ) #compute P-value
print(p)

## ----box91--------------------------------------------------------------------
library(mvabund)
data(guineapig)
ft_guinea = manylm(errors~treatment,data=guineapig)
anova(ft_guinea)

## ----box92--------------------------------------------------------------------
data(globalPlants)
ft_height = manylm(height~lat, data=globalPlants)
anova(ft_height)

## ----box93--------------------------------------------------------------------
library(mvabund)
ft_guinea = manylm(errors~treatment, data=guineapig)
anova(ft_guinea, resamp="residual")

## ----ex94---------------------------------------------------------------------
ft_heightR=lm(height~rain, data=globalPlants)

## ----box94--------------------------------------------------------------------
ft_heightRL=manylm(height~rain+lat, data=globalPlants)
anova(ft_heightRL, resamp="perm.resid")

## ----box95, fig.width=4, fig.height=3.5---------------------------------------
ft_heightRLlm = lm(height~lat, data=globalPlants)
plotenvelope(ft_heightRLlm, n.sim=99)

## ----ex95, fig.width=4, fig.height=3.5----------------------------------------
globalPlants$loght = log(globalPlants$height)
ft_loghtRL=manylm(loght~rain+lat, data=globalPlants)
anova(ft_loghtRL, resamp="perm.resid")
ft_loghtRLlm = lm(loght~rain+lat, data=globalPlants)
plotenvelope(ft_loghtRLlm, n.sim=99)

## ----ex95 lm------------------------------------------------------------------
anova(ft_loghtRLlm)

## ----ex96, fig.width=4, fig.height=3.5----------------------------------------
data(guineapig)
guineapig$logErrors = log(guineapig$errors)
ft_guineaLog = lm(logErrors~treatment,data=guineapig)
plotenvelope(ft_guineaLog, n.sim=99)
by(guineapig$logErrors,guineapig$treatment,sd)

## ----ex96 anova---------------------------------------------------------------
library(mvabund)
ftMany_guineaLog = manylm(logErrors~treatment,data=guineapig)
anova(ftMany_guineaLog)

## ----ex49, fig.width=4, fig.height=3.5----------------------------------------
 data(snowmelt)
 snowmelt$logFlow = log(snowmelt$flow)
 snowmelt$logFlow[snowmelt$logFlow==-Inf]=NA
 snowReduced = na.omit(snowmelt[,c("logFlow","snow","elev")]) #this line not normally needed, lm can handle NA's, but seems needed because of a weird conflict with MCMCglmm code in Chapter 11 solutions
 ft_logsnow = lm(logFlow~elev+snow, data=snowReduced)
 plotenvelope(ft_logsnow, n.sim=99)
 summary(ft_logsnow)
 confint(ft_logsnow)

## ----ex49 re------------------------------------------------------------------
library(mvabund)
mft_logsnow = manylm(logFlow~elev+snow, data=snowReduced)
summary(mft_logsnow)
confint(mft_logsnow)

## ----ex410--------------------------------------------------------------------
data(aphidsBACI)
lm_aphids = lm(logcount~Plot+Time+Treatment:Time,data=aphidsBACI)
anova(lm_aphids)

## ----ex10 re------------------------------------------------------------------
mlm_aphids = manylm(logcount~Plot+Time+Treatment:Time,data=aphidsBACI)
anova(mlm_aphids)

## ----box96--------------------------------------------------------------------
  data(estuaries)
  ft_estLM = manylm(Total~Mod,data=estuaries)
  anova(ft_estLM,resamp="case",block=estuaries$Estuary)

## ----box97--------------------------------------------------------------------
data(ravens)
crowGun = ravens[ravens$treatment == 1,]
library(reshape2)
crowLong = melt(crowGun,measure.vars = c("Before","After"),
    variable.name="time",value.name="ravens")

library(permute)
CTRL = how(blocks=crowLong$site)
permIDs = shuffleSet(24,nset=999,control=CTRL)
ravenlm = manylm(ravens~site+time,data=crowLong)
anova(ravenlm,bootID=permIDs)

## ----box97 lm-----------------------------------------------------------------
ravenlm = lm(ravens~site+time,data=crowLong)
anova(ravenlm)

## ----box98--------------------------------------------------------------------
data(Myrtaceae)
# fit a lm:
Myrtaceae$logrich=log(Myrtaceae$richness+1)
mft_richAdd = manylm(logrich~soil+poly(TMP_MAX,degree=2)+
                      poly(TMP_MIN,degree=2)+poly(RAIN_ANN,degree=2),
                                         data=Myrtaceae)
                                         
# construct a boot ID matrix: 
BootID = BlockBootID(x = Myrtaceae$X, y = Myrtaceae$Y, block_L = 20,
             nBoot = 99, Grid_space = 5)
anova(mft_richAdd,resamp="case",bootID=BootID)

## ----box98 lm-----------------------------------------------------------------
ft_richAdd = lm(logrich~soil+poly(TMP_MAX,degree=2)+
                      poly(TMP_MIN,degree=2)+poly(RAIN_ANN,degree=2),
                                         data=Myrtaceae)
anova(ft_richAdd)

## ----box99--------------------------------------------------------------------
ft_richAdd = lm(logrich~soil+poly(TMP_MAX,degree=2)+
          poly(TMP_MIN,degree=2)+poly(RAIN_ANN,degree=2),
          data=Myrtaceae)
nBoot=nrow(BootID)
predMat = matrix(NA,length(Myrtaceae$logrich),nBoot)
for(iBoot in 1:nBoot)
{
   ids = BootID[iBoot,]
   ft_i = update(ft_richAdd,data=Myrtaceae[ids,])
   predMat[ids,iBoot] = predict(ft_i)
 }
bootSEs = apply(predMat,1,sd,na.rm=TRUE)
lmSEs   = predict(ft_richAdd,se.fit=TRUE)$se.fit
cbind(bootSEs,lmSEs)[1:10,]

## ----ex98---------------------------------------------------------------------
bootSEall = matrix(NA,length(bootSEs),4)
block_Ls=c(5,10,20,40)
colnames(bootSEall)=block_Ls
bootSEall[,3]=bootSEs #we already did block length 20 in Code Box 9.9
for (iLength in c(1,2,4))
{
  BootIDi = BlockBootID(x = Myrtaceae$X, y = Myrtaceae$Y, block_L = block_Ls[iLength],
             nBoot = 99, Grid_space = 5)
  predMat = matrix(NA,length(Myrtaceae$logrich),nBoot)
  for(iBoot in 1:nBoot)
  {
     ids = BootIDi[iBoot,]
     ft_i = update(ft_richAdd,data=Myrtaceae[ids,])
     predMat[ids,iBoot] = predict(ft_i)
   }
  bootSEall[,iLength] = apply(predMat,1,sd,na.rm=TRUE)
}
head(bootSEall)

## ----ex98 mean----------------------------------------------------------------
apply(bootSEall,2,mean,na.rm=TRUE)

