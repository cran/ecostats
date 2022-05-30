## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fig6.1, fig.width=6, fig.height=4----------------------------------------
library(ecostats)
data(estuaries)
plot(Total~Estuary,data=estuaries,col=c(4,2,2,4,2,4,2))
legend("bottomleft",legend=c("Modified","Pristine"),col=c(4,2),pch=15,pt.cex=2)

## ----box6.1-------------------------------------------------------------------
library(ecostats)
data(estuaries)
library(lme4)
ft_estu = lmer(Total~Mod+(1|Estuary),data=estuaries)
summary(ft_estu)

## ----code62, warning=FALSE----------------------------------------------------
ft_estu = lmer(Total~Mod+(1|Estuary),data=estuaries)
scatter.smooth(residuals(ft_estu)~fitted(ft_estu),
    xlab="Fitted values",ylab="Residuals")
abline(h=0,col="red")
scatter.smooth(residuals(ft_estu)~predict(ft_estu,re.form=NA),
    xlab="Fitted values (no random effects)",ylab="Residuals")
abline(h=0,col="red")

## ----code63-------------------------------------------------------------------
ft_estu = lmer(Total~Mod+(1|Estuary),data=estuaries,REML=F)
ft_estuInt = lmer(Total~(1|Estuary),data=estuaries,REML=F)
anova(ft_estuInt,ft_estu)

## ----code64-------------------------------------------------------------------
confint(ft_estu)

## ----code65-------------------------------------------------------------------
rft=ranef(ft_estu,condVar=T)
library(lattice)
dotplot(rft)

## ----ex62---------------------------------------------------------------------
estuaries$isMod = as.numeric(estuaries$Mod=="Modified")
estuaries$isPri = as.numeric(estuaries$Mod!="Modified")
ft_estuDiff = lmer(Total~Mod+(0+isMod|Estuary)+(0+isPri|Estuary),data=estuaries,REML=F)
summary(ft_estuDiff)
BIC(ft_estu,ft_estuDiff)

## ----ex63---------------------------------------------------------------------
data(aphidsBACI)
str(aphidsBACI)

## ----ex63mod------------------------------------------------------------------
ft_aphids=lmer(logcount~Treatment*Time+(1|Plot),data=aphidsBACI)
ft_aphidNull=lmer(logcount~Time+(1|Plot),data=aphidsBACI)
anova(ft_aphidNull,ft_aphids)

## ----ex63an-------------------------------------------------------------------
lm_aphids=lm(logcount~Plot+Treatment*Time,data=aphidsBACI)
anova(lm_aphids)

## ----ex63summ-----------------------------------------------------------------
summary(lm_aphids)
summary(ft_aphids)

## ----ex64 plot, fig.width=8, fig.height=4-------------------------------------
data(estuaryZone)
cols=c("blue","red","lightblue","pink")
plot(Total~interaction(Estuary,Zone),data=estuaryZone,col=cols[c(1,2,2,1,2,1,2,3,4,4,3,4,3,4)])
legend("bottomright",legend=c("Mod-Inner","Prist-Inner","Mod-Outer","Pris-Outer"),col=cols,pch=15,pt.cex=2)

## ----ex64 lme-----------------------------------------------------------------
library(lme4)
lme_MZ = lmer(Total~Zone*Mod + (Zone|Estuary), data=estuaryZone )

scatter.smooth(residuals(lme_MZ)~fitted(lme_MZ),
    xlab="Fitted values",ylab="Residuals")
abline(h=0,col="red")
scatter.smooth(residuals(lme_MZ)~predict(lme_MZ,re.form=NA),
    xlab="Fitted values (no random effects)",ylab="Residuals")
abline(h=0,col="red")


## ----ex64 anova---------------------------------------------------------------
lme_MplusZ = lmer(Total~Zone+Mod + (Zone|Estuary), data=estuaryZone )
anova(lme_MplusZ,lme_MZ)

## ----ex64 mod-----------------------------------------------------------------
lme_Z = lmer(Total~Zone + (Zone|Estuary), data=estuaryZone )
anova(lme_Z,lme_MplusZ)

## ----box6.6, message=FALSE, eval=FALSE----------------------------------------
#  nBoot=500
#  bStat=rep(NA,nBoot)
#  ft_estu = lmer(Total~Mod+(1|Estuary),data=estuaries)
#  for(iBoot in 1:nBoot)
#  {
#     estuaries$TotalSim=unlist(simulate(ft_estu))
#     ft_i = lmer(TotalSim~Mod+(1|Estuary),data=estuaries)
#     bStat[iBoot] = fixef(ft_i)[2]
#  }
#  sd(bStat) #standard error of Mod effect

## ----summft_estu--------------------------------------------------------------
summary(ft_estu)

## ----box67, message=FALSE-----------------------------------------------------
ft_noestu = lm(Total~Mod,data=estuaries)
library(ecostats)
anovaPB(ft_noestu,ft_estu,n.sim=99)

## ----ex66 interaction, message=FALSE------------------------------------------
lme_MZ = lmer(Total~Zone*Mod + (Zone|Estuary), data=estuaryZone, REML=FALSE )
lme_MplusZ = lmer(Total~Zone+Mod + (Zone|Estuary), data=estuaryZone, REML=FALSE )
anovaPB(lme_MplusZ,lme_MZ,n.sim=99)

## ----ex66 estuary, message=FALSE----------------------------------------------
lme_MZ = lmer(Total~Zone*Mod + (Zone|Estuary), data=estuaryZone, REML=FALSE )
lme_MZnoest = lm(Total~Zone+Mod, data=estuaryZone)
anovaPB(lme_MZnoest,lme_MZ,n.sim=99)

