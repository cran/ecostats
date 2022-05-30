## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----box71, fig.width=8,fig.height=7------------------------------------------
library(ecostats)
data(aphids)
cols=c(rgb(1,0,0,alpha=0.5),rgb(0,0,1,alpha=0.5)) #transparent colours
par(mfrow=c(2,1),mar=c(3,3,1.5,1),mgp=c(2,0.5,0),oma=c(0,0,0.5,0))
with(aphids$oat, interaction.plot(Time,Plot,logcount,legend=FALSE,
  col=cols[Treatment], lty=1, ylab="Counts [log(y+1) scale]",
  xlab="Time (days since treatment)") )
legend("bottomleft",c("Excluded","Present"),col=cols,lty=1)
mtext("(a)",3,adj=0,line=0.5,cex=1.4)
with(aphids$oat, interaction.plot(Time,Treatment,logcount, col=cols,
  lty=1, legend=FALSE, ylab="Counts [log(y+1) scale]",
  xlab="Time (days since treatment)"))
legend("topright",c("Excluded","Present"),col=cols,lty=1)
mtext("(b)",3,adj=0,line=0.5,cex=1.4)

## ----box72--------------------------------------------------------------------
library(lme4)
aphid_int = lmer(logcount~Treatment*Time+Treatment*I(Time^2)+(1|Plot),
    data=aphids$oat,REML=FALSE) # random intercepts model
aphid_slope = lmer(logcount~Treatment*Time+Treatment*I(Time^2)+(Time|Plot),
    data=aphids$oat, REML=FALSE) # random slopes model
library(nlme) # refit random intercepts model in nlme to get a ACF:
aphid_int2 = lme(logcount~Treatment*Time+Treatment*I(Time^2),
    random=~1|Plot, data=aphids$oat, method="ML")
plot(ACF(aphid_int2),alpha=0.05) # only works for nlme-fitted mixed models
# now try a model with a temporally structured random effect:
aphid_CAR1 = update(aphid_int2,correlation=corCAR1(,form=~Time|Plot))
BIC(aphid_int,aphid_int2,aphid_slope,aphid_CAR1)

## ----box73--------------------------------------------------------------------
print(aphid_int)
anova(aphid_int)
aphid_noTreat = lmer(logcount~Time+I(Time^2)+(1|Plot),
   data=aphids$oat, REML=FALSE)
anova(aphid_noTreat,aphid_int)

## ----box74--------------------------------------------------------------------
print(aphid_slope)
anova(aphid_slope)
aphid_noTreatS = lmer(logcount~Time+I(Time^2)+(Time|Plot),
    data=aphids$oat, REML=FALSE)
anova(aphid_noTreatS,aphid_slope)

## ----ex72plot, fig.width=8,fig.height=4---------------------------------------
data(aphids)
cols=c(rgb(1,0,0,alpha=0.5),rgb(0,0,1,alpha=0.5)) #transparent colours
par(mar=c(3,3,1.5,1),mgp=c(2,0.5,0),oma=c(0,0,0.5,0))
with(aphids$wheat, interaction.plot(Time,Plot,logcount,legend=FALSE,
  col=cols[Treatment], lty=1, ylab="Counts [log(y+1) scale]",
  xlab="Time (days since treatment)") )
legend("bottomleft",c("Excluded","Present"),col=cols,lty=1)

## ----ex72 long----------------------------------------------------------------
library(lme4)
aphidw_int = lmer(logcount~Treatment*Time+Treatment*I(Time^2)+(1|Plot),
    data=aphids$wheat,REML=FALSE) # random intercepts model
aphidw_slope = lmer(logcount~Treatment*Time+Treatment*I(Time^2)+(Time|Plot),
    data=aphids$wheat, REML=FALSE) # random slopes model
library(nlme) # refit random intercepts model in nlme to get a ACF:
aphidw_int2 = lme(logcount~Treatment*Time+Treatment*I(Time^2),
    random=~1|Plot, data=aphids$wheat, method="ML")
plot(ACF(aphidw_int2),alpha=0.05) # only works for nlme-fitted mixed models
# now try a model with a temporally structured random effect:
# for some reason this one returns non-convergence unless I make Tiem a (numerical) factor:
aphidsTimenFac=glmmTMB::numFactor(aphids$wheat$Time) 
aphidw_CAR1 = update(aphidw_int2,correlation=corCAR1(,form=~aphidsTimenFac|Plot))

BIC(aphidw_int,aphidw_int2,aphidw_slope,aphidw_CAR1)

## ----ex72 birds---------------------------------------------------------------
aphidw_noTr = lmer(logcount~Time+I(Time^2)+(1|Plot),
    data=aphids$wheat,REML=FALSE) # random intercepts model
anova(aphidw_noTr,aphidw_int)

## ----ex73 prep----------------------------------------------------------------
aphids$oat$field   = "oat"
aphids$wheat$field = "wheat"
aphids$wheat$Plot=paste0("W",aphids$wheat$Plot) #making sure we have different names for different Plots across fields
aphids$all         = rbind(aphids$oat,aphids$wheat)
aphids$all$field   = factor(aphids$all$field)
str(aphids$all)

## ----ex73 anova---------------------------------------------------------------
aphida_int = lmer(logcount~field*Time*Treatment+field*I(Time^2)*Treatment+(1|Plot),
    data=aphids$all,REML=FALSE) # random intercepts model
aphida_noTr = lmer(logcount~field*Time+field*I(Time^2)+(1|Plot),
    data=aphids$all,REML=FALSE) # random intercepts model
anova(aphida_noTr,aphida_int)

## ----box75--------------------------------------------------------------------
data(Myrtaceae)
Myrtaceae$logrich=log(Myrtaceae$richness+1)
ft_rich = lm(logrich~soil+poly(TMP_MAX,TMP_MIN,RAIN_ANN,degree=2),
     data=Myrtaceae)
ft_richAdd = lm(logrich~soil+poly(TMP_MAX,degree=2)+
     poly(TMP_MIN,degree=2)+poly(RAIN_ANN,degree=2), data=Myrtaceae)
BIC(ft_rich,ft_richAdd)

## ----box75 spatial, eval = FALSE----------------------------------------------
#  library(nlme)
#  richForm = logrich~soil+poly(TMP_MAX,degree=2)+poly(TMP_MIN,degree=2)+
#         poly(RAIN_ANN,degree=2)
#  ft_richExp = gls(richForm,data=Myrtaceae,correlation=corExp(form=~X+Y))
#  ft_richNugg = gls(richForm,data=Myrtaceae,
#           correlation=corExp(form=~X+Y,nugget=TRUE))
#  BIC(ft_richExp,ft_richNugg)

## ----box76, eval=FALSE--------------------------------------------------------
#  ft_richNugg
#  anova(ft_richAdd)
#  anova(ft_richNugg)

## ----box77--------------------------------------------------------------------
library(pgirmess)
corRich = with(Myrtaceae,correlog(cbind(X,Y),logrich))
plot(corRich,xlim=c(0,150),ylim=c(-0.05,0.2))
abline(h=0,col="grey90")

Myrtaceae$resid = residuals(ft_richAdd)
corRichResid = with(Myrtaceae,correlog(cbind(X,Y),resid))
plot(corRichResid,xlim=c(0,150),ylim=c(-0.05,0.2))
abline(h=0,col="grey90")

## ----box 78,fig.width=8,fig.height=9------------------------------------------
library(caper)
data(shorebird)
shore4d=phylobase::phylo4d(shorebird.tree,shorebird.data)
library(phylosignal)
barplot.phylo4d(shore4d,c("Egg.Mass","F.Mass","M.Mass"))

## ----box79, fig.width=7, fig.height=7-----------------------------------------
library(GGally)
ggpairs(log(shorebird.data[,2:4]))

## ----box710-------------------------------------------------------------------
library(caper)
shorebird = comparative.data(shorebird.tree, shorebird.data,
                   Species, vcv=TRUE)
pgls_egg = pgls(log(Egg.Mass) ~ log(F.Mass)+log(M.Mass),
                data=shorebird)
summary(pgls_egg)

## ----box711,fig.width=8,fig.height=8,eval=FALSE-------------------------------
#  par(mfrow=c(2,2))
#  plot(pgls_egg)
#  res.df = data.frame(Species = shorebird.data$Species,
#                  res = residuals(pgls_egg))
#  res4d = phylobase::phylo4d(shorebird.tree,res.df)
#  res.pg = phyloCorrelogram(res4d,trait="res")
#  plot(res.pg)

## ----ex76 lambda--------------------------------------------------------------
pgls_eggL = pgls(log(Egg.Mass) ~ log(F.Mass)+log(M.Mass), lambda="ML",
                data=shorebird)
summary(pgls_eggL)

## ----ex76 lm------------------------------------------------------------------
lm_egg = lm(log(Egg.Mass) ~ log(F.Mass)+log(M.Mass), data=shorebird.data)
summary(lm_egg)

## ----ex76 ll------------------------------------------------------------------
logLik(lm_egg)
logLik(pgls_eggL)

