## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----ravens check-------------------------------------------------------------
 library(ecostats)
 data(ravens)
 qqenvelope(ravens$delta[ravens$treatment==1])

## ----ravens t-----------------------------------------------------------------
 data(ravens)
 crowGun = ravens[ravens$treatment == 1,]
 t.test(crowGun$Before, crowGun$After, paired=TRUE, alternative="less")

## ----ravens lm----------------------------------------------------------------
 library(reshape2)
 crowLong = melt(crowGun,measure.vars = c("Before","After"),
                 variable.name="time",value.name="ravens")
 head(crowLong)
 ravenlm = lm(ravens~site+time,data=crowLong)
 anova(ravenlm)

## ----ravens block-------------------------------------------------------------
crowAfter = ravens[ravens$treatment <=3,]
ft_crowAfter = lm(After~site+treatment,data=crowAfter)
anova(ft_crowAfter)

## ----ravens block assumptions-------------------------------------------------
 plotenvelope(ft_crowAfter)

## ----seaweed plot-------------------------------------------------------------
 data(seaweed)
 seaweed$Dist = factor(seaweed$Dist)
 plot(Total~Wmass, data=seaweed, col=Dist, 
      xlab="Wet Mass [log scale]",ylab="Density (per gram) [log scale]")
 legend("topright",levels(seaweed$Dist),col=1:3,pch=1)

## ----seaweed lm---------------------------------------------------------------
 seaweed$logTot = log(seaweed$Total)
 seaweed$logWmass = log(seaweed$Wmass)
 lmMassDist=lm(logTot~logWmass+Dist,data=seaweed)
  anova(lmMassDist)

## ----seaweed plt--------------------------------------------------------------
plotenvelope(lmMassDist)

## ----seaweed lm switch--------------------------------------------------------
 lmDistMass=lm(logTot~Dist+logWmass,data=seaweed)
 anova(lmDistMass)

## ----seaweed lm drop1---------------------------------------------------------
drop1(lmMassDist,test="F")

## ----seaweed compBox----------------------------------------------------------
plot(Total ~ interaction(Dist,Time), data=seaweed, log="y") ## and as usual use xlabel, ylabel to name axes

## ----seaweed fact-------------------------------------------------------------
 ft_seaweedFact=lm(logTot~Time*Dist, data=seaweed)
 plotenvelope(ft_seaweedFact)

## ----code4.9------------------------------------------------------------------
ft_seaweedFact = lm(logTot~Time*Dist,data=seaweed)
anova(ft_seaweedFact)

## ----seaweed interplot--------------------------------------------------------
 library(dplyr)
 seaweed$Time = as.factor(seaweed$Time) 
 by_DistTime = group_by(seaweed,Dist,Time)
 distTimeMeans = summarise(by_DistTime, logTotal=mean(log(Total)))
 distTimeMeans
 library(ggplot2)
 library(ggthemes) #loads special themes
 ggplot(seaweed, aes(x = factor(Dist), y = Total, colour = Time)) +
   geom_point() + geom_line(data = distTimeMeans, aes(y = exp(logTotal),
   group = Time)) + theme_few() + xlab("Distance of Isolation") +
   ylab("Total abundance [log scale]") + scale_y_log10(breaks=c(2,5,10,20))

## ----seaweed interplot2-------------------------------------------------------
interaction.plot(seaweed$Dist, seaweed$Time, ft_seaweedFact$fitted,
      xlab="Isolation of patch", ylab="Total density [log]", trace.label="Time")

## ----seaweed no factor--------------------------------------------------------
data(seaweed)
ft_nofactor=lm(log(Total)~Time*Dist,data=seaweed)
anova(ft_nofactor)

## ----seaweed multcomp---------------------------------------------------------
 seaweed$Dist = factor(seaweed$Dist)
 seaweed$Time = factor(seaweed$Time)
 seaweed$logTot = log(seaweed$Total)
 ft_seaweedFact = lm(logTot~Time*Dist, data=seaweed)
 library(multcomp)
 contFact = mcp(Dist="Tukey") # telling R to compare on the Dist factor
 compFact = glht(ft_seaweedFact, linfct=contFact) # run multiple comparisions

## ----seaweed multcomp Main----------------------------------------------------
 ft_seaweedMain=lm(logTot~Time+Dist,data=seaweed) # note it is + not *
 contrast = mcp(Dist="Tukey") # telling R to compare on the Dist factor
 compDistMain = glht(ft_seaweedMain, linfct=contrast)
 confint(compDistMain)

## ----seaweed multcomp Int-----------------------------------------------------
 td = interaction(seaweed$Dist,seaweed$Time)
 ft_seaweedInt=lm(logTot~td,data=seaweed) # Time*Dist as a single term
 contInt = mcp(td="Tukey") # telling R to compare on all Time*Dist levels
 compDistInt = glht(ft_seaweedInt, linfct=contInt)
 summary(compDistInt)

## ----seaweed multcomp inTime--------------------------------------------------
 levels(seaweed$Time) = c("five","ten") #need non-numbers for mcp to work
 levels(seaweed$Dist) = c("Zero","Two","Ten")
 td = interaction(seaweed$Dist,seaweed$Time)
 ft_seaweedInt=lm(log(Total)~td,data=seaweed) # Time*Dist as a single term
 contDistinTime = mcp(td = c("Two.five - Zero.five = 0",
                             "Ten.five - Zero.five = 0",
                             "Ten.five - Two.five = 0",
                             "Two.ten - Zero.ten = 0",
                             "Ten.ten - Zero.ten = 0",
                             "Ten.ten - Two.ten = 0"))
 compDistinTime = glht(ft_seaweedInt, linfct=contDistinTime)
 summary(compDistinTime)

## ----seaweed ANCOVA Int-------------------------------------------------------
 lmMassDistInter=lm(logTot~log(Wmass)*Dist,data=seaweed)
 anova(lmMassDistInter)

## ----height quad--------------------------------------------------------------
 ft_latRain2 = lm(log(height)~poly(rain,lat,degree=2),data=globalPlants)
 summary(ft_latRain2)

## ----snowmelt-----------------------------------------------------------------
 data(snowmelt)
 plot(flow~snow,data=snowmelt)
 snowReduced = na.omit(snowmelt[,c("flow","snow","elev")]) #this line not normally needed, lm can handle NA's, but seems needed because of a weird conflict with MCMCglmm
 ft_snow = lm(flow~elev+snow, data=snowReduced)
 plotenvelope(ft_snow)

## ----snowmelt log-------------------------------------------------------------
 plot(flow~snow,data=snowmelt,log="y")
 snowReduced$logFlow = log(snowReduced$flow)
 snowReduced2 = snowReduced[snowReduced$logFlow>-Inf,] # look it's a bit naughty, removing the infinite value, but no biggie as only one value
 ft_logsnow = lm(logFlow~elev+snow, data=snowReduced2)
 plotenvelope(ft_logsnow)
 summary(ft_logsnow)
 confint(ft_logsnow)

## ----aphid netting------------------------------------------------------------
data(aphidsBACI)
str(aphidsBACI)
plot(logcount~interaction(Time,Treatment),data=aphidsBACI)

## ----aphid lm-----------------------------------------------------------------
lm_aphids = lm(logcount~Plot+Time+Treatment:Time,data=aphidsBACI)
plotenvelope(lm_aphids,which=1:3)

## ----aphid anova--------------------------------------------------------------
anova(lm_aphids)

## ----seaweed 3----------------------------------------------------------------
data(seaweed)
str(seaweed)
seaweed$logTot = log(seaweed$Total)
seaweed$Dist = factor(seaweed$Dist)
par(mar=c(4,7,1,1))
plot(logTot~interaction(Dist,Size,Time),data=seaweed,xlab="",ylab="Total abundance [log scale]",horizontal=TRUE,col=2:4,las=1)
legend("topleft",legend=paste0("Dist=",levels(seaweed$Dist)),pch=15,col=2:4,pt.cex=2)

## ----seaweed anova3-----------------------------------------------------------
ft_seaweed3 = lm(log(Total)~Size*Time*Dist,data=seaweed)
plotenvelope(ft_seaweed3,which=1:3)
anova(ft_seaweed3)

## ----seaweed ancova3----------------------------------------------------------
ft_seaweedW3 = lm(logTot~Wmass*Size*Time*Dist,data=seaweed)
anova(ft_seaweedW3)

