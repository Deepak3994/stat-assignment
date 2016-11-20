
library(mvtnorm, pos=18)
library(survival, pos=18)
library(MASS, pos=18)
library(TH.data, pos=18)
library(multcomp, pos=18)
AnovaModel.1 <- aov(BWT ~ RACE, data=lowbwt)
summary(AnovaModel.1)
with(lowbwt, numSummary(BWT, groups=RACE, statistics=c("mean", "sd")))
with(lowbwt, tapply(BWT, RACE, var, na.rm=TRUE))
leveneTest(BWT ~ RACE, data=lowbwt, center="median")
AnovaModel.2 <- aov(BWT ~ RACE, data=lowbwt)
summary(AnovaModel.2)
with(lowbwt, numSummary(BWT, groups=RACE, statistics=c("mean", "sd")))
AnovaModel.3 <- aov(BWT ~ RACE, data=lowbwt)
summary(AnovaModel.3)
with(lowbwt, numSummary(BWT, groups=RACE, statistics=c("mean", "sd")))
local({
  .Pairs <- glht(AnovaModel.3, linfct = mcp(RACE = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs)) # confidence intervals
  print(cld(.Pairs)) # compact letter display
  old.oma <- par(oma=c(0,5,0,0))
  plot(confint(.Pairs))
  par(old.oma)
})
oneway.test(BWT ~ RACE, data=lowbwt) # Welch test
AnovaModel.4 <- aov(BWT ~ RACE, data=lowbwt)
summary(AnovaModel.4)
with(lowbwt, numSummary(BWT, groups=RACE, statistics=c("mean", "sd")))
AnovaModel.5 <- aov(BWT ~ RACE, data=lowbwt)
summary(AnovaModel.5)
with(lowbwt, numSummary(BWT, groups=RACE, statistics=c("mean", "sd")))
local({
  .Pairs <- glht(AnovaModel.5, linfct = mcp(RACE = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs)) # confidence intervals
  print(cld(.Pairs)) # compact letter display
  old.oma <- par(oma=c(0,5,0,0))
  plot(confint(.Pairs))
  par(old.oma)
})

