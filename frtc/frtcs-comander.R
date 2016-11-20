
frtcs <- readXL("/home/deepak/Documents/Sois/Statistics/stat-assignment/frtc/frtcs.xls", rownames=FALSE, 
  header=TRUE, na="", sheet="FRTCS", stringsAsFactors=TRUE)
frtcs <- na.omit(frtcs)
frtcs$variable <- with(frtcs, sbp1 sbp2)
frtcs$variable <- with(frtcs, sbp1-sbp2)
densityPlot( ~ variable, data=frtcs, bw="SJ", adjust=1, kernel="gaussian")
densityPlot( ~ variable, data=frtcs, bw="SJ", adjust=1, kernel="gaussian")
Boxplot( ~ variable, data=frtcs, id.method="y")
library(abind, pos=16)
library(e1071, pos=17)
numSummary(frtcs[,"variable"], statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,
  .25,.5,.75,1))
with(frtcs, median(sbp2 - sbp1, na.rm=TRUE)) # median difference
with(frtcs, wilcox.test(sbp2, sbp1, alternative='two.sided', paired=TRUE))
numSummary(frtcs[,"sbp1"], statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,
  .5,.75,1))
numSummary(frtcs[,"sbp2"], statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,
  .5,.75,1))
frtcs <- within(frtcs, {
  sex <- factor(sex, labels=c('Male','Female'))
})
frtcs <- within(frtcs, {
  antihyp0 <- factor(antihyp0, labels=c('No','Yes'))
})
local({
  .Table <- xtabs(~antihyp0+sex, data=frtcs)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~antihyp0+sex, data=frtcs)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nColumn percentages:\n")
  print(colPercents(.Table))
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~antihyp0+sex, data=frtcs)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nRow percentages:\n")
  print(rowPercents(.Table))
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~antihyp0+sex, data=frtcs)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nColumn percentages:\n")
  print(colPercents(.Table))
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})

