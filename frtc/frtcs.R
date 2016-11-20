#reading xls file
frtcs <- readXL("/home/deepak/Documents/Sois/Statistics/stat-assignment/frtc/frtcs.xls", rownames=FALSE, 
                +   header=TRUE, na="", sheet="FRTCS", stringsAsFactors=TRUE)
#removing NA values
frtcs <- na.omit(frtcs)

#calculating the difference
frtcs$variable <- with(frtcs, sbp1-sbp2)

#to check mean median and mode
numSummary(frtcs[,"variable"], statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,
                                                                                           +   .25,.5,.75,1))
#to check whether mean is more than 2*SD to check for normality
#it is paired t-test and is not normally distributed so we should use paired-sample wilcoxon test
with(frtcs, wilcox.test(sbp2, sbp1, alternative='two.sided', paired=TRUE))

#here we get p value = 0.01191
#which is less than 0.05 which is significant
#there is significant change in sbp.

#now we should calculate mean sbp at 2 time points and should say whether it is decreased or increased
#for sb1
numSummary(frtcs[,"sbp1"], statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,
                                                                                       +   .5,.75,1))
#for sb2
numSummary(frtcs[,"sbp2"], statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,
                                                                                       +   .5,.75,1))
#now to check the proportion of male and female giving the antihypo drugs whether it is same or diff
#changing the variables of sex
frtcs <- within(frtcs, {sex <- factor(sex, labels=c('Male','Female'))})

#changing the variables of antihyp
frtcs <- within(frtcs, {antihyp0 <- factor(antihyp0, labels=c('No','Yes'))})

#now to check the significant or not of 2 variables we should use 
#contigency table with column percentage(statistics)
local({.Table <- xtabs(~antihyp0+sex, data=frtcs)cat("\nFrequency table:\n")print(.Table).Test <- chisq.test(.Table, correct=FALSE)print(.Test)})

#we get p value as 0.776 which is greater than 0.05 so we dont have enough evidence to reject the null hypothesis
#i.e the antihypo is given equally for both male and female


