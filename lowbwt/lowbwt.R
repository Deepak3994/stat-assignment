#To read a xls file 
lowbwt<-read_excel("/home/deepak/Documents/Sois/Statistics/practicals/lowbwt.xls", col_names =  TRUE)

#To show the data in table format
View(lowbwt)

#To calculate mean
numSummary(lowbwt[,"BWT"], groups=lowbwt$SMOKE, statistics=c("mean","sd","IQR","quantiles"), quantiles=c(0.21,0.5,0.75,1))


#To convert the intergers to factors
lowbwt <- within(lowbwt, {
  SMOKE <- factor(SMOKE, labels=c('Non-smoker','Smoker'))
})

lowbwt <- within(lowbwt, {
  RACE <- factor(RACE, labels=c('White','Black','Unknown'))
})

#Boxplot for BWT to check the outliers
Boxplot( ~ BWT, data=lowbwt, id.method="y")

#density plot for smokers and non-smoker in BWT
densityPlot(BWT~SMOKE, data=lowbwt, bw="SJ", adjust=1, kernel="gaussian")

#box plot for smokers and non-smoker in BWT
Boxplot(BWT~SMOKE, data=lowbwt, id.method="y")

#To check the homoginty of variance we use livens test
#for leven test the p value should be more than 0.05 then only it is satified
with(lowbwt, tapply(BWT, SMOKE, var, na.rm=TRUE))
leveneTest(BWT ~ SMOKE, data=lowbwt, center="mean")
#if levens test is significant then welch t-test if not significant then two sample t-test
#welch t-test
t.test(BWT~SMOKE, alternative='two.sided', conf.level=.95, var.equal=FALSE, data=lowbwt)

# sample t-test just change var=TRUE
t.test(BWT~SMOKE, alternative='two.sided', conf.level=.95, var.equal=TRUE, data=lowbwt)

#p(0.009)<0.05 There ias statiscally significant diff in the birth weigth of babies born to smoker and non-smoker mothers

#to check whether race depends on bwt
#we should use one-way anova
#first we should chek whether it is normally distributed, next we should chek the levens test to check homoginity of variance
#whether it is equally distributed
#so we get p value as 0.6316 > 0.056 so we accept the null hypothesis i.e there is no significant difference between the variances

#now we calculate the one-way anova
#we get the p-value as 0.0078 which is less than 0.05, so there is one significant difference
#to check this we select pair-wise in one-way anova test
#the values which we  get is less than 0.05 that is there is significant diff in mean bwt.



