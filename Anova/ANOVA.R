#defining the vectors
Normal<-c(157.7,99.5,194.7,132.9,184.2,133.8,141,162.5,167.2,147.7,114.8,97.7,162.4,154.7,161.7)
ImpairedGlucoseTolerant<-c(182,160.6,148,129.5,163.9,174.6,145.3,145.4,138.6,133.4,192,141,187.4,135.7,144.8)
TypeIIDiabetes<-c(220.3,184,201.1,173.4,201.8,210.6,195.7,181.8,161.44,169.3,175,181.2,158.5,201.6,174.7)

#Test for normality for each vector
mean(Normal)>2*sd(Normal)

#Peforming Leven's Test
y<-c(Normal,ImpairedGlucoseTolerant,TypeIIDiabetes)
group<-as.factor(c(rep(1,length(Normal)),rep(2,length(ImpairedGlucoseTolerant)),rep(3,length(TypeIIDiabetes))))
leveneTest(y,group,location='mean')

#Performoing ANOVA
combinedgroups<-data.frame(cbind(Normal,ImpairedGlucoseTolerant,TypeIIDiabetes))
stackedgroup<-stack(combinedgroups)
aovresult<-aov(values~ind,data=stackedgroup)
summary(aovresult)

#Tukey Test
TukeyHSD(aovresult)