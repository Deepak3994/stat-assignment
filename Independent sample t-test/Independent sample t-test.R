#making two vectors for the two variables
Males<-c(2701.24,2771.55,3270.77,3504.29,3345.77,3302.59,2914.89,3592.45,3443.87,3116.55,3506.81,2876.04,2834.86,3156.55,3352.85,2473.35,3620.4,3117.79,3304.69,2892.27)
Females<-c(3081.26,2544.79,2675.81,2886.36,2971.95,2494.12,3233.69,3455.01,2864.69,3323.72,3180.56,2998.1,2989.66,2726.11,2699.4,2651.16,2396.76,2811.47,3017.78,3381.92)

#Test for normality
plot(density(c(Males,Females)))

#Homgenity of Variance,levine test,download package lawstat
y<-c(Males,Females)
group<-as.factor(c(rep(1,length(Males)),rep(2,length(Females))))
levene.test(y,group,location="mean")

#if assumptions correct,perform Independent Sample T's Test
t.test(Males,Females)
#If p value less tham 0.05 reject Null Hypothesis
