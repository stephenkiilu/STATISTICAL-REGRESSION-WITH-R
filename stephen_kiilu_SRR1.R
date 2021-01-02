##loading data
library(ggplot2)
death_rates <- read.csv("~/AIMS/REVIEW PHASE/REGRESSION WITH R/Assignment/Stats Regr - Homework1/death_rates.csv")
attach(death_rates)
death_rates
##Slicing the data

data=death_rates
attach(data)
male=data[1:31,]
female=data[32:62,]
 
##Question ONE
##Summary of age in the dataset 
summary(Age)# summary statistics
n=length(Age)# sample size
n
mean_age=mean(Age)
var_age=sum((Age-mean_age)^2)/(n-1)#variance of the Age
var_age
sd_age=var_age^0.5#standard deviation of Age
sd_age

mean_death=mean( DeathRate)#mean DeathRate per 10,000
mean_death
summary(DeathRate)#summary of Death Rates
var_death=sum((DeathRate-mean_death)^2)/(n-1)#variance of DeathRates per 10,000
var_death
sd_age=var_age^0.5#standard deviation of Age
sd_death=var_death^0.5#standard deviation of DeathRates per 10,000
sd_death
cor(data$DeathRate,data$Age)

##Graphics
par(mfrow=c(1,2))## Boxplot of Age and DeathRate side by side
boxplot(Age,main="boxplot of Age ",xlab="Age",ylab="years") 
boxplot(DeathRate,main="boxplot of DeathRate ",xlab="DeathRate",ylab="Number of deaths in 10,000")


plot(Age,DeathRate)##scatter diagram of Age and DeathRate


par(mfrow=c(2,1))## Histogram of Age and DeathRate in 10,000 side by side
hist(Age)
hist(DeathRate)


boxplot(Age,DeathRate,names=c("Age","DeathRate"),main="Boxplot of Age and DeathRate")

##QUESTION 2
#Side by side scatter diagrams of DeathRate aginst Age in females and males.
par(mfrow=c(2,1))
plot(female$Age,female$DeathRate,main = "scatter plot of DeathRate against Age in females",xlab="Age",ylab = "DeathRate")
plot(male$Age,male$DeathRate,main = "scatter plot of DeathRate against Age in males",xlab="Age",ylab = "DeathRate")
#male
mean_male_age=mean(male$Age)
var_male_age=sum((male$Age-mean_male_age)^2)/(31-1)
mean_male_DeathRate=mean(male$DeathRate)
var_male_DeathRate=sum((male$DeathRate-mean_male_DeathRate)^2)/(31-1)
cov_age_death=sum((male$Age-mean_male_age)*(male$DeathRate-mean_male_DeathRate))/(31-1)
cor_male=cov_age_death/(var_male_age*var_male_DeathRate)^0.5
cor_male##correlation between Age and DeathRate in males



#female
mean_female_age=mean(female$Age)
var_female_age=sum((female$Age-mean_female_age)^2)/(31-1)
mean_female_DeathRate=mean(female$DeathRate)
var_female_DeathRate=sum((female$DeathRate-mean_female_DeathRate)^2)/(31-1)
cov_age_death=sum((female$Age-mean_female_age)*(female$DeathRate-mean_female_DeathRate))/(31-1)
cor_female=cov_age_death/(var_female_age*var_female_DeathRate)^0.5
corr=cor(female$Age,female$DeathRate)
corr##correlation between Age and DeathRate in female

##QUESTION 3
beta=cov_age_death/var_female_age##Gradient of regression line
alpha=mean_female_DeathRate-(beta*mean_female_age)##Intercept of regression line
#DeathRate_female=0.7089*female_age-15.4879 rregression line equation
ggplot(female,aes(female$Age,female$DeathRate))+geom_point(color="tomato3")+geom_abline(method=lm)+geom_smooth(method = lm,color="tomato3")+
ggtitle("Regression line of Age against DeatRate in females")+ylab("DeathRate")+xlab("Age")


##QUESTION 4
y=0.7089*(51)-15.4879
y##Predicted DeathRates of females aged 51

##QUESTION 5
##Indicators of quality of the model
##Coefficient of Determination R-Squared
y_est=0.7089*female$Age-15.4879 ## y estimated
SSE=sum((female$DeathRate-y_est)^2)
SST=sum((female$DeathRate-mean_female_DeathRate)^2)
R_squared=1-SSE/SST
R_squared##Coefficient of Determination, R-Squared


#Mean square error,MSE
MSE=sum((female$DeathRate-y_est)^2)/29
MSE


plot(death_rates$Age,death_rates$DeathRate)
f=lm(death_rates$DeathRate~death_rates$Age)
abline(reg=f)
