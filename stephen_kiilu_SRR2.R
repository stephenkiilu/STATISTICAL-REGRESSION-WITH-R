data_purchase_behaviour <- read.csv("~/AIMS/REVIEW PHASE/REGRESSION WITH R/Data/data_purchase_behaviour.csv")


####QUESTION ONE
##Descriptive statistics
data=data_purchase_behaviour###load data
dim(data)## check dimension of my data
## Getting the variables
Gender=data$Gender
city=data$City_Category
Maritals=data$Marital_Status
Age=data$Age_num
stay=data$Stay_In_Current_City_Years
purchase=data$Purchase

summary(data[,-c(1:2,6)])##sumary statistics of my relevant data
table(Maritals) ##table of marital status

##Graphics
par(mfrow=c(2,3))
barplot(table(Age),xlab = "age in years",ylab = "frequency",main="Barplot of age ")
barplot(table(stay),ylab = "count",xlab="no. of years in current city",main="length of stay in current city")
barplot(table(Gender),xlab = "Gender",ylab = "count",main="Barplot of gender")
barplot(table(Maritals),main="Marital status",ylab = "count",names=c("unmarried","married"))
barplot(table(city),main="Barplot city categories",xlab = "city category",ylab = "frequency")
boxplot(purchase,xlab="purchase",ylab="amount",
        main="Boxplot of purchase amount")


###QUESTION 2
##Model building by backward elimination method
correlation=cor(data[,-c(1:4,6)])##correlation matrix
fit1=lm(purchase~city+Gender+Maritals+Age+stay)## All variables
summary(fit1)
fit2=lm(purchase~city+Gender+Age+Maritals)## Eliminate city stay because it most insignficant
summary(fit2)

fit=lm(purchase~city+Gender+Age)## remove marital status, which is insignficant
summary(fit)## We settle with this regression model because all variables are signficant
###QUESTION 3 
## QUESTION 4
##Checking for model assumptions
par(mfrow=c(2,2))
plot(fit1,which=1)## Check for linear association
plot(fit1,which=2)##check for normality
plot(fit1,which=3)##check for homoskedasticity
plot(fit1,which=4)##check for outliers

##QUESTION 5
##QUESTION6
###QUESTION 7 
###comparing my model with a model built on Age and Gender only.
model=lm(purchase~Gender+Age)
summary(model)
anova(model,fit)
## My final model is better than model built on gender and age only,
#the p value of my model is more signficant.





