library(MASS)
install.packages("ISLR")
library(ISLR)
install.packages("tidyverse")
library(tidyverse)
#a) Import data
is.na(College)

# b) Here we used the head() to know the variables before modification and we removed one variable $X and by using the fix() we fix the college data.
college=College
rownames(college) <- college$x
college$x<-NULL
fix(college)

#c 
#i) summary() it is used to produce a numerical summary of variables in the college dataset
summary(college)
#ii) pairs() function to produce a scatterplot matrix of the first ten columns or variables of the college dataset.
pairs(college[,1:10])
plot(Outstate ~ Private, data = college, col = c("green", "blue"))
#iii) plot() function to produce side-by-side boxplots of Outstate versus Private.
plot(Outstate ~ Elite, data = college, col = c("green", "Red"))
#iv) Creating a quantitative variable Elite, by starting the Top10perc and boxplot of outstate versus Elite.%
Elite = rep("No", nrow(college))
Elite[College$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college,Elite)
summary(college)
#v) hist() function to produce histogram with different variables
hist(college$Expend, breaks = 100, xlab = "Instructional Expenditure for student", main = "")
par(mfrow=c(2,2))
hist(college$Apps, xlab = "Application Recieved", main = "")
hist(college$perc.alumni, col = 2, xlab = "% of Alumni who donate", main = "")
hist(college$S.F.Ratio, col = 3,breaks = 10, xlab = "Students/faculty ration", main = "")
hist(college$Expend, breaks = 100,xlab = "Instructional Expenditure for Student", main = "")
#vi) Continue exploring data
summary(college$perc.alumni)
alumnii<-college[college$perc.alumni<40,]
nrow(alumnii)

#ISLR Applied Problem 9
# a) We are loading the Auto dataset and finding which are qualitative and quantitative.
str(Auto)


#b) We are retrieving the range of each quantitative predictors by using range() function.
summary(Auto[,-c(4,9)])


#c)Mean and standard deviation of each quantitative predictor
sapply(Auto[,-c(4,9)],mean)
sapply(Auto[,-c(4,9)],sd)


#d) Now removing the 10th through 85th observations and finding range, mean and standard deviation for each predictor in subset.
subset<-Auto[-c(10:85),-c(4,9)]
sapply(subset,range)
sapply(subset,mean)
sapply(subset,sd)


#e)
Auto$cylinders<-as.factor(Auto$cylinders)
Auto$year<-as.factor(Auto$year)
Auto$origin<-as.factor(Auto$origin)
pairs(Auto)


#f)
cor(Auto$weight, Auto$horsepower)
cor(Auto$horsepower,Auto$displacement)

#ISLR 2.4 Applied Problem 10
#a) Loading Boston datasetx``
library(MASS)
Boston$chas<-as.factor(Boston$chas)
nrow(Boston)
ncol(Boston)

#b)
pairs(Boston)


#c) Any of the predictors associated with per capita crime rate?
hist(Boston$crim, breaks = 75)


#d)Any of the suburbs of Boston appear? Taxes? Rates? Ratio?
hist(Boston$crim, breaks = 25)
nrow(Boston[Boston$crim>30,])
hist(Boston$tax, breaks = 75)
nrow(Boston[Boston$tax ==666,])
hist(Boston$crim, breaks=75)
nrow(Boston[Boston$ptratio>20,])


#e) How many of the suburbs in the set on Charles river?
nrow(Boston[Boston$chas==1,])


#f) What is the medium pupil-teacher among the towns?
median(Boston$ptratio)


#g) Which suburbs of Boston has lowest median value of owner
row.names(Boston[min(Boston$medv),])
range(Boston$tax)
Boston[min(Boston$medv),]$tax


#h) In this data set, how many of the suburbs average more than seven rooms per dwelling?
nrow(Boston[Boston$rm>7,])
nrow(Boston[Boston$rm>8,])

#) ISLR 3.7 Applied Problem 8.

#a) i) Is there a relationship the predictor and the response

library(ISLR)
data(Auto)
fit=lm(mpg ~ horsepower, data = Auto)
summary(fit)


#ii) 
To calculate the leftover blunder relative to the reaction we utilize the cruel of the reaction and the RSE. 
The cruel of mpg is 23.4459184. The RSE of the lm.fit was 4.9057569 which demonstrates a rate mistake of 20.9237141%.
We may moreover note that as the R2 is rise to 0.6059483, nearly 60.5948258% of the inconstancy in mpg can be clarified utilizing horsepower.

#iii)
As the coefficient of "horsepower" is negative, the relationship is additionaly negative.
The more drive a vehicle has the direct relapse shows the less mpg fuel proficiency the vehicle will have. 

#iv)

predict(fit,data.frame(horsepower=98), interval = "confidence")
predict(fit, data.frame(horsepower=98), interval = "prediction")

#b)
plot(Auto$horsepower,Auto$mpg,main = "scatterplot of mpg vs. horsepower", ylab = "mog", col = "blue")

#c)
plot(predict(fit), residuals(fit))

plot(predict(fit), rstudent(fit))
par(mfrow=c(2,2))

plot(fit)


#ISLR 3.7 Applied Problem 9. 
#a)
pairs(Auto)

#b)
names(Auto)
cor(Auto[1:8])

#c)i)
fit2<-lm(mpg ~. -name,data=Auto)
summary(fit2)

#ii) 
Able to reply this address by checking the p-values related with each predictors t-statistic.
We may conclude that all indicators are measurably noteworthy but "cylinders", "horsepower" and "acceleration".

#iii) 
The coefficient to the "year" variable recommends that the normal impact of an increment of 1 year
is an increment of 0.7507727 in "mpg" (all other indicators remaining steady). 
In other words, cars gotten to be more fuel effective each year by nearly 1 mpg/year.

#d)
par(mfrow=c(2,2))
plot(fit)

#e)
par(mfrow=c(2,2))
plot(fit)
fit3<-lm(mpg ~cylinders * displacement+displacement * weight, data = Auto[,1:8])
summary(fit3)

#f)
par(mfrow=c(2,2))
plot(log(Auto$horsepower),Auto$mpg)
plot(sqrt(Auto$horsepower),Auto$mpg)
plot((Auto$horsepower)^2,Auto$mpg)

#) ISLR 3.7 Applied Problem 10.
#a)
fit4<-lm(Sales ~ Price + US,data = Carseats)
summary(fit4)

#b) 
The coefficient of the "Price" variable may be translated by saying that the normal 
impact of a cost increment of 1 dollar may be diminish of 54.4588492 units 
in deals all other indicators remaining settled.The coefficient of the "Urban" 
variable may be translated by saying that on normal the unit deals in urban area are 21.9161508
units less than in rustic area all other indicators remaining settled.
The coefficient of the "US" variable may be deciphered by saying that on normal the unit deals in
a US store are 1200.5726978 units more than in a non US store all other indicators remaining settled.

#c)
sales= 13.04+(-0.054) * Price+(-0.021)*Urban+(1.-20) *US+E

#d)
Ready ot dismiss the invalid speculation for the "Proce" and "US" factors. 

#e)
fit4<-lm(Sales ~ Price + US,data = Carseats)
summary(fit4)

#f) 
The R2 for the litter show is imperceptibly way better than for 
the greater demonstrate. Basically around 23.9262888% of the changeability
is clarified by the demonstrate.

#g)
confint(fit4)

#h)
par(mfrow= c(2,2))
plot(fit4)
