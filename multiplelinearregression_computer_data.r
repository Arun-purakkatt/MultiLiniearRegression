computer <- read.csv("D:\\datasets\\Regression\\Computer_Data.csv")
View(computer)
dim(computer)
install.packages("fastDummies", repose = "https://cran.r-project.org/")
library(fastDummies)
new.computer = dummy_cols(computer,remove_first_dummy = T)
View(new.computer)
mydata <- subset(new.computer,select = -c(X,cd,multi,premium))
View(mydata)
attach(mydata)
summary(mydata)
str(mydata)
windows()
plot(mydata)
cor(mydata)
#Linear model m1
m1 <- lm(price~ speed+hd+ram+screen+ads+trend+cd_yes+multi_yes+premium_no)
summary(m1)

#Identifying influential record
install.packages("car", repose = "https://cran.r-project.org/")

library(car)

# plotting influential records
windows()
influencePlot(m1)

#varience inflation factor
vif(m1)

#AV plot
windows()
avPlots(m1)

#exponential transformation
m2 <- lm(log(price)~speed+hd+ram+screen+ads+trend+cd_yes+multi_yes+premium_no)
summary(m2)

#log transformation
m3 <- lm( price ~ log(speed)+log(hd)+ram+screen+ads+trend+cd_yes+multi_yes+premium_no)
summary(m3)

#square root transformation
m4 <- lm(price~ sqrt(speed)+sqrt(hd)+sqrt(ram)+sqrt(screen)+sqrt(ads)+sqrt(trend)+sqrt(cd_yes)+sqrt(multi_yes)+sqrt(premium_no))
summary(m4)

# finalmodel m3
