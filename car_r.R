library(readxl)
car_data_final <- read_excel("C:/Users/pddes/Desktop/final dataset.xlsx")
data(car_data_final)

#installing packages
install.packages("broom")
install.packages("xtable")
install.packages("knitr")
install.packages("dplyr")
library("broom")
library("xtable")
library("knitr")
library("dplyr")

#for VIF
install.packages("car")
library("car")

#for RESET test
install.packages("lmtest")
library("lmtest")

#for Jarque Bera test
install.packages("tseries")
library("tseries")

#for graphs
install.packages("ggplot2")
library("ggplot2")

#Making dummy variables as categorical variables in R, and adding square of mileage and engine power
data1= mutate(car_data_final,seller_factor=factor(seller_type_value), transmission_factor=factor(transmission_type_value), fuel_factor=factor(fuel_type_value), luxury_factor=factor(car_status_value), mileage_sq= mileage_kmpl*mileage_kmpl, engine_cc_sq=engine_cc*engine_cc,  log_km_driven=log(km_driven))

#making a regression model using this data
reg1=lm(log_of_selling_price~log_of_avg_price+km_driven+vehicle_age+seller_factor+transmission_factor+fuel_factor+luxury_factor+ mileage_kmpl+ mileage_sq+engine_cc+ engine_cc_sq+seats,data=data1)

#displaying regression results
reg1_stats=summary(reg1)

#Multicollinearity test
multi=vif(reg1)

#Ramsey Test for OVB
(resettest(reg1,c(0,1,2),"fitted",data1))

#Plotting residuals vs true values
plot(data1$log_of_selling_price,reg1$residuals,xlab="log(selling price)",ylab="residuals")

#jarque bera test for normality
(jbtest=jarque.bera.test(reg1$residuals))

#BP Test for checking Heteroskedasticity
bp_test=bptest(reg1)

#box cox transformation to rectify heteroskedasticity
install.packages("caret")
library(caret)
(lambda=BoxCoxTrans(data1$selling_price...1))
y_transformed=((data1$selling_price...1)^(-0.2)-1)/(-0.2)
data1=cbind(data1,y_transformed)
reg_bc=lm(y_transformed~log_of_avg_price+log_km_driven+vehicle_age+seller_factor+transmission_factor+fuel_factor+luxury_factor+ mileage_kmpl+ mileage_sq+engine_cc+ engine_cc_sq+seats,data=data1)
(bptest(reg_bc)) #rechecking BP test results, still heteroskedastic

#Using robust standard errors
# install.packages("robustbase")
# library(robustbase)
# reg1_rob= lmrob(log_of_selling_price~log_of_avg_price+km_driven+vehicle_age+seller_factor+transmission_factor+fuel_factor+luxury_factor+ mileage_kmpl+ mileage_sq+engine_cc+ engine_cc_sq+seats,data=data1)
# summary(reg1_rob)
# (tidy_reg1_rob=tidy(reg1_rob))

#Checking significance using robust standard errors
install.packages("sandwich")
library(sandwich)
coeftest(reg1,vcov=vcovHC(reg1,"HC1"))

#graphic K-density plot of residuals
install.packages("kdensity")
library(kdensity)
kden = density(reg1$residuals, from=-4, to=4)
plot(kden, main="K-Density Plot for Residuals")

#graph matrix
par("mar")
par(mar=c(1,1,1,1))
pairs(~log_of_avg_price+vehicle_age+log_km_driven+mileage_kmpl+mileage_sq+engine_cc+engine_cc_sq+seats,data=data1,lower.panel=panel.smooth)