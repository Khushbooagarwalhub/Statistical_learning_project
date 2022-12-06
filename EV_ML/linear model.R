#SAL project linear modeling

library(data.table)
library(jsonlite)
library(stringr)
library(dplyr)
library(tidyr)
library(maditr)
library(xml2)
library(rvest)
library(tidyverse)
library(MASS)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(readr)
library(pscl)
library(caret)
library(leaps)
library(boot)
library(glmnet)

df <- fread("~/Desktop/SAL_project/df_county_modified.csv")

df = subset(df, select = -c(V1) ) #drops one column
df = subset(df, select = -c(V1) ) #drops one column
df = subset(df, select = -c(Num_EV) ) #drops one column

df$income_per_capita = gsub("\\$", "", df$income_per_capita)

df$income_per_capita = gsub(",", "", df$income_per_capita)
df$On.Road.TransportationGasolineconsumption..gallons.capita. = gsub(",", "", df$On.Road.TransportationGasolineconsumption..gallons.capita.)
df$On.Road.TransportationDieselconsumption..gallons.capita. = gsub(",", "", df$On.Road.TransportationDieselconsumption..gallons.capita.)
df$On.Road.Transportationvehicle.miles.traveled..miles.capita. = gsub(",", "", df$On.Road.Transportationvehicle.miles.traveled..miles.capita.)
df$On.road.TransportationGasolineGHG.emissions.mtons.CO2e = gsub(",", "", df$On.road.TransportationGasolineGHG.emissions.mtons.CO2e)
df$On.road.TransportationDieselGHG.emissions.mtons.CO2e = gsub(",", "", df$On.road.TransportationDieselGHG.emissions.mtons.CO2e)

df$income_per_capita <- as.numeric(df$income_per_capita) 
df$On.Road.TransportationGasolineconsumption..gallons.capita. <- as.numeric(df$On.Road.TransportationGasolineconsumption..gallons.capita.) 
df$On.Road.TransportationDieselconsumption..gallons.capita. <- as.numeric(df$On.Road.TransportationDieselconsumption..gallons.capita.) 
df$On.Road.Transportationvehicle.miles.traveled..miles.capita. <- as.numeric(df$On.Road.Transportationvehicle.miles.traveled..miles.capita.) 
df$On.road.TransportationGasolineGHG.emissions.mtons.CO2e <- as.numeric(df$On.road.TransportationGasolineGHG.emissions.mtons.CO2e) 
df$On.road.TransportationDieselGHG.emissions.mtons.CO2e <- as.numeric(df$On.road.TransportationDieselGHG.emissions.mtons.CO2e) 

#Scatter plot
df_scatter = subset(df, select = -c(County) )

#re-defining the panel.smooth function
smooth.panel= function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                        cex = 1, col.smooth = 2, span = 2/3, iter = 3, ...) 
{
  points(x, y, pch = 18, col ="mediumseagreen", bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
          col = col.smooth, ...)
  r <- round(cor(x,y), digits=2)
  txt<- paste0("R= ",r)
  usr<- par("usr"); on.exit(par(usr))
  par(usr = c(0,1,0,1))
  text(0.5,0.9,txt)
}
pairs(df_scatter, upper.panel=smooth.panel, lower.panel = smooth.panel, main = "Scatterplot Matrix")

#CORRELATION
# correlation for all variables
round(cor(df_scatter),digits = 2)

#predictors with highest correlation values are included for the linear regression
reg_all = lm(vehicles_registered ~ income_per_capita + Num_stations + Num_ports + population + employment + ResidentialElectricityconsumption..MWh.capita. + On.Road.TransportationGasolineconsumption..gallons.capita. + On.Road.TransportationDieselconsumption..gallons.capita. + On.Road.Transportationvehicle.miles.traveled..miles.capita. + On.road.TransportationGasolineGHG.emissions.mtons.CO2e + On.road.TransportationDieselGHG.emissions.mtons.CO2e, data=df_scatter)

summary(reg_all)

#looking at each parameter separately

reg1 = lm(vehicles_registered~income_per_capita, data=df_scatter)
summary(reg1)

reg2 = lm(vehicles_registered~Num_stations, data=df_scatter)
summary(reg2)

reg3 = lm(vehicles_registered~population, data=df_scatter)
summary(reg3)

reg4 = lm(vehicles_registered~employment, data=df_scatter)
summary(reg4)

reg5 = lm(vehicles_registered~On.road.TransportationGasolineGHG.emissions.mtons.CO2e, data=df_scatter)
summary(reg5)

reg6 = lm(vehicles_registered~ On.road.TransportationDieselGHG.emissions.mtons.CO2e, data=df_scatter)
summary(reg6)

reg_only = lm(vehicles_registered~ income_per_capita+Num_stations+population+employment+On.road.TransportationGasolineGHG.emissions.mtons.CO2e+On.road.TransportationDieselGHG.emissions.mtons.CO2e, data=df_scatter)
summary(reg_only)

ggplot(data=df_scatter, aes(x=income_per_capita,y=vehicles_registered)) +
  geom_smooth(method="lm") +
  geom_point() +
  stat_cor(label.x = 0.01, label.y = 8000)+
  stat_regline_equation(label.x = 0.01, label.y = 10000)

ggplot(data=df_scatter, aes(x=Num_stations,y=vehicles_registered)) +
  geom_smooth(method="lm") +
  geom_point() +
  stat_cor(label.x = 0.01, label.y = 8000)+
  stat_regline_equation(label.x = 0.01, label.y = 10000) +
  xlim(0,50)

ggplot(data=df_scatter, aes(x=population,y=vehicles_registered)) +
  geom_smooth(method="lm") +
  geom_point() +
  stat_cor(label.x = 0.01, label.y = 8000)+
  stat_regline_equation(label.x = 0.01, label.y = 10000) +
  xlim(0,2000000)

ggplot(data=df_scatter, aes(x=employment,y=vehicles_registered)) +
  geom_smooth(method="lm") +
  geom_point() +
  stat_cor(label.x = 0.01, label.y = 8000)+
  stat_regline_equation(label.x = 0.01, label.y = 10000) +
  xlim(0,1600000)

ggplot(data=df_scatter, aes(x=On.road.TransportationGasolineGHG.emissions.mtons.CO2e,y=vehicles_registered)) +
  geom_smooth(method="lm") +
  geom_point() +
  stat_cor(label.x = 0.01, label.y = 8000)+
  stat_regline_equation(label.x = 0.01, label.y = 10000)+
  xlim(0,12000000)

ggplot(data=df_scatter, aes(x=On.road.TransportationDieselGHG.emissions.mtons.CO2e,y=vehicles_registered)) +
  geom_smooth(method="lm") +
  geom_point() +
  stat_cor(label.x = 0.01, label.y = 8000)+
  stat_regline_equation(label.x = 0.01, label.y = 10000) +
  xlim(0,2000000)


#calculate McFadden's R-Squared
pscl::pR2(reg_all)["McFadden"]
#calculate variable importance
caret::varImp(reg_all)
#calculate VIF values
car::vif(reg_all)

#HETEROSCEDASTICITY
plot(reg_all)
#COLLINEARITY
reg_all.e = pairs(~df_scatter$income_per_capita + df_scatter$Num_stations + df_scatter$population + df_scatter$employment + df_scatter$On.road.TransportationDieselGHG.emissions.mtons.CO2e + df_scatter$On.road.TransportationGasolineGHG.emissions.mtons.CO2e) 
#OUTLIER
#calculate studentized residuals
stud_resids <- studres(reg_all)
#view first three studentized residuals
head(stud_resids)
#plot predictor variable vs. studentized residuals
plot(df_scatter$income_per_capita + df_scatter$Num_stations + df_scatter$population + df_scatter$population + df_scatter$employment + df_scatter$ResidentialElectricityconsumption..MWh.capita. + df_scatter$On.Road.TransportationGasolineconsumption..gallons.capita. + df_scatter$On.Road.TransportationDieselconsumption..gallons.capita. + df_scatter$On.Road.Transportationvehicle.miles.traveled..miles.capita. + df_scatter$On.road.TransportationGasolineGHG.emissions.mtons.CO2e + df_scatter$On.road.TransportationDieselGHG.emissions.mtons.CO2e , stud_resids,  ylab='Studentized Residuals', xlab='Fitted values') #ylim = c(-3,4) 
#add horizontal line at 0
abline(0, 0)
abline(3, 0, col="red")
abline(-3, 0, col="red")

#removing outliers
# Store the residuals as a new column in DF
df_scatter$studres<-studres(reg_all)
df_scatter$Outs<-ifelse(abs(df_scatter$studres)>=3, 0, 1)
plot(df_scatter$studres, col=df_scatter$Outs+1, pch=16)
#Make a new data.frame with no outliers
df_scatter2<-df_scatter[!df_scatter$Outs==0,]
nrow(df_scatter2)
nrow(df_scatter)
# Plot new data
plot(df_scatter2$studres, col=df_scatter2$Outs+1,pch=16)


#HIGH LEVERAGE POINTS #calculate leverage for each observation in the model
hats <- as.data.frame(hatvalues(reg_all))
#sort observations by leverage, descending
hats[order(-hats['hatvalues(reg_all)']), ]
#plot leverage values for each observation
plot(hatvalues(reg_all),ylab='Leverage', xlab='Index')
plot(df_scatter$income_per_capita + df_scatter$Num_stations + df_scatter$population + df_scatter$population + df_scatter$employment + df_scatter$ResidentialElectricityconsumption..MWh.capita. + df_scatter$On.Road.TransportationGasolineconsumption..gallons.capita. + df_scatter$On.Road.TransportationDieselconsumption..gallons.capita. + df_scatter$On.Road.Transportationvehicle.miles.traveled..miles.capita. + df_scatter$On.road.TransportationGasolineGHG.emissions.mtons.CO2e + df_scatter$On.road.TransportationDieselGHG.emissions.mtons.CO2e, hatvalues(reg_all),ylab='Leverage', xlab='Fitted Values')
plot(hatvalues(reg_all),stud_resids,xlab='Leverage', ylab='Studentized residuals')

df_scatter2 = subset(df_scatter2, select = -c(studres) ) #drops column
df_scatter2 = subset(df_scatter2, select = -c(Outs) ) #drops one column

#regression with final set
reg_f = lm(vehicles_registered ~ income_per_capita + Num_stations + population + population + employment + ResidentialElectricityconsumption..MWh.capita. + On.Road.TransportationGasolineconsumption..gallons.capita. + On.Road.TransportationDieselconsumption..gallons.capita. + On.Road.Transportationvehicle.miles.traveled..miles.capita. + On.road.TransportationGasolineGHG.emissions.mtons.CO2e + On.road.TransportationDieselGHG.emissions.mtons.CO2e, data=df_scatter2)

summary(reg_f)


#Make predictions
predicted <- predict(reg_f, df_scatter2, type="response")
#predicted_f=ifelse(predicted<0.5,0,1)
##predicted_f=ifelse(abs((predicted-df_scatter$Num_EV)/predicted)<0.5,0,1)
df_scatter2$predicted=predicted
#table(df_scatter$predicted, df_scatter$Num_EV)
#mean(df_scatter$predicted==df_scatter$Num_EV)
plot(df_scatter2$vehicles_registered, df_scatter2$predicted)
ggplot() +
  geom_point(aes(x =1:nrow(df_scatter2), y = df_scatter2$vehicles_registered),
             colour = 'red') +
  geom_point(aes(x = 1:nrow(df_scatter2), y = df_scatter2$predicted),
             colour = 'blue') +
  ggtitle('Prediction vs Real values') +
  xlab('Index') +
  ylab('Num_EV')


#sampling
dt = sort(sample(nrow(df_scatter2), nrow(df_scatter2)*.7))
train<-df_scatter2[dt,]
test<-df_scatter2[-dt,]

reg_s = lm(vehicles_registered ~ income_per_capita + Num_stations + Num_ports + population + employment + ResidentialElectricityconsumption..MWh.capita. + On.Road.TransportationGasolineconsumption..gallons.capita. + On.Road.TransportationDieselconsumption..gallons.capita. + On.Road.Transportationvehicle.miles.traveled..miles.capita. + On.road.TransportationGasolineGHG.emissions.mtons.CO2e + On.road.TransportationDieselGHG.emissions.mtons.CO2e, data=train)

predicted <- predict(reg_s, test, type="response")
ggplot() +
  geom_point(aes(x = 1:nrow(test), y = test$vehicles_registered),
             colour = 'red') +
  geom_point(aes(x = 1:nrow(train), y = predict(reg_s, newdata = train)),
            colour = 'blue') +
  ggtitle('Prediction') +
  xlab('X') +
  ylab('Y')
compare <- cbind (actual=test$Num_EV, predicted)  # combine actual and predicted
mean (apply(compare, 1, min)/apply(compare, 1, max))

#create new dataset
df_scatter3 = subset(df_scatter2, select = -c(predicted) ) #drops one column

#Subset selection
#Best subset
regfit.full <- regsubsets(vehicles_registered~., data= df_scatter3, nvmax = 11)
summary(regfit.full)
summary(regfit.full)$cp
which.min(summary(regfit.full)$cp)
coef(regfit.full,7) #to see the coefficient estimates associated with this model (lowest C_p value)
plot(summary(regfit.full)$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
points(7, summary(regfit.full)$cp[7], col = "red", cex = 2,pch = 20)

#Forward subset selection
regfit.fwd <- regsubsets(vehicles_registered~., data = df_scatter3, nvmax = 11, method = "forward")
summary(regfit.fwd)
summary(regfit.fwd)$cp
which.min(summary(regfit.fwd)$cp)
plot(summary(regfit.fwd)$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
points(7, summary(regfit.fwd)$cp[7], col = "blue", cex = 2,pch = 20)


#VALIDATION

####WITH OLD DATA#####
df_scatter = subset(df_scatter, select = -c(Outs) ) #drops one column
df_scatter = subset(df_scatter, select = -c(studres) ) #drops one column

#sampling
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(df_scatter), replace = TRUE,prob=c(0.7,0.3))
test <- (!train)
regfit.fwd <- regsubsets(vehicles_registered~., data = df_scatter[train, ], nvmax = 11 ,method = "forward")
summary(regfit.fwd)

#validation set approach
test.mat <- model.matrix(vehicles_registered~., data = df_scatter[test, ])
val.errors <- rep(NA, 11) 
for (i in 1:11) {
  coefi <- coef(regfit.fwd, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi 
  val.errors[i] <- sqrt(mean((df_scatter$vehicles_registered[test] - pred)^2))
}
which.min(val.errors)
val.errors
plot(val.errors, xlab = "Number of Variables", ylab = "RMSE by Validation Set Approach", type = "l")
points(11, val.errors[11], col = "pink", cex = 2,pch = 20)

#training set
train.mat = model.matrix(vehicles_registered~., data = df_scatter[train, ])
train.errors = rep(NA, 11) 
for (i in 1:11) {
  coefi = coef(regfit.fwd, id = i)
  pred = train.mat[, names(coefi)] %*% coefi 
  train.errors[i] = sqrt(mean((df_scatter$vehicles_registered[train] - pred)^2))
}
train.errors
which.min(train.errors)
plot(train.errors, xlab = "Number of Variables", ylab = "RMSE training", type = "l")
points(11, train.errors[11], col = "red", cex = 2,pch = 20)

#function for prediction
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]]) 
  mat <- model.matrix(form, newdata) 
  coefi <- coef(object, id = id) 
  xvars <- names(coefi) 
  mat[, xvars] %*% coefi
}

#k Fold CV
k <- 50
n <- nrow(df_scatter)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 11, dimnames = list(NULL, paste(1:11)))
for (j in 1:k) {
  regfit.fwd2 <- regsubsets(vehicles_registered~., data = df_scatter[folds != j, ], nvmax = 12, method = "forward") 
  for (i in 1:11) {
    pred <- predict(regfit.fwd2, df_scatter[folds == j, ], id = i) 
    cv.errors[j, i] <- sqrt(mean((df_scatter$vehicles_registered[folds == j] - pred)^2))
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
coef(regfit.fwd2, 2)
par(mfrow = c(1, 1))
plot(mean.cv.errors, xlab = "Number of Variables", ylab = "RMSE by k-Fold CV", type = "l")
points(2, mean.cv.errors[2], col = "green", cex = 2,pch = 20)


plot(val.errors,type="l", xlab = "Number of Variables", ylab = "RMSE",col="red",ylim=c(300,1500))
lines(mean.cv.errors,col="green")
lines(train.errors, col = "blue")
legend(7, 1300,legend=c("Validation Set", "K-Fold","Training Set"), fill=c("red", "green","blue"))



####WITH NEW CLEANED DATA####
#sampling
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(df_scatter3), replace = TRUE,prob=c(0.7,0.3))
test <- (!train)
regfit.fwd <- regsubsets(vehicles_registered~., data = df_scatter3[train, ], nvmax = 11 ,method = "forward")
summary(regfit.fwd)

#validation set approach
test.mat <- model.matrix(vehicles_registered~., data = df_scatter3[test, ])
val.errors <- rep(NA, 11) 
for (i in 1:11) {
  coefi <- coef(regfit.fwd, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi 
  val.errors[i] <- sqrt(mean((df_scatter3$vehicles_registered[test] - pred)^2))
}
which.min(val.errors)
coef(regfit.fwd, 7)
val.errors
plot(val.errors, xlab = "Number of Variables", ylab = "RMSE by Validation Set Approach", type = "l")
points(7, val.errors[7], col = "pink", cex = 2,pch = 20)

#training set
train.mat = model.matrix(vehicles_registered~., data = df_scatter3[train, ])
train.errors = rep(NA, 11) 
for (i in 1:11) {
  coefi = coef(regfit.fwd, id = i)
  pred = train.mat[, names(coefi)] %*% coefi 
  train.errors[i] = sqrt(mean((df_scatter3$vehicles_registered[train] - pred)^2))
}
train.errors
which.min(train.errors)
plot(train.errors, xlab = "Number of Variables", ylab = "RMSE training", type = "l")
points(11, train.errors[11], col = "red", cex = 2,pch = 20)

#function for prediction
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]]) 
  mat <- model.matrix(form, newdata) 
  coefi <- coef(object, id = id) 
  xvars <- names(coefi) 
  mat[, xvars] %*% coefi
}

#k Fold CV
k <- 50
n <- nrow(df_scatter3)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 11, dimnames = list(NULL, paste(1:11)))
for (j in 1:k) {
  regfit.fwd2 <- regsubsets(vehicles_registered~., data = df_scatter3[folds != j, ], nvmax = 12, method = "forward") 
  for (i in 1:11) {
    pred <- predict(regfit.fwd2, df_scatter3[folds == j, ], id = i) 
    cv.errors[j, i] <- sqrt(mean((df_scatter3$vehicles_registered[folds == j] - pred)^2))
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
coef(regfit.fwd2, 8)
par(mfrow = c(1, 1))
plot(mean.cv.errors, xlab = "Number of Variables", ylab = "RMSE by k-Fold CV", type = "l")
points(8, mean.cv.errors[8], col = "green", cex = 2,pch = 20)


plot(val.errors,type="l", xlab = "Number of Variables", ylab = "RMSE",col="red",ylim=c(100,300))
lines(mean.cv.errors,col="green")
lines(train.errors, col = "blue")
legend(7, 250,legend=c("Validation Set", "K-Fold","Training Set"), fill=c("red", "green","blue"))


