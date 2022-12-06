#Import the required libraries
library(data.table)
library(jsonlite)
library(rpart)
library(e1071)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(maptree)
library(dplyr)
#library(InformationValue)
library(MASS)
#library(pscl)
#library(caret)
#library(ISLR)
#library(Rfast)
#library(diceR)
#library("readxl")
library(randomForest)

#Codes taken from 
library(dplyr)    # for data wrangling
library(ggplot2)  # for awesome graphics

# Modeling packages
library(ranger)   # a c++ implementation of random forest 

library(xgboost)

library(gbm)
library(pdp)

library(corrplot)

library(caret)
library(ipred)
library(tidyverse)



setwd("C:/Users/khush/OneDrive/Desktop/grad_school_apps/grad_app_UT_EER/sem4/Dataset")

#df_zip <- fread("19zp44tx.csv")


#df_port_zip <- fread("EV_Charging_Station_Network_Austin.csv")
#colnames(df_port_zip)
#df_port_selected <- df_port_zip[ , c(6,7)]
#df_port_sum <- df_port_selected %>% group_by(`Postal Code`) %>% 
#  summarise(sum_ports=sum(`No. of Ports`),
#            .groups = 'drop') %>%
#  as.data.frame

#Read in EV infra data zip-wise
df_stations <- fread("alt_fuel_stations (Oct 29 2022).csv")
df_stations_selected <- df_stations [,c(7,18,19,20)]
df_stations_selected[is.na(df_stations_selected)] <- 0
df_stations_sum <- df_stations_selected %>% group_by(ZIP) %>% 
  summarise(sum_EVL1=sum(`EV Level1 EVSE Num`),
            sum_EVL2=sum(`EV Level2 EVSE Num`),
            sum_FAST=sum(`EV DC Fast Count`),
            .groups = 'drop') %>%
  as.data.frame
#Read in IRS zip data
df_zip <- fread("TX_zip_data_irs.csv")
df_zip1 <- merge(df_zip, df_stations_sum,by.x="ZIP",by.y="ZIP", all=TRUE)

df_ev <- fread("EV_zip.csv")
df_ev <- df_ev[,c(1,117)]
df_zip2 <- merge(df_zip1, df_ev,by.x="ZIP",by.y="Zip Code", all=TRUE)
df_zip3 <- df_zip2[!is.na(df_zip2$Population)]
df_zip3[is.na(df_zip3)] <- 0


#Data cleaning steps
colnames(df_zip3)[2] <- "Number_of_household"
colnames(df_zip3)[10] <- "Vehicles_registered"

df_zip3$Num_port <- df_zip3$sum_EVL1 + df_zip3$sum_EVL2 + df_zip3$sum_FAST
df_zip3$Number_of_household <- as.numeric(gsub(",","",df_zip3$Number_of_household))
df_zip3$Population <- as.numeric(gsub(",","",df_zip3$Population))
df_zip3$P_Elderly <- as.numeric(gsub(",","",df_zip3$P_Elderly))
df_zip3$Income <- as.numeric(gsub(",","",df_zip3$Income))

########################
##Random forest 
########################

#Divide the data into train and test 
train <- sample (1: nrow(df_zip3), nrow(df_zip3)*0.70)

##Random forest Model 
#rf.ev <- randomForest(vehicles_registered~ income_per_capita+Num_stations+Num_ports+population+employment+Gasoline_consumption_pc+Miles_travelled_pc+diesel_ghg_emission_mtons+Num_stations+population+Residential_electricity_consumption_pc+Gasoline_consumption_pc+Diesel_consumption_pc+Miles_travelled_pc+gasolineGHG_emissions_mtons+diesel_ghg_emission_mtons, data = df_county,subset = train , mtry = 4, importance = TRUE)
#rf.ev <- randomForest(Vehicles_registered~ Number_of_household + Population + P_Elderly + Income + Unemployment + Num_port, data = df_zip3,subset = train , mtry = 6, importance = TRUE)
rf.ev <- randomForest(Vehicles_registered~ Number_of_household + Population + P_Elderly + Income + Unemployment + sum_EVL1 + sum_EVL2 + sum_FAST, data = df_zip3,subset = train , mtry = 2, importance = TRUE)
rf.ev

ev.test <- df_zip3[-train, "Vehicles_registered"]

yhat.rf <- predict(rf.ev , newdata = df_zip3[-train , ])


plot(ev.test$Vehicles_registered,yhat.rf, xlab="Observed Registered Vehicles", ylab="Predicted Registered Vehicles")
abline (0, 1)
rmse <- (mean(( yhat.rf - ev.test$Vehicles_registered)^2)^0.5)
rmse
rmse_pc <- (mean(((yhat.rf+500)/(ev.test$Vehicles_registered+500) - 1)^2)^0.5)
rmse_pc
varImpPlot(rf.ev)
##using bagging package in R
# make bootstrapping reproducible
set.seed(123)

hyper_grid <- expand.grid(
  #mtry = floor(11 * c(.05, .15, .25, .333, .4)),
  num.trees = c(8*1, 8*3, 8*5, 8*10, 8*15, 8*20, 8*30),
  mtry = c(1,2,3,4),
  min.node.size = c(1,3,5), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8),                       
  rmse = NA,
  test_rmse = NA
)

# execute full cartesian grid search
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = Vehicles_registered~ Number_of_household + Population + P_Elderly + Income + Unemployment + sum_EVL1 + sum_EVL2 + sum_FAST, 
    data            = df_zip3[train,], 
    num.trees       = hyper_grid$num.trees[i],
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
    respect.unordered.factors = 'order',
  )
  
  # export OOB error 
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
  yhat.ranger <- predict(fit , data = df_zip3[-train , ])
  hyper_grid$test_rmse[i] <- (mean(( yhat.ranger$predictions - ev.test$Vehicles_registered)^2)^0.5)
}

# assess top 100 models
hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(100)

###

###
#Test top model
rf_ranger.ev <- ranger(
  Vehicles_registered~ Number_of_household + Population + P_Elderly + Income + Unemployment + sum_EVL1 + sum_EVL2 + sum_FAST, 
  data = df_zip3[train,],
  num.trees       = 240,
  mtry = 4,
  min.node.size = 3,
  replace = FALSE,
  sample.fraction = 0.5,
  respect.unordered.factors = "order",
  seed = 123
)
# get OOB RMSE
(default_rmse <- sqrt(rf_ranger.ev$prediction.error))

yhat.ranger <- predict(rf_ranger.ev , data = df_zip3[-train , ])
plot(ev.test$Vehicles_registered,yhat.ranger$predictions, xlab="Observed Registered Vehicles", ylab="Predicted Registered Vehicles")
abline (0, 1)
#Calculate RMSE for test set
rmse <- (mean(( yhat.ranger$predictions - ev.test$Vehicles_registered)^2)^0.5)
rmse
rmse_pc <- rmse/(min(ev.test$Vehicles_registered)+max(ev.test$Vehicles_registered)/2)
rmse_pc*100
