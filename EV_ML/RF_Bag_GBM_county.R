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

#Functions used from below library. Referred book for method: Hands-On Machine Learning with R - Bradley Boehmke & Brandon Greenwell
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


setwd("C:/Users/khush/OneDrive/Desktop/grad_school_apps/grad_app_UT_EER/sem4/Dataset")
set.seed (4)

#Read files
df_combined <- fread("combined_data.csv")
df_county <- fread("df_county_modified.csv")
#Remove Columns
df_county <- df_county[,-c(1,4)]

colnames(df_county)[8] <- "Residential_electricity_consumption_pc"
colnames(df_county)[9] <- "Gasoline_consumption_pc"
colnames(df_county)[10] <- "Diesel_consumption_pc"
colnames(df_county)[11] <- "Miles_travelled_pc"
colnames(df_county)[12] <- "gasolineGHG_emissions_mtons"
colnames(df_county)[13] <- "diesel_ghg_emission_mtons"

#Remove commas and typecast to numeric
df_county$income_per_capita <- gsub('\\$','',df_county$income_per_capita)
df_county$income_per_capita <- gsub(',','',df_county$income_per_capita)
df_county$income_per_capita <- as.numeric(df_county$income_per_capita)
df_county$Gasoline_consumption_pc <- as.numeric(gsub(',','',df_county$Gasoline_consumption_pc))
df_county$Diesel_consumption_pc <- as.numeric(gsub(',','',df_county$Diesel_consumption_pc))
df_county$Miles_travelled_pc <- as.numeric(gsub(',','',df_county$Miles_travelled_pc))
df_county$gasolineGHG_emissions_mtons <- as.numeric(gsub(',','',df_county$gasolineGHG_emissions_mtons))
df_county$diesel_ghg_emission_mtons <- as.numeric(gsub(',','',df_county$diesel_ghg_emission_mtons))

#Merge the two files
merge1<- merge(df_county,df_combined,by.x="County",by.y="County")

#Read SVI data
df_svi <- fread("Texas_COUNTY.csv")

colnames(df_svi)[5] <- "County"

colnames(df_svi)


df_svi2 <- df_svi[, c("County","E_NOHSDP", "E_POV150","E_MINRTY","E_NOVEH","E_AGE65","E_AGE17","AREA_SQMI","E_TOTPOP")]

df_svi2$pop_density <- (df_svi$E_TOTPOP/df_svi$AREA_SQMI)

#Remove commas and typecast to numeric
df_county_final <- merge(merge1,df_svi2,by="County")
df_county_final <- df_county_final[,-c(25,26)]
df_county_final$Percent_Urban <- as.numeric(gsub('%','',df_county_final$Percent_Urban))
df_county_final$US_Highways <- as.numeric(gsub('N\\/A','0',df_county_final$US_Highways))
df_county_final$IH_Highways <- as.numeric(gsub('N\\/A','0',df_county_final$IH_Highways))
df_county_final$State_Highways <- as.numeric(gsub('N\\/A','0',df_county_final$State_Highways))
write.csv(df_county_final,"df_county_final.csv")
df_county <- df_county_final

#Divide the data into train and test 
train <- sample (1: nrow(df_county), nrow(df_county)*0.70)
dft <- df_county[train,]
#Do this only first time
#write.csv(train,"train_rf.csv")
#keep sampling same for all the models so that you can compare the results easily
train <- fread("train_rf_exp2.csv")
train <- as.integer(train$x)

#Modify these columns to per capita
df_county$E_MINRTY <- df_county$E_MINRTY / df_county$population
df_county$E_NOVEH  <- df_county$E_NOVEH  / df_county$population
df_county$E_NOHSDP <- df_county$E_NOHSDP/  df_county$population
df_county$E_POV150 <- df_county$E_POV150 /  df_county$population
df_county$E_AGE65 <- df_county$E_AGE65 / df_county$population
df_county$E_AGE17 <- df_county$E_AGE17 / df_county$population

#correlation plot
df_county_selected <- df_county[,-c(1,14)]
county.cor = cor(df_county_selected)
png(filename = "mycorrplot.png", width = 1500, height = 1000)
corrplot(county.cor)
dev.off()

df_county$EV_infra = df_county$Num_stations + df_county$Num_ports
df_county <- df_county[,-c(4,5,9,10)]

#Modify these columns to per capita
df_county$gasolineGHG_emissions_mtons <- df_county$gasolineGHG_emissions_mtons / df_county$population
df_county$diesel_ghg_emission_mtons <- df_county$diesel_ghg_emission_mtons / df_county$population
df_county$employment <- df_county$employment / df_county$population
df_county <- df_county[, -c(10)]

#Plot correlation again
df_county_selected <- df_county[,-c(1)]
county2.cor = cor(df_county_selected)
png(filename = "mycorrplot2.png", width = 1500, height = 1000)
corrplot(county2.cor)
dev.off()

##Random forest Model 
#rf.ev <- randomForest(vehicles_registered~ income_per_capita+Num_stations+Num_ports+population+employment+Gasoline_consumption_pc+Miles_travelled_pc+diesel_ghg_emission_mtons+Num_stations+population+Residential_electricity_consumption_pc+Gasoline_consumption_pc+Diesel_consumption_pc+Miles_travelled_pc+gasolineGHG_emissions_mtons+diesel_ghg_emission_mtons, data = df_county,subset = train , mtry = 4, importance = TRUE)
rf.ev <- randomForest(vehicles_registered~ income_per_capita+EV_infra+employment+Residential_electricity_consumption_pc+Miles_travelled_pc+US_Highways+IH_Highways+State_Highways+Percent_Urban+E_NOHSDP+E_POV150+E_MINRTY+E_NOVEH+E_AGE65+E_AGE17+pop_density, data = df_county,subset = train , mtry = 5, importance = TRUE)
rf.ev

ev.test <- df_county[-train, "vehicles_registered"]
#Predict on training data
yhat.rf <- predict(rf.ev , newdata = df_county[-train , ])


plot(ev.test$vehicles_registered,yhat.rf, xlab="Observed Registered Vehicles", ylab="Predicted Registered Vehicles")
abline (0, 1)
rmse <- (mean(( yhat.rf - ev.test$vehicles_registered)^2)^0.5)
rmse
#Calculated the percentage RMSE. Since some observations are zero, we cannot normalize with true data.
#rmse_pc <- (mean(((yhat.rf)/(ev.test$vehicles_registered) - 1)^2)^0.5)
#As we are more interested in calculating the RMSE for counties where EV is not close to zero
#We normalize with mid value of the range - (max+min)/2
rmse_pc <- rmse/(min(ev.test$vehicles_registered)+max(ev.test$vehicles_registered)/2)
rmse_pc*100

#Change the number of trees for the random forest model 
#rf.ev2 <- randomForest(vehicles_registered~ income_per_capita+Num_stations+Num_ports+population+employment+Residential_electricity_consumption_pc+Gasoline_consumption_pc+Diesel_consumption_pc+Miles_travelled_pc+gasolineGHG_emissions_mtons+diesel_ghg_emission_mtons+US_Highways+IH_Highways+State_Highways+Percent_Urban+E_NOHSDP+E_POV150+E_MINRTY+E_NOVEH+E_AGE65+E_AGE17+pop_density, data = df_county,subset = train , mtry = 7,ntree = 25)
#rf.ev2
#yhat.rf2 <- predict(rf.ev2 , newdata = df_county[-train , ])
#plot(ev.test$vehicles_registered,yhat.rf2, xlab="Observed Registered Vehicles", ylab="Predicted Registered Vehicles")
#abline (0, 1)
#rmse <- (mean(( yhat.rf2 - ev.test$vehicles_registered)^2)^0.5)
#rmse

#View the importance of each function
#importance(rf.ev)
varImpPlot(rf.ev)

#############################
#Random forest using ranger 
#############################

# train a default random forest model
rf_ranger.ev <- ranger(
  vehicles_registered~ income_per_capita+EV_infra+employment+Residential_electricity_consumption_pc+Miles_travelled_pc+US_Highways+IH_Highways+State_Highways+Percent_Urban+E_NOHSDP+E_POV150+E_MINRTY+E_NOVEH+E_AGE65+E_AGE17+pop_density, 
  data = df_county[train,],
  mtry = floor(16 / 3),
  respect.unordered.factors = "order",
  seed = 123
)

# get OOB RMSE
(default_rmse <- sqrt(rf_ranger.ev$prediction.error))

yhat.ranger <- predict(rf_ranger.ev , data = df_county[-train , ])


plot(ev.test$vehicles_registered,yhat.ranger$predictions, xlab="Observed Registered Vehicles", ylab="Predicted Registered Vehicles")
abline (0, 1)
rmse <- (mean(( yhat.ranger$predictions - ev.test$vehicles_registered)^2)^0.5)
rmse

#Hyperparameter tuning, number of trees = pX10

#perform a grid search for hyperparameter tuning

# create hyperparameter grid
hyper_grid <- expand.grid(
  #mtry = floor(11 * c(.05, .15, .25, .333, .4)),
  num.trees = c(16*1, 16*3, 16*5, 16*10, 16*15, 16*20, 16*30),
  mtry = c(1,2,4,5,6),
  min.node.size = c(1,3,5,10), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8),                       
  rmse = NA,
  test_rmse = NA
)

# execute full cartesian grid search
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = vehicles_registered~ income_per_capita+EV_infra+employment+Residential_electricity_consumption_pc+Miles_travelled_pc+US_Highways+IH_Highways+State_Highways+Percent_Urban+E_NOHSDP+E_POV150+E_MINRTY+E_NOVEH+E_AGE65+E_AGE17+pop_density, 
    data            = df_county[train,], 
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
  yhat.ranger <- predict(fit , data = df_county[-train , ])
  hyper_grid$test_rmse[i] <- (mean(( yhat.ranger$predictions - ev.test$vehicles_registered)^2)^0.5)
}

# assess top 25 models
hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(25)

###

###
#Test top model
rf_ranger.ev <- ranger(
  vehicles_registered~ income_per_capita+EV_infra+employment+Residential_electricity_consumption_pc+Miles_travelled_pc+US_Highways+IH_Highways+State_Highways+Percent_Urban+E_NOHSDP+E_POV150+E_MINRTY+E_NOVEH+E_AGE65+E_AGE17+pop_density, 
  data = df_county[train,],
  num.trees       = 480,
  mtry = 6,
  min.node.size = 3,
  replace = FALSE,
  sample.fraction = 0.80,
  respect.unordered.factors = "order",
  seed = 123
)
# get OOB RMSE
(default_rmse <- sqrt(rf_ranger.ev$prediction.error))

yhat.ranger <- predict(rf_ranger.ev , data = df_county[-train , ])
plot(ev.test$vehicles_registered,yhat.ranger$predictions, xlab="Observed Registered Vehicles", ylab="Predicted Registered Vehicles")
abline (0, 1)
#Calculate test rmse
rmse <- (mean(( yhat.ranger$predictions - ev.test$vehicles_registered)^2)^0.5)
rmse

rmse_pc <- rmse/(min(ev.test$vehicles_registered)+max(ev.test$vehicles_registered)/2)
rmse_pc*100
#####
# create hyperparameter grid(for plotting against n trees)
hyper_grid <- expand.grid(
  num.trees = c(16*1, 16*2, 16*3, 16*4, 16*5, 16*6, 16*7, 16*8, 16*9, 16*10, 16*15, 16*20, 16*30, 16*40, 16*50),
  #num.trees = 80,
  mtry = 6,
  min.node.size = 3, 
  replace = FALSE,                               
  sample.fraction = .8,
  rmse = NA,
  test_rmse = NA
)

# execute full cartesian grid search
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = vehicles_registered~ income_per_capita+EV_infra+employment+Residential_electricity_consumption_pc+Miles_travelled_pc+US_Highways+IH_Highways+State_Highways+Percent_Urban+E_NOHSDP+E_POV150+E_MINRTY+E_NOVEH+E_AGE65+E_AGE17+pop_density, 
    data            = df_county[train,], 
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
  yhat.ranger <- predict(fit , data = df_county[-train , ])
  hyper_grid$test_rmse[i] <- (mean(( yhat.ranger$predictions - ev.test$vehicles_registered)^2)^0.5)
}

# assess top 100 models
hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(25)

plot(hyper_grid[,"num.trees"], hyper_grid[,"rmse"], type = "o",  bg = "red",   # Fill color
     col = "red",xlab = "Number of trees", ylab = "Test RMSE",family = "mono")
plot(hyper_grid[,"num.trees"], hyper_grid[,"test_rmse"], type = "o",col= "blue",family="mono",xlab = "Number of trees", ylab = "TEST RMSE")
#Feature importance

# re-run model with permutation-based variable importance
# re-run model with impurity-based variable importance
rf_impurity <- ranger(
  formula = vehicles_registered~ income_per_capita+EV_infra+employment+Residential_electricity_consumption_pc+Miles_travelled_pc+US_Highways+IH_Highways+State_Highways+Percent_Urban+E_NOHSDP+E_POV150+E_MINRTY+E_NOVEH+E_AGE65+E_AGE17+pop_density,
  data = df_county, 
  num.trees = 340,
  mtry = 10,
  min.node.size = 3,
  sample.fraction = .80,
  replace = FALSE,
  importance = "impurity",
  respect.unordered.factors = "order",
  verbose = FALSE,
  seed  = 123
)



rf_permutation <- ranger(
  formula = vehicles_registered~ income_per_capita+EV_infra+employment+Residential_electricity_consumption_pc+Miles_travelled_pc+US_Highways+IH_Highways+State_Highways+Percent_Urban+E_NOHSDP+E_POV150+E_MINRTY+E_NOVEH+E_AGE65+E_AGE17+pop_density, 
  data = df_county, 
  num.trees = 340,
  mtry = 10,
  min.node.size = 3,
  sample.fraction = .80,
  replace = FALSE,
  importance = "permutation",
  respect.unordered.factors = "order",
  verbose = FALSE,
  seed  = 123
)
p1 <- vip::vip(rf_impurity, num_features = 10, bar = TRUE, aesthetics= list(color="midnightblue", size=0.2,fill = "midnightblue"),width=0.02)
p2 <- vip::vip(rf_permutation, num_features =10 , bar = TRUE, aesthetics= list(color="midnightblue", size=0.2,fill = "midnightblue"), width=0.02)

gridExtra::grid.arrange(p1, p2, nrow = 1)


# Get first tree
#tree_1 <- gettree(rf_ranger.ev, k = 1)
#library(reprtree)
#reprtree:::plot.getTree(rf.ev)
#randomForest::getTree(rf.ev, k=3, labelVar = TRUE)

####################################
#Bagging 
####################################
#Bagging 
bag.ev <- randomForest(vehicles_registered~ income_per_capita+EV_infra+employment+Residential_electricity_consumption_pc+Miles_travelled_pc+US_Highways+IH_Highways+State_Highways+Percent_Urban+E_NOHSDP+E_POV150+E_MINRTY+E_NOVEH+E_AGE65+E_AGE17+pop_density, data = df_county,subset = train , mtry = 16, importance = TRUE)
bag.ev

yhat.bag <- predict(bag.ev , newdata = df_county[-train , ])


plot(ev.test$vehicles_registered,yhat.bag, xlab="Observed Registered Vehicles", ylab="Predicted Registered Vehicles")
abline (0, 1)
rmse <- (mean(( yhat.bag - ev.test$vehicles_registered)^2)^0.5)
rmse

##using bagging package in R
# make bootstrapping reproducible
set.seed(123)

# train bagged model
bag.ev2 <- bagging(
  formula = vehicles_registered~ income_per_capita+EV_infra+employment+Residential_electricity_consumption_pc+Miles_travelled_pc+US_Highways+IH_Highways+State_Highways+Percent_Urban+E_NOHSDP+E_POV150+E_MINRTY+E_NOVEH+E_AGE65+E_AGE17+pop_density,
  data = df_county[train , ],
  nbagg = 100,  
  coob = TRUE,
  control = rpart.control(minsplit = 2, cp = 0)
)

bag.ev2

## create hyperparameter grid
hyper_grid <- expand.grid(
    nbagg = c(50,100,150,200,250,300,350,400,450,500)
  )
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- bagging(
    formula         = vehicles_registered~ income_per_capita+EV_infra+employment+Residential_electricity_consumption_pc+Miles_travelled_pc+US_Highways+IH_Highways+State_Highways+Percent_Urban+E_NOHSDP+E_POV150+E_MINRTY+E_NOVEH+E_AGE65+E_AGE17+pop_density, 
    data            = df_county[train,], 
    nbagg       = hyper_grid$nbagg[i],
    coob = TRUE,
    control = rpart.control(minsplit = 2, cp = 0)
  )
  
  # export OOB error 
  hyper_grid$rmse[i] <- fit$err
}

# assess top 25 models
hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(25)
#using cross validation 
library(caret)
bag_cv <- train(
  vehicles_registered~ income_per_capita+EV_infra+employment+Residential_electricity_consumption_pc+Miles_travelled_pc+US_Highways+IH_Highways+State_Highways+Percent_Urban+E_NOHSDP+E_POV150+E_MINRTY+E_NOVEH+E_AGE65+E_AGE17+pop_density,
  data = df_county[train,],
  method = "treebag",
  trControl = trainControl(method = "cv", number = 10),
  nbagg = 200,  
  control = rpart.control(minsplit = 2, cp = 0)
)

yhat.bag <- predict(bag_cv , newdata = df_county[-train , ])


plot(ev.test$vehicles_registered,yhat.bag, xlab="Observed Registered Vehicles", ylab="Predicted Registered Vehicles")
abline (0, 1)
rmse <- (mean(( yhat.bag - ev.test$vehicles_registered)^2)^0.5)
rmse
rmse_pc <- rmse/(min(ev.test$vehicles_registered)+max(ev.test$vehicles_registered)/2)
rmse_pc*100
vip::vip(bag_cv, num_features = 16, aesthetics = list(color = "midnightblue", fill = "midnightblue"))

# Construct partial dependence plots
p1 <- pdp::partial(
  bag_cv, 
  pred.var = "EV_infra",
  grid.resolution = 20
) %>% 
  autoplot()

p2 <- pdp::partial(
  bag_cv, 
  pred.var = "Miles_travelled_pc", 
  grid.resolution = 20
) %>% 
  autoplot()

gridExtra::grid.arrange(p1, p2, nrow = 1)
p1
p2

#################
##Boosting 
#################

library(gbm)
set.seed (1)
boost.ev <- gbm( vehicles_registered~ income_per_capita+EV_infra+employment+Residential_electricity_consumption_pc+Miles_travelled_pc+US_Highways+IH_Highways+State_Highways+Percent_Urban+E_NOHSDP+E_POV150+E_MINRTY+E_NOVEH+E_AGE65+E_AGE17+pop_density, data = df_county[train, ],
                 distribution = "gaussian", n.trees = 5000,
                 interaction.depth = 2, shrinkage = 0.01)
summary(boost.ev)
plot(boost.ev , i = "EV_infra")
plot(boost.ev , i = "pop_density")
plot(boost.ev , i = "employment")
yhat.boost <- predict(boost.ev, newdata = df_county[-train , ], n.trees = 5000)

plot(ev.test$vehicles_registered,yhat.boost, xlab="Observed Registered Vehicles", ylab="Predicted Registered Vehicles")
abline (0, 1)
rmse <- (mean(( yhat.boost - ev.test$vehicles_registered)^2)^0.5)
rmse

#Gradient Boosting (basic GBM model creation)

gbm.ev<- gbm(
  formula = vehicles_registered~ income_per_capita+EV_infra+employment+Residential_electricity_consumption_pc+Miles_travelled_pc+US_Highways+IH_Highways+State_Highways+Percent_Urban+E_NOHSDP+E_POV150+E_MINRTY+E_NOVEH+E_AGE65+E_AGE17+pop_density,
  data = df_county [train,],
  distribution = "gaussian",  # SSE loss function
  n.trees = 5000,
  shrinkage = 0.1,
  interaction.depth = 3,
  n.minobsinnode = 10,
  cv.folds = 10
)

# find index for number trees with minimum CV error
best <- which.min(gbm.ev$cv.error)

# get MSE and compute RMSE
sqrt(gbm.ev$cv.error[best])


# plot error curve
gbm.perf(gbm.ev, method = "cv")

##grid search

# search grid
hyper_grid <- expand.grid(
  n.trees = c(3000,5000,7000),
  shrinkage = 0.1,
  interaction.depth = c(3, 5, 7),
  n.minobsinnode = c(5, 10, 15)
)

# execute grid search
for(i in seq_len(nrow(hyper_grid))) {
  
  # fit gbm
  set.seed(123)  # for reproducibility
  train_time <- system.time({
    m <- gbm(
      formula = vehicles_registered~ income_per_capita+EV_infra+employment+Residential_electricity_consumption_pc+Miles_travelled_pc+US_Highways+IH_Highways+State_Highways+Percent_Urban+E_NOHSDP+E_POV150+E_MINRTY+E_NOVEH+E_AGE65+E_AGE17+pop_density,
      data = df_county [train,],
      distribution = "gaussian",
      #distribution = "poisson",
      n.trees = hyper_grid$n.trees[i], 
      shrinkage = 0.1, 
      interaction.depth = hyper_grid$interaction.depth[i], 
      n.minobsinnode = hyper_grid$n.minobsinnode[i],
      cv.folds = 10 
    )
  })
  
  # add SSE, trees, and training time to results
  hyper_grid$RMSE[i]  <- sqrt(min(m$cv.error))
  
}

# results
arrange(hyper_grid, RMSE)

gbm.ev<- gbm(
  formula = vehicles_registered~ income_per_capita+EV_infra+employment+Residential_electricity_consumption_pc+Miles_travelled_pc+US_Highways+IH_Highways+State_Highways+Percent_Urban+E_NOHSDP+E_POV150+E_MINRTY+E_NOVEH+E_AGE65+E_AGE17+pop_density,
  data = df_county [train,],
  distribution = "gaussian",  # SSE loss function
  #distribution = "poisson",
  n.trees = 3000,
  shrinkage = 0.1,
  interaction.depth = 5,
  n.minobsinnode = 5,
  cv.folds = 10
)

sqrt(min(gbm.ev$cv.error))

yhat.boost <- predict(gbm.ev, newdata = df_county[-train , ], n.trees = 3000)

plot(ev.test$vehicles_registered,yhat.boost, xlab="Observed Registered Vehicles", ylab="Predicted Registered Vehicles")
abline (0, 1)
#calculate RMSE for test data
rmse <- (mean(( yhat.boost - ev.test$vehicles_registered)^2)^0.5)
rmse
rmse_pc <- rmse/(min(ev.test$vehicles_registered)+max(ev.test$vehicles_registered)/2)
rmse_pc*100

