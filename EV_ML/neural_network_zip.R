set.seed(500)
library(neuralnet)
library(MASS)

setwd("C:/Users/khush/OneDrive/Desktop/grad_school_apps/grad_app_UT_EER/sem4/Dataset")
set.seed (4)

#df_zip <- fread("New_data/19zp44tx.csv")


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



colnames(df_zip3)[2] <- "Number_of_household"
colnames(df_zip3)[10] <- "Vehicles_registered"

df_zip3$Num_port <- df_zip3$sum_EVL1 + df_zip3$sum_EVL2 + df_zip3$sum_FAST

df_zip3$Number_of_household <- as.numeric(gsub(",","",df_zip3$Number_of_household))
df_zip3$Population <- as.numeric(gsub(",","",df_zip3$Population))
df_zip3$P_Elderly <- as.numeric(gsub(",","",df_zip3$P_Elderly))
df_zip3$Income <- as.numeric(gsub(",","",df_zip3$Income))
df_zip3$Unemployment <- as.numeric(df_zip3$Unemployment)
df_zip3[is.na(df_zip3)] <- 0

# Normalize the data
maxs <- as.numeric(apply(df_zip3, 2, max)) 
mins <- as.numeric(apply(df_zip3, 2, min))
scaled <- as.data.frame(scale(df_zip3, center = mins, 
                              scale = maxs - mins))

#Split the data into training and testing set
index <- sample(1:nrow(df_zip3), round(0.70 * nrow(df_zip3)))
train_ <- scaled[index,]
test_ <- scaled[-index,]



nn <- neuralnet(Vehicles_registered~ Number_of_household + Population + P_Elderly + Income + Unemployment + sum_EVL1 + sum_EVL2 + sum_FAST,
                data = train_, hidden = c(5, 3), 
                linear.output = TRUE)

# Predict on test data
pr.nn <- compute(nn, test_[,-c(1,10)])

# Compute mean squared error
pr.nn_ <- pr.nn$net.result * (max(df_zip3$Vehicles_registered) - min(df_zip3$Vehicles_registered)) 
+ min(df_zip3$Vehicles_registered)
test.r <- (test_$Vehicles_registered) * (max(df_zip3$Vehicles_registered) - min(df_zip3$Vehicles_registered)) + 
  min(df_zip3$Vehicles_registered)
RMSE.nn <- (sum((test.r - pr.nn_)^2) / nrow(test_))^0.5
RMSE.nn
# Plot the neural network
plot(nn)


