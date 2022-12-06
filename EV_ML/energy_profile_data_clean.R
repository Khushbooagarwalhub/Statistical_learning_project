#import important libraries

library(data.table)
library(jsonlite)
library(readxl)
library(readr)
library(tidyselect)
library(tidycensus)
library(tidyverse)
library(lattice)
library(readr)
library(dplyr)
library(readxlsb)

setwd("C:/Users/khush/OneDrive/Desktop/grad_school_apps/grad_app_UT_EER/sem4/Dataset")
#vehicle_inventory <- fread("2016cityandcountyenergyprofiles.csv")

#Read in file and merge top rows to make column names, as header spans multiple rows
header <- scan("2016cityandcountyenergyprofiles.csv", nlines = 1, what = character(),sep=",",quote = NULL)
header2 <- scan("2016cityandcountyenergyprofiles.csv", skip = 1, nlines = 1, what = character(),sep=",",quote = NULL)
header3 <- scan("2016cityandcountyenergyprofiles.csv", skip = 2, nlines = 1, what = character(),sep=",",quote = NULL)
header4 <- scan("2016cityandcountyenergyprofiles.csv", skip = 3, nlines = 1, what = character(),sep=",",quote = NULL)
header5 <- scan("2016cityandcountyenergyprofiles.csv", skip = 4, nlines = 1, what = character(),sep=",",quote = NULL)
vehicle_inventory <- fread("2016cityandcountyenergyprofiles.csv", skip = 5, header = FALSE)
names(vehicle_inventory) <- paste0(header,header2, header3, header4, header5)
colnames(vehicle_inventory)

# Substitutions to clean county name for TX
vehicle_inventory_TX <- vehicle_inventory[vehicle_inventory$state_abbr=='TX']
vehicle_inventory_TX$county_name <- sub(" County", "", vehicle_inventory_TX$county_name)
vehicle_inventory_TX$county <- vehicle_inventory_TX$county_name
vehicle_inventory_TX$county_year <- paste(vehicle_inventory_TX$county_name, "2016", sep="_")

#Read file
write.csv(vehicle_inventory_TX,"C:/Users/khush/OneDrive/Desktop/grad_school_apps/grad_app_UT_EER/sem4/Dataset/energy_profile_2016_kh.csv", row.names = FALSE)


vehicle_inventory <- fread("2016cityandcountylightdutyvehicleinventory.csv", skip=1)
vehicle_inventory <- vehicle_inventory[,1:29]

vehicle_inventory_TX <- vehicle_inventory[vehicle_inventory$state_abbr=='TX']

# Substitutions to clean county name for TX
vehicle_inventory_TX$county_name <- sub(" County", "", vehicle_inventory_TX$county_name)
vehicle_inventory_TX$county <- vehicle_inventory_TX$county_name
vehicle_inventory_TX$county_year <- paste(vehicle_inventory_TX$county_name, "2016", sep="_")

# Write out cleaned data
write.csv(vehicle_inventory_TX,"C:/Users/khush/OneDrive/Desktop/grad_school_apps/grad_app_UT_EER/sem4/Dataset/vehicle_inventory_2016_kh.csv", row.names = FALSE)
