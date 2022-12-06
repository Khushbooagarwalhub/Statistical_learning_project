library(readxl)
library(dplyr)
en_prfile = read.csv("energy_profile_2016_kh.csv")
ev_reg = read.csv("EV_reg_TX.csv")
p_cap = read.csv("income_p_capita_city.csv")
p_cap_county = read.csv("income_per_capita_county.csv")
ports = read.csv("ports_clean.csv")
stations = read.csv("stations_clean.csv")

###
View(en_prfile)
View(ports)
View(p_cap)
en_prfile$county_state_name = gsub(" County, TX","", en_prfile$county_state_name)


profiles = subset(en_prfile,select = c("County","population","employment","ResidentialElectricityconsumption..MWh.capita.",
            "On.Road.TransportationGasolineconsumption..gallons.capita.","On.Road.TransportationDieselconsumption..gallons.capita.",
            "On.Road.Transportationvehicle.miles.traveled..miles.capita.","On.road.TransportationGasolineGHG.emissions.mtons.CO2e",
            "On.road.TransportationDieselGHG.emissions.mtons.CO2e"))

ev_reg = setNames(aggregate(ev_reg$Count,by=list(ev_reg$County),FUN=sum),c("County","Num_EV"))

ports$Num_ports = ports$EV.DC.Fast.Count+ports$EV.Level1.EVSE.Num+ports$EV.Level2.EVSE.Num
ports = setNames(aggregate(ports$Num_ports,by=list(ports$County),FUN=sum),c("County","Num_ports"))

stations$Num_ports = stations$EV.DC.Fast.Count+stations$EV.Level1.EVSE.Num+stations$EV.Level2.EVSE.Num
stations = setNames(aggregate(stations$Num_ports,by=list(stations$County),FUN=sum,header=T),c("County","Num_stations"))

df = merge(p_cap_county,stations, by.x="county",by.y="County",all.x=T)
df = df %>% rename("County" = "county")
df = merge(df,ports,by="County",all.x=T)

df = merge(df,profiles,by="County",all.x=T)
df = merge(df,ev_reg,by="County",all.x=T)

df[is.na(df)] <- 0
df =df[ , !(names(df) %in% "X")]

#####City
ev_reg = read.csv("EV_reg_TX.csv")
p_cap = read.csv("income_p_capita_city.csv")
p_cap_county = read.csv("income_per_capita_county.csv")
ports = read.csv("ports_clean.csv")
stations = read.csv("stations_clean.csv")


ev_reg = setNames(aggregate(ev_reg$Count,by=list(ev_reg$City),FUN=sum),c("City","Num_EV"))

ports$Num_ports = ports$EV.DC.Fast.Count+ports$EV.Level1.EVSE.Num+ports$EV.Level2.EVSE.Num
ports = setNames(aggregate(ports$Num_ports,by=list(ports$City),FUN=sum),c("City","Num_ports"))

stations$Num_ports = stations$EV.DC.Fast.Count+stations$EV.Level1.EVSE.Num+stations$EV.Level2.EVSE.Num
stations = setNames(aggregate(stations$Num_ports,by=list(stations$City),FUN=sum,header=T),c("City","Num_stations"))
View(p_cap)

df = merge(p_cap,stations, by.x="City",by.y="City",all.x=T)
df = merge(df,ports,by="City",all.x=T)

df = merge(df,ev_reg,by="City",all.x=T)

df[is.na(df)] <- 0
df =df[ , !(names(df) %in% "X")]
