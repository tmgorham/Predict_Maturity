#How to use the fishbase package

library(rfishbase)
library(dplyr)

#set environemtn to access fishbase API
Sys.setenv(FISHBASE_API="fishbase")

#check to see your species names are all up to date
validate_names(c("Oreochromis niloticus", "Carcharhinus coatesi"))
 
#list all available fields
fields = list_fields()

# search tables for a specific list of species
car <- species_list(Family="Carcharhinidae")
popgrowth = popgrowth(species_list=car)
# popgrowth is table with life history traits/things people use for stock assessments, including covariates like repro mode and temp
stocks = stocks(species_list = car)

#get temperature prefs for carcharhinids
# only for female or mixed sexes
popgrowth = popgrowth %>% select(Species, StockCode, Sex, Temperature) %>% filter(Temperature>1, Sex!='male')


# add stock descriptions
stocks = stocks %>% select(StockCode, StockDefs, StockDefsGeneral)

car_temp <- left_join(popgrowth, stocks, by=c('StockCode'='StockCode'))

#save dataset to data folder
setwd("C:/Users/tmgor/Dropbox/Taylor Chapter 3/Data/source data")
write.csv(car_temp, "fishbase_temp.csv", row.names=F)


