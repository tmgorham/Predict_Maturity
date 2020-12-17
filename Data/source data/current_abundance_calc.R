#Script to calculate sharks per hour per reef for all species
# input for reference points

#upload data
setwd('C:/Users/tmgor/Dropbox/finprint_model_inputs')
list.files()
db <- read.csv('maxnspecies_&covariates_2019-06-12.csv')

#step 1 - calculate number of hours of video per reef
library(dplyr)
names(db)
reefhours <- db %>% group_by(region_name, location_name, site_name, trip_code, reef_id) %>% summarise(numsets=n_distinct(set_id))
db %>% filter(reef_id==43, trip_code=='FP_2016_BZ_01')       # double check results

#step 2 - calculate sum of sharks observed per species per reef
sharks <- db %>% group_by(trip_code, reef_id, common_name, latin_name, shark_id) %>% summarise(sum_maxn=sum(maxn)) %>% filter(is.na(shark_id)==FALSE)

# step 3 - calculate sharks per hour per species 
sph <- left_join(sharks, reefhours)
sph2 <- sph %>% mutate(sharksperhour = sum_maxn/numsets)

# step 4 - calculate max sharks/hour per location and ave sharks/hour per location, and ratio to indicate status
sph3 <- sph2 %>% group_by(region_name, location_name, common_name, latin_name, shark_id) %>% summarise(max_sph = max(sharksperhour), ave_sph=mean(sharksperhour)) %>%
     mutate(ratio=ave_sph/max_sph)

setwd('C:/Users/tmgor/OneDrive - Dalhousie University/Chapter 3')
write.csv(sph3, 'current_status_sharkspecies.csv', row.names = F)


### more testing
a <- db %>% filter(location_name=='USA-Western Atlantic')
db %>% select(location_name) %>% distinct()
