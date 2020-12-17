#create covariate dataset for modelling life history traits in Carcharhinids
#sources:
#    fishbase
#    Pardo et al 2016
#    Beukhof et al 2019

#open packages
library(rfishbase)
library(dplyr)
library(readxl)

################    get fishbase data
# set fishbase envr
Sys.setenv(FISHBASE_API="fishbase")
#get a list of carcharhinid species
car <- species_list(Family="Carcharhinidae")
#get a list of Carcharhinid common names
common_car <- common_names(car)
c1 <- common_car %>% filter(Language=="English")

#save common names
setwd("C:/Users/tmgor/Dropbox/Taylor Chapter 3/Data/source data")
write.csv(c1, "car_common_names.csv", row.names=F)

#get trophic position
#take highest value per species & life stage
troph <- diet(species_list=car, fields=c("Species", "Troph", "SampleStage")) %>% 
     group_by(Species, SampleStage) %>%
     summarise(Troph=max(Troph))
# adult trophic position
troph_ad <- troph %>% filter(SampleStage %in% c("adults", "juv./adults")) %>% 
     rename(adult_trophic=Troph)
#juvenile trophic position
troph_juv <- troph %>% filter(SampleStage %in% c("recruits/juv.")) %>% 
     rename(juv_trophic=Troph)

#get depth range and habitat type
homepage <- species(species_list=car, fields=c("Species", "FBname", "DemersPelag", "DepthRangeShallow", "DepthRangeDeep"))
t <- species(car)
names(t)
species("Carcharhinus acronotus")


species(species_list=car, fields=c("Species", "DemersPelag"))
#join data together
c2 <- left_join(homepage, troph_ad)

#what fields are in homepage?
homepage <- species(species_list=car)
names(homepage)
species('Carcharodon carcharias', fields=c("Species", "FBname", "DemersPelag", "DepthRangeShallow", "DepthRangeDeep"))

hp2 <- homepage %>% select(Species, FBname, DemersPelag,DepthRangeShallow, DepthRangeDeep, LongevityWild, Length, CommonLength, Weight, LifeCycle)



## list all fields
List_fb_fields <- list_fields()
ls_fields_uniq <- List_fb_fields %>% select(columns) %>% unique()
ls_tables <- List_fb_fields %>% select(table) %>% unique()

#look at fecundity table
fecundity <- fecundity(species_list=car)
write.csv(fecundity, 'fishbase_fecundity.csv', row.names = F)


#look at maturity table
maturity <- maturity(species_list=car)
names(maturity)
m2 <- maturity %>% filter(is.na(AgeMatMin)==F)

#list of stocks and stock codes
stockcodes <- stocks(species_list=car, fields=c("Species", "StockCode", "StockDefs", "StockDefsGeneral", "LocalUnique"))

# look at popchar table
popchar <- popchar(species_list=car)
write.csv(popchar, "fishbase_popchar.csv", row.names=F)


#################    upload Pardo dataa
setwd("C:/Users/tmgor/OneDrive - Dalhousie University/Chapter 3/Pardo et al 2016")
list.files()
rmax_pardo <- read.csv("updated_rmax_data.csv")
lifehist_pardo <- read.csv("ChondroRmax140429.csv")

c3 <- left_join(c2, rmax_pardo)
c4 <- left_join(c3, lifehist_pardo)

##################    upload Beukhof data
setwd("C:/Users/tmgor/OneDrive - Dalhousie University/Chapter 3")
list.files()
beukhof <- read_excel("Beukhof Trait Collection.xlsx" , sheet="Trait values")

#beukhof data is structured by LME, have to reduce duplication or match stocks for this to work

c5 <- left_join(c4, beukhof, by=c("Species"="taxon"))

write.csv(c4, "car_trait_covariates.csv", row.names=F)
