#Maturity Figures
# load pacakges
library(dplyr)
library(tidyr)
library(ggplot2)
library(directlabels)
library(cowplot)

#load data
setwd('C:/Users/tmgor/Dropbox/Taylor Chapter 3/Predict_Maturity/Data/for model/out')
mat = read.csv('mat34_model_results.csv')
names(mat)
mat2 = mat %>% select(X, Lmax, Depth, Interbirth_interval, Amax, Litter, Offspring_size, Trophic, Amax.S, Trophic.S, K.S)

##################
#Part 1 density plots
# basic kernel density plot
ggplot(mat2, aes(x=K.S)) + geom_line(stat='density') + expand_limits(y=0)

#adjust smoothing parameter
ggplot(mat2, aes(x=K.S)) + geom_line(stat='density', adjust=0.25) + expand_limits(y=0)

#add more curves and colours to compare
ggplot(mat2, aes(x=K.S)) + geom_line(stat='density')+ 
     geom_line(stat='density', adjust=0.25, colour='green') + 
     geom_line(stat='density', adjust=2, colour='orange')

# change fill and transparency
ggplot(mat2, aes(x=Litter)) + geom_density(fill="blue", colour=NA, alpha=.2)+ geom_line(stat="density")

#rearrange dataframe so can make multiple density plots
mat3 = mat2 %>% gather(Predictor, X, Lmax, Depth, Interbirth_interval, Amax, Litter, Offspring_size, Trophic, Amax.S, Trophic.S, K.S)

ggplot(mat3, aes(x=X, fill=Predictor)) + geom_density(alpha=0.3)

# just a50 params, save figure
png('mat11_effectdensity_a50.png', width=1000, height=700)
Sparams = c('Amax.S', 'Trophic.S', 'K.S')
mat4 = mat3 %>% filter(!Predictor %in% Sparams) 
ggplot(mat4, aes(x=X, fill=Predictor)) + geom_density(alpha=0.3)
dev.off()

# just S params, save figure
png('mat11_effectdensity_S.png', width=1000, height=700)
mat5 = mat3 %>% filter(Predictor %in% Sparams)
ggplot(mat5, aes(x=X, fill=Predictor)) + geom_density(alpha=0.3)
dev.off()

#make a faceted graph, save figure
png('mat11_effectdensity_facet.png', width=1000, height=700)
ggplot(mat3, aes(x=X, fill=Predictor)) + geom_density(alpha=0.3, colour=NA) + facet_wrap(Predictor ~ .) + guides(fill=FALSE)
dev.off()


##############################################
#Part 2 ogives
og = read.csv('mat34_predicted_ogives_species.csv')
a50 = read.csv('mat34_predicteda50s_species.csv')
names(og)

# predicted maturity/year
o  =  ggplot(og, aes(x=age, y=matmu, colour=species_full)) + geom_line() #mean
o + theme(legend.text=element_text(face='italic', size='8')) + xlim(0,35)

ggplot(og, aes(x=age, y=mat25, colour=species_full)) + geom_line() #25th percentile
ggplot(og, aes(x=age, y=mat75, colour=species_full)) + geom_line() #75th percentile

#plot with error ribbons

o = ggplot(og, aes(x=age, y=matmu, ymin=mat25, ymax=mat75, fill=species_full)) + geom_line() + geom_ribbon(alpha=0.3)
o + theme(legend.text=element_text(face='italic', size='8')) + xlim(0,35)



# add species labels
species = a50$species_full
# first choose where you want labels to go 
# x axis of label goes at a50 of each species ogive
ages = a50$a50mu
# put y axis of species labels at intervals of mat values between 0.05 and 0.95
mats=seq(0.3, 0.7, length.out=25)
# put all species labels at mat value of 0.5
mats = rep(0.5, times=25)

# save this plot
png('mat34_predict_ogives_species.png', width=1500, height=700)
o + annotate('text', x=ages, y=mats, label=species, alpha=0.5) + theme(legend.text=element_text(face='italic', size='10')) + xlim(-2,35)
dev.off()


# try with direct labels
# can match colour now but now specify location of labels
ggplot(og, aes(x=age, y=matmu, colour=species_full)) + geom_line() + directlabels::geom_dl(aes(label=species_full), method="midrange")


################################################
## now try ogives with stock specific data
og = read.csv('mat34_predicted_ogives_stocks.csv')
a50 = read.csv('mat34_predicteda50s_stocks.csv')

# separate species into regions
pac = og %>% filter(Region %in% c('Northeast Pacific', 'Northwest Pacific', 'Southwest Pacific'))
atl = og %>% filter(Region %in% c('Eastern Atlantic', "Northwest Atlantic", "Southeast Atlantic",   "Southwest Atlantic", "Western Atlantic"))
ind = og %>% filter(Region %in% c("Eastern Indian", "Western Indian Ocean"))
pac50 = a50 %>% filter(Region %in% c('Northeast Pacific', 'Northwest Pacific', 'Southwest Pacific'))
atl50 = a50 %>% filter(Region %in% c('Eastern Atlantic', "Northwest Atlantic", "Southeast Atlantic",   "Southwest Atlantic", "Western Atlantic"))
ind50 = a50 %>% filter(Region %in% c("Eastern Indian", "Western Indian Ocean"))

#pacific
# set up label positions
stocks = pac50$Stock
ages = pac50$a50mu
mats=seq(0.3, 0.7, length.out=21)

png('mat34_predict_ogives_pacstocks.png', width=1200, height=700)
ggplot(pac, aes(x=age, y=matmu, colour=Stock)) + geom_line(size=1) + 
     theme(legend.text=element_text(face='italic', size='8')) + xlim(0,35) +
      annotate('text', x=ages, y=mats, label=stocks, alpha=0.7)
dev.off()    

#atlantic
# set up label positions
stocks = atl50$Stock
ages = atl50$a50mu
mats=seq(0.2, 0.8, length.out=28)

png('mat34_predict_ogives_atlstocks.png', width=1200, height=700)
ggplot(atl, aes(x=age, y=matmu, colour=Stock)) + geom_line(size=1) + 
     theme(legend.text=element_text(face='italic', size='8')) + xlim(0,35) +
     annotate('text', x=ages, y=mats, label=stocks, alpha=0.7)
dev.off()    

#indian
# set up label positions
stocks = ind50$Stock
ages = ind50$a50mu
mats=seq(0.2, 0.8, length.out=35)

png('mat34_predict_ogives_indstocks.png', width=1200, height=700)
ggplot(ind, aes(x=age, y=matmu, colour=Stock)) + geom_line(size=1) + 
     theme(legend.text=element_text(face='italic', size='8')) + xlim(0,35) +
     annotate('text', x=ages, y=mats, label=stocks, alpha=0.7)
dev.off() 

###############################################
# the short way
#ogives for wide ranging species
wid = og %>% filter(species_full %in% c("Galeocerdo cuvier", "Prionace glauca", "Carcharhinus leucas", "Carcharhinus falciformis"))
wid50 = a50 %>% filter(species_full %in% c("Galeocerdo cuvier", "Prionace glauca", "Carcharhinus leucas", "Carcharhinus falciformis"))

ggplot(mat3, aes(x=X, fill=Predictor)) + geom_density(alpha=0.3, colour=NA) + facet_wrap(Predictor ~ .) + guides(fill=FALSE)

ggplot(wid, aes(x=age, y=matmu, colour=Stock)) + 
     theme(legend.text=element_text(face='italic', size='8')) + xlim(0,35) + 
     facet_wrap(species_full)

## ...doesn't work

###########################################################
# tiger sharks
tig = og %>% filter(species_full=="Galeocerdo cuvier")
tig50 = a50 %>% filter(species_full=="Galeocerdo cuvier")
stocks = tig50$Stock
ages = tig50$a50mu
mats=seq(0.4, 0.6, length.out=5)

gc = ggplot(tig, aes(x=age, y=matmu, colour=Stock)) + geom_line(size=1) + 
     theme(legend.text=element_text(face='italic', size='8')) + xlim(0,35) +
     annotate('text', x=ages, y=mats, label=stocks, alpha=0.7)

# blue sharks
blu = og %>% filter(species_full=="Prionace glauca")
blu50 = a50 %>% filter(species_full=="Prionace glauca")
stocks = blu50$Stock
ages = blu50$a50mu
mats=seq(0.4, 0.6, length.out=8)

pg = ggplot(blu, aes(x=age, y=matmu, colour=Stock)) + geom_line(size=1) + 
     theme(legend.text=element_text(face='italic', size='8')) + xlim(0,20) +
     annotate('text', x=ages, y=mats, label=stocks, alpha=0.7)


# bull sharks
bull = og %>% filter(species_full=="Carcharhinus leucas")
bull50 = a50 %>% filter(species_full=="Carcharhinus leucas")
stocks = bull50$Stock
ages = bull50$a50mu
mats=seq(0.4, 0.6, length.out=5)


cl = ggplot(bull, aes(x=age, y=matmu, colour=Stock)) + geom_line(size=1) + 
     theme(legend.text=element_text(face='italic', size='8')) + xlim(0,20) +
     annotate('text', x=ages, y=mats, label=stocks, alpha=0.7)


# silky sharks
sil = og %>% filter(species_full=="Carcharhinus falciformis")
sil50 = a50 %>% filter(species_full=="Carcharhinus falciformis")
stocks = sil50$Stock
ages = sil50$a50mu
mats=seq(0.4, 0.6, length.out=6)

cf = ggplot(sil, aes(x=age, y=matmu, colour=Stock)) + geom_line(size=1) + 
     theme(legend.text=element_text(face='italic', size='8')) + xlim(0,20) +
     annotate('text', x=ages, y=mats, label=stocks, alpha=0.7)

#plot all at once
png('mat34_predict_ogives_widerangers.png', width=1500, height=700)
plot_grid(gc, pg, cl, cf)
dev.off()

####################################
### facet plot for all stocks
png('mat34_predict_ogives_facetstocks.png', width=1500, height=1000)
ggplot(og, aes(x=age, y=matmu, ymin=mat25, ymax=mat75, fill=Stock)) + 
     geom_line() + geom_ribbon(alpha=0.3) + 
     theme(legend.text=element_text(face='italic', size='8')) + xlim(0,35) +
     facet_wrap(Stock ~ .) + guides(fill=FALSE)
dev.off()


########################################################################
#Part 3 predicted vs observed
# load data
library(readxl)
pa50s <- read.csv('mat34_predicteda50s_species.csv')
pss <- read.csv('mat34_predictedSs_species.csv')
setwd('C:/Users/tmgor/Dropbox/Taylor Chapter 3/Predict_Maturity/Data/for model/in')
obs <- read_excel("predict_car_traits.xlsx", sheet="Sheet3")
 
p2 <- left_join(pa50s, obs)

setwd('C:/Users/tmgor/Dropbox/Taylor Chapter 3/Predict_Maturity/Data/for model/out')
png('mat34_predict_a50s.png', width=1500, height=700)
ggplot(data=p2, aes(x=species_full, fill=species_full)) + 
     geom_boxplot(aes(ymin=a505, ymax=a5095, middle=a50mu, upper=a5075, lower=a5025), stat='identity') +
     geom_point(aes(y=amat, size=6)) + theme(legend.position = "none") +
     coord_flip()  + guides(fill=FALSE) +
     ylab("age at maturity") + xlab("")
dev.off()

# make teh same plot with dots instead 
#gather data so can put in dot plot
pa50s2 <- gather(pa50s, perc, pa50, a50mu, a505, a5025, a5075, a5095)
# add observed age of maturity
p3 <- left_join(pa50s2, obs)

ggplot(p3, aes(y=species_full)) + geom_point(aes(x=pa50, colour=species_full, size=6)) + 
     geom_point(aes(x=amat, size=6)) + 
     theme(legend.position = "none") +
     ylab("") + xlab("age at maturity")
