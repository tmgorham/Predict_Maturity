#Maturity Figures
# load pacakges
library(dplyr)
library(tidyr)
library(ggplot2)

#load data
setwd('C:/Users/tmgor/Dropbox/Taylor Chapter 3/Predict_Maturity/Data/for model/out')
mat = read.csv('mat11_model_results.csv')
names(mat)
mat2 = mat %>% select(X, Lmax, Depth, Interbirth_interval, Amax, Litter, Offspring_size, Trophic, Amax.S, Trophic.S, K.S)

###################Part 1 density plots
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

#makea faceted graph, save figure
png('mat11_effectdensity_facet.png', width=1000, height=700)
ggplot(mat3, aes(x=X, fill=Predictor)) + geom_density(alpha=0.3, colour=NA) + facet_wrap(Predictor ~ .) + guides(fill=FALSE)
dev.off()


##############################################
#Part 2 ogives
og = read.csv('predicted_ogives.csv')
a50 = read.csv('mat11_predicteda50s.csv')
names(og)

# predicted maturity/year
ggplot(og, aes(x=age, y=matmu, colour=species_full)) + geom_line() #mean
ggplot(og, aes(x=age, y=mat25, colour=species_full)) + geom_line() #25th percentile
ggplot(og, aes(x=age, y=mat75, colour=species_full)) + geom_line() #75th percentile

#plot with error ribbons

o = ggplot(og, aes(x=age, y=matmu, ymin=mat25, ymax=mat75, fill=species_full)) + geom_line() + geom_ribbon(alpha=0.3)
o + theme(legend.text=element_text(face='italic', size='8')) + xlim(0,35)


#use theme to move legend around
o = ggplot(og, aes(x=age, y=matmu, colour=species_full)) + geom_line()
o + theme(legend.position='bottom')


# add species labels
species = a50$species_full
ages = a50$a50mu
mats=seq(0.05, 0.95, length.out=29)
# save this plot
png('mat11_predict_ogives.png', width=1500, height=700)
o + annotate('text', x=ages, y=mats, label=species)
dev.off()

#Part 3 predicted vs observed
# load data
pa50s <- read.csv('mat18_predicteda50s.csv')
pss <- read.csv('mat18_predictedSs.csv')
setwd('C:/Users/tmgor/Dropbox/Taylor Chapter 3/Predict_Maturity/Data/for model/in')
obs <- read.csv('tmp_car_traits.csv')

#gather data so can put in dot plot
pa50s2 <- gather(pa50s, perc, pa50, a50mu, a505, a5025, a5075, a5095)
# add observed ages
p3 <- left_join(pa50s2, obs, by=c("species_full"="ï..species_full"))

a <- ggplot(p3, aes(x=pa50, y=species_full, colour=species_full)) + geom_point() 
a + theme(legend.position = "none")

ggplot(pa50s, aes(x=a505, y=species_full)) + geom_point()

setwd('C:/Users/tmgor/Dropbox/Taylor Chapter 3/Predict_Maturity/Data/for model/out')
png('mat18_predict_a50s.png', width=1500, height=700)
b <- ggplot(p3, aes(y=species_full)) + geom_point(aes(x=pa50, colour=species_full, size=6)) + geom_point(aes(x=a50, size=6))
b + theme(legend.position = "none")
dev.off()
