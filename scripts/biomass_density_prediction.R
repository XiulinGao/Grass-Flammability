###biomass_density_prediction.R

###read raw canopy architecture data and clean up to a format that can  
###contains total above ground biomass(predict), biomass density for
###10cm and >10cm sections. prediction is based on assuming plant geometry
###as a cylinder.

library(dplyr)
source("./clean_up_trial.R")

raw.ca <- read.csv('../data/2016-canopy-fmc.csv', stringsAsFactors=FALSE, 
                   na.strings=c("N/A",""))
canopy <- raw.ca %>% filter(!sp.cd=="ARPU9") #throw away ARPU9, 
                                             #which contained just one rep.

#un-burned 
ucanopy <- filter(canopy, treatment == 'u') %>%
  mutate(h.above10 = height-10) %>% mutate(total.mass=drym.10+dry.m)
 
#burned    
bcanopy <- filter(canopy, treatment == 'b') %>%
  mutate(h.above10=height-10) %>% left_join(trials)
bcanopy <- bcanopy[, c(1:26, 42)]


#use unburned plants canopy architecture dataset to predict biomass
#for burned plants with using tiller number, height, sp.cd and total 
#biomass as prediction factor. 

#0~10cm, use only tiller number

mass10LM <- lm(drym.10 ~ tiller.num + total.mass*sp.cd, data = ucanopy)
summary(mass10LM)
predictmass <- bcanopy %>% mutate (mass10 = predict(mass10LM, newdata = bcanopy))

#>10cm, use both tiller number and height

massLM <- lm(dry.m ~ tiller.num + h.above10*sp.cd + total.mass*sp.cd, data = ucanopy)
summary(massLM)

predictmass <- predictmass %>% mutate(mass = predict(massLM, newdata = bcanopy))

#then calculate biomass density for both sections, is mass density for whole plant 
#needed?

#assume plant canopy geomerty as a cylinder, calculate diameter for 
#below and above 10cm section
predictmass $ dia10 <- rowMeans(subset(predictmass, select = c(wda10.1, wda10.2, wda10.3,
                                                               wdb10.1, wdb10.2, wdb10.3)),
                                                               na.rm = TRUE)
predictmass $ dia <- rowMeans(subset(predictmass, select = c(wda10.3, wda.1, wda.2, 
                                                             wda.3, wdb10.3,wdb.1, 
                                                             wdb.2, wdb.3)), na.rm = TRUE)
#calculate biomass density
arcmass <- predictmass %>% mutate(vol10 = pi*dia10^2 *10/4) %>%
  mutate(vol = pi*dia^2*h.above10/4) %>%
  mutate(tvol = 1/3*pi*height*(dia*dia+dia10*dia10+dia10*dia)) %>%
  mutate(density10 = mass10/vol10) %>%
  mutate(density = mass/vol)  %>% 
  mutate(mratio = mass/mass10) %>%
  mutate(tdensity = total.mass/tvol)
  
  
  


#clean up arcmass by dropping data that won't be used for later analysis
mass.density <- arcmass[, c("label", "pair", "treatment", "sp.cd", "height", "h.above10", 
                          "mass10", "mass","mratio",
                          "density10", "density", "tdensity")]
 

 
 
 

