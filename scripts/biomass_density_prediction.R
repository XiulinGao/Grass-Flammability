###biomass_density_prediction.R

###read raw canopy architecture data and clean up to a format that can  
###contains total above ground biomass(predict), biomass density for
###10cm and >10cm sections. prediction is based on assuming plant geometry
###as a cylinder.

library(dplyr)

raw.ca <- read.csv('../data/2016-canopy-fmc.csv', stringsAsFactors=FALSE, 
                   na.strings=c("N/A",""))
canopy <- raw.ca %>% filter(!sp.cd=="ARPU9") #throw away ARPU9, 
                                             #which contained just one rep.

#un-burned 
ucanopy <- filter(canopy, treatment == 'u') %>%
  mutate(logmass10=log10(drym.10)) %>%
  mutate(logmass=log10(dry.m)) %>%
  mutate(logtiller=log10(tiller.num)) %>%  #log biomass and tiller number, fixed
  mutate(h_above10 = height-10)
 
#burned    
bcanopy <- filter(canopy, treatment == 'b') %>%
  mutate(logtiller=log10(tiller.num)) %>%
  mutate(h_above10=height-10)


#use unburned plants canopy architecture dataset to predict biomass
#for burned plants with using tiller number and height as factors.
#height is only used for above 10cm plant secntion.

#0~10cm, use only tiller number
mass10Mod <- lm(logmass10 ~ logtiller*sp.cd, data = ucanopy)
predictmass <- bcanopy %>% mutate (logmass10 = predict(mass10Mod, newdata = bcanopy))

#>10cm, use both tiller number and height
massMod <- lm(logmass ~ logtiller*sp.cd + h_above10*sp.cd, data = ucanopy)
predictmass <- predictmass %>% mutate(logmass = predict(massMod, newdata = bcanopy))



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
  mutate(vol = pi*dia^2*h_above10/4) %>%
  mutate(density10 = 10^(logmass10)/vol10) %>% mutate(logden10 = log10(density10+1)) %>%
  mutate(density = 10^(logmass)/vol)  %>% mutate(logden = log10(density +1)) 
#density is not normally distributed, log10(x+1) fixed it with avoiding negative value

#clean up arcmass by dropping data that won't be used for later analysis
mass_density <- arcmass[, c("label", "pair", "treatment", "sp.cd", "height", "h_above10", 
                          "logmass10", "logmass", "density10", "logden10", "density","logden")]
 

 
 
 

