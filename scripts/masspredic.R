#read raw canopy architecture data and clean up to a format that can be 
#directly used for model building and biomass prediction for burned
#plants

library(dplyr)
library(broom)
library(ggplot2)
library(lme4)
#assume plant geomerty as cylinder, calculate volume 
raw.ca <- read.csv('../data/2016-canopy-fmc.csv', stringsAsFactors=FALSE, na.strings="N/A")
raw.ca $ dia10 <- rowMeans(subset(raw.ca, select = c(wda10.1, wda10.2, wda10.3, wdb10.1, 
                                                     wdb10.2, wdb10.3)),na.rm = TRUE)
raw.ca $ dia <- rowMeans(subset(raw.ca, select = c(wda10.3, wda.1, wda.2, wda.3, wdb10.3,
                                                   wdb.1, wdb.2, wdb.3)), na.rm = TRUE)

canopy <- raw.ca %>% filter(!sp.cd=="ARPU9") %>%
  mutate(vol10 = pi*dia10*dia10*10) %>%
  mutate(vol = pi*dia*dia*(height-10))
  
  ucanopy <- filter(canopy, treatment == 'u') %>%
  mutate(logmass10=log10(drym.10)) %>%
  mutate(logtiller=log10(tiller.num)) %>%
    mutate(logmass=log10(dry.m))
    

bcanopy <- filter(canopy, treatment == 'b') %>%
  mutate(logtiller=log10(tiller.num))


#use unburned plants canopy architecture dataset to predict biomass
#for burned plants with using tiller number and height as factors.
#height is only used for above 10cm plant secntion.

#0~10cm
mass10Mod <- lm(logmass10 ~ logtiller*sp.cd, data = ucanopy)
predictb <- bcanopy %>% mutate (logmass10 = predict(mass10Mod, newdata = bcanopy))

#>10cm, use both tiller number and height
massMod <- lm(logmass ~ logtiller*sp.cd + height, data = ucanopy)
predictb <- predictb %>% mutate(logmass = predict(massMod, newdata = bcanopy))

#calculate biomass/volume at 0~10 and >10 sections  with assuming 
#canopy as in cylinder shape.

cylinderca <- predictb %>% mutate(massdensity10 = logmass10/vol10) %>%
  mutate(massdensity = logmass/vol)

#full_join canopy data with temps.sum data

 source("./hobo-temps.R")


#due to duplication for 2 trials on 2016/08/03, I will drop 
#the first record for each, since the first one usually is
# the failed trial with incomplete records.

#find the duplicated trial id
n1 <- trials$trial.date[duplicated(trials$label)]
n2 <- trials$trial.num[duplicated(trials$label)] -1 #the one needs to be droped is 
                                                  # before this duplicated one
dropid <- paste(n1, n2, sep="_")
#drop the rows of duplicated trials, extremly ugly code i know....
drop <- temps.sum %>% subset(trial.id == dropid[1]) %>%
  full_join(subset(temps.sum, trial.id == dropid[2]))

# combine canopy data with hobo summary 
tempsum.ca <- temps.sum %>% anti_join(drop) %>% group_by(label, trial.id) %>%
  full_join(cylinderca) %>% arrange(trial.id)

#since hobo data is missing for trial on 2016/07/28, exclude this portion of data
tempca <- tempsum.ca %>% filter(!trial.date=='7/28/16')

#1.if mass/volume character of each section influences 
# peak temp at corresponding section

#0~10cm
tempca10.mod <- lm(log(peak.temp) ~ massdensity10*sp.cd,
                     data = filter(tempca, location %in% c('base.B', 'base.A', 'height.10')))

summary(tempca10.mod) #problem: seems need to rescale variable

#>10cm
#linear mixed model can't work here, getting error
#"numbers of observations(=106) <= numbers of random effects(=130) for term
#(1 + fuelbed|label)"

 tempca.mod <- lm(log(peak.temp) ~ massdensity*sp.cd,
                   data = filter(tempca, location %in% c('height.20', 'height.40')))
 summary(tempca.mod)
 anova(tempca.mod)
 
#2.totall biomass density influence ?
 tolmassca <- tempca %>% mutate (logtolm = log10(10^logmass10 + 10^logmass)) %>%
   mutate(tvol = vol+vol10) %>% mutate(tbd = logtolm/tvol) 
 
 tmassMod <- lm(log(peak.temp) ~ tbd*sp.cd, data = tolmassca)
 summary(tmassMod)
 
 
 

 
 
 

