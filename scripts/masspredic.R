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

canopy <- raw.ca %>% mutate(vol10 = pi*dia10*dia10*10) %>%
  mutate(vol = pi*dia*dia*(height-10))
  

ucanopy <- filter(canopy, treatment == 'u')
bcanopy <- filter(canopy, treatment == 'b')


#use unburned plants as a whole dataset to build biomass predict model on tiller number
#and height. height is just used for biomass above 10cm

#0~10cm
massMod10 <- lm(log10 ~ logtiller*sp.cd, data = ucanopy)
predictb <- bcanopy %>% mutate (logmass10 = predict(massMod10, newdata = bcanopy))

#>10cm, use both tiller number and height
massMod11 <- lm(log(dry.m) ~ log(tiller.num) + log(height-10), data = ucanopy)
predictb <- predictb %>% mutate(logmass11 = predict(massMod11, newdata = bcanopy))
predictb <- predictb %>% mutate(drym.10 = exp(logmass10)) %>%
  mutate(dry.m = exp(logmass11))
predictb <- predictb[, -c(5:8, 13:18, 20:27)]

#calculate biomass/volume at 0~10 and >10 intervals assuming 
#canopy geometry as cylinder

cylinderca <- predictb %>% mutate(fuelbed10 = logmass10/vol10)

cylinderca <- cylinderca[, -c(12:13)]

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
tempca10.mod <- lm(log(peak.temp) ~ fuelbed10*sp.cd,
                     data = subset(tempca, location %in% c('base.B', 'base.A', 'height.10')))
tempca10.mod.null <- lmer(log(peak.temp) ~ log(fuelbed10) + (1 + log(fuelbed10)|label),
                         data = subset(tempca, location == c('base.B', 'base.A', 'height.10')))

anova(tempca10.mod.null, tempca10.mod)
summary(tempca10.mod) #problem: seems need to rescale variable

#>10cm
#linear mixed model can't work here, getting error
#"numbers of observations(=106) <= numbers of random effects(=130) for term
#(1 + fuelbed|label)"

 tempca.mod <- lmer(log(peak.temp) ~ log(fuelbed)*sp.cd + (1 + log(fuelbed)|label),
                   data = subset(tempca, location == c('height.20', 'height.40')))
 tempca.mod.null <- lmer(log(peak.temp) ~ log(fuelbed) + (1 + log(fuelbed)|label),
                        data = subset(tempca, location == c('height.20', 'height.40')))

#linear model for >10cm

byspecies <- tempca %>% group_by(sp.cd)
masscaMods <- byspecies %>% subset(location %in% c('height.20', 'height40')) %>%
  do(masscaMod = lm(log(peak.temp) ~ fuelbed, data = .))

# use broom::tidy to grab coefficents
masscaModsCoef <- tidy(masscaMods, masscaMod) %>% filter(term=='fuelbed')

ggplot(masscaModsCoef, aes(sp.cd, estimate)) + geom_point()

#2.if mass/volume influences mass loss rate?




 
 
 

