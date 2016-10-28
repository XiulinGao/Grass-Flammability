#biomass loss rate - biomass density.R
#looking at if biomass density inlfuence maximum biomass loss rate

library(dplyr)
library(ggplot2)
library(Matrix)
library(lme4)

source('./burning_trial_summaries.R') #grab biomass loss model coefficients as 
                                      #a proxy for combustibility for each trial
source('./masspredict_massdensity.R') #grab canopy data

flamingModsCoef <- filter(flamingModsCoef, !sp.cd=="ARPU9")#drop species with one sample
#flamingModsCoef contains unequal burns compared to arcmass, which is architecture data for 
#burned plants.
#check out the difference between two datasets
missing_label <- setdiff(unique(arcmass$label), unique(flamingModsCoef$label))
#checked for all those missing trials, they are either failed trials or ignition time
#missing,I will throw those away, as well mods with positive estimate
massloss_arc <- left_join(filter(arcmass, !label %in% missing_label), flamingModsCoef)

#explore the pattern between biomass density and mass loss rate
ggplot(filter(massloss_arc, estimate<0) , aes(logden10, estimate, color=sp.cd)) + 
  geom_smooth(method='lm', se=FALSE) +
  geom_point() #ARPUL, PAAN and BRINI showed: massloss rate is negatively influenced by mass density, 
               #STNE, ERCUC4, ELLO3, BRINI and CHLA5 showed the other way

ggplot(filter(massloss_arc, estimate<0), aes(logden, estimate, color=sp.cd))+
  geom_smooth(method='lm', se=FALSE)+
  geom_point() #ARPUL and CHLA5 showed negative relationship, all the others showed positive relationship

#linear model
lossdensity10Mod <- lm(estimate ~ logden10*sp.cd, data=filter(massloss_arc, estimate<0))
anova(lossdensity10Mod) #there is species difference in mass loss rate
summary(lossdensity10Mod) 

lossdensityMod <- lm(estimate ~ logden*sp.cd, data=filter(massloss_arc, estimate<0))
anova(lossdensityMod) # both species and density matters
summary(lossdensityMod)

#how total mass influence mass loss rate
burns <- burns %>% mutate(t_mass = initial.mass-final.mass) %>%
  
