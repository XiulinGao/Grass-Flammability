###final_summary_dataset.R
###grab summarize for peak temp, max. biomass loss rate per trial, 
###architecture data for <10cm and >10cm sections, the create data frame contain 
###summaries for peak temp., mass loss rate and biomass density for each trial

library(dplyr)
source("./hobo-temps.R") #grab peak temp summaries for each trial:temps.sum
source("./burning_trial_summaries.R") #grab mass loss rate for each trial:
                                      #flamlossr
source("./biomass_density_prediction.R")# architecture data

#throw away intervals from trials and tempsec.sum, since it 
#does not work when join trials with other dataset(can't open file),
#it is not necessary to keep it 

trials <- select(trials, -interval)
trials.orga <- trials[order(trials$label), ]
tempsec.sum <- select(tempsec.sum, -interval) 

#1. data frame has temp summary for above ane below section, trial records
#and canopy traits

temp.alldata <- left_join(trials, mass.density) %>% 
left_join(tempsec.sum)

temp.above <- temp.alldata %>% filter(location=="above.sec") %>%
  mutate(log.tmass=log(total.mass))

temp.base <- temp.alldata %>% filter(location=="base") %>%
  mutate(log.tmass=log(total.mass))

#smog.alldata <- left_join(fix.trials, fix.tempsec.sum) %>%
  #left_join(smolderingModsCoef) %>% left_join(mass.density)

#smog.below <- smog.alldata %>% filter(location=="below.sec")

#2.datasets only contain mass loss rate, trial records and canopy traits
#this dataset can be used for analyze how traits influence mass loss rate
#include mass loss rate for two different stages: flaming and smoldering

flam.loss <- left_join(trials, mass.density) %>% 
  left_join(flamlossr) %>% filter(! is.na(lossrate)) 

#smog.loss <- left_join(fix.trials, smolderingModsCoef) %>%
  #left_join(mass.density) %>% filter(! is.na(lossrate)) 


#3.dataset only contains burn record and biomass density, mainly for 
#exploring how total biomass and mass density influence max. flame height

arc.trial <- left_join(trials, mass.density)

#4.missing data occured for different measuremetns of different trials,
# PCA need two seperate datasets which contains all obtained flammability measurements
# but only temp. measurements at either base or above soil surface 

pcadata.above <- left_join(trials, flam.loss) %>% left_join(temp.above) %>%
  select(mconsum, massloss, degsec, dur, lossrate, max.fh, ignition, combustion)

pcadata.base <- left_join(trials, flam.loss) %>% left_join(temp.base) %>%
  select(mconsum, massloss, degsec, dur,lossrate, max.fh, ignition, combustion)

#clean up dataset not needed
rm("balance_data", "balance_sum", "bytrial", "flamburn", "flambytrial", 
   "trials")
