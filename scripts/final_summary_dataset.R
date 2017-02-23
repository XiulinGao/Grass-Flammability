###final_summary_dataset.R
###grab summarize for peak temp, max. biomass loss rate per trial, 
###architecture data for <10cm and >10cm sections, the create data frame contain 
###summaries for peak temp., mass loss rate and biomass density for each trial

library(dplyr)

source("./hobo-temps.R") #grab peak temp summaries for each trial:temps.sum
source("./burning_trial_summaries.R") #grab mass loss rate for each trial:
                                      #flamingModsCoef
source("./biomass_density_prediction.R")# architecture data

#throw away intervals from trials and tempsec.sum, since it 
#does not work when join trials with other dataset(can't open file),
#it is not necessary to keep it 

fix.trials <- trials[, -which(names(trials)=="interval")]
fix.tempsec.sum <- tempsec.sum[, -which(names(tempsec.sum)=="interval")]

#1. data frame has temp summary for above ane below section, as well
#as contain both flaming and smoldering biomass loss rate, trial records
#and canopy

flam.alldata <- left_join(fix.trials, fix.tempsec.sum) %>% 
  left_join(flamingModsCoef) %>% left_join(mass.density)

flam.above <- flam.alldata %>% filter(location=="above.sec")

smog.alldata <- left_join(fix.trials, fix.tempsec.sum) %>%
  left_join(smolderingModsCoef) %>% left_join(mass.density)

smog.below <- smog.alldata %>% filter(location=="below.sec")

#2.datasets only contain mass loss rate, trial records and canopy traits
#this dataset can be used for analyze how traits influence mass loss rate
#include mass loss rate for two different stages: flaming and smoldering

flam.loss <- left_join(fix.trials, flamingModsCoef) %>% 
  left_join(mass.density) %>% filter(! is.na(lossrate)) 

smog.loss <- left_join(fix.trials, smolderingModsCoef) %>%
  left_join(mass.density) %>% filter(! is.na(lossrate)) 

#3.datasets only contain temperature summary, trial records
# and canopy traits, split into sections (<10 and >10 sections) 

temp.abovesec <- left_join(fix.trials, fix.tempsec.sum) %>%
  filter(location=="above.sec") %>% left_join(mass.density)

temp.belowsec <- left_join(fix.trials, fix.tempsec.sum) %>%
  filter(location=="below.sec") %>% left_join(mass.density)

#4. split flam.alldata and smog.alldata into subsets
#for only flaming stage with available temp and mass loss  
#data for above sec;smoldering stage with available temp and
#mass loss data for below sec; 

flamtemp.above <- filter(flam.alldata, location=="above.sec") %>%
  filter(! is.na(dur)) %>% filter(! is.na(lossrate)) 


smogtemp.below <- filter(smog.alldata, location=="below.sec") %>%
  filter(! is.na(dur)) %>% filter(! is.na(lossrate)) 

#5.dataset only contains burn record and biomass density, mainly for 
#exploring how total biomass and mass density influence max. flame height

flam.height <- left_join(fix.trials, mass.density)

#6.dataset only contains flammability measures for pca, split by sections

#flamabove.pca <- flam.alldata %>% filter(location == "above.sec") %>%
  #subset(select = c("sp.cd", "label", "trial.id", "dur", "degsec",  
                    #"peak.temp", "lossrate", "max.fh")) %>% filter(!is.na(lossrate))

#flambelow.pca <- flam.alldata %>% filter(location == "below.sec") %>%
  #subset(select = c("sp.cd", "label", "trial.id", "dur", "degsec",  
                    #"peak.temp", "lossrate", "max.fh")) %>% filter(!is.na(lossrate))

#smogabove.pca <- smog.alldata %>% filter(location == "above.sec") %>%
  #subset(select = c("sp.cd", "label", "trial.id", "dur", "degsec", 
                    #"peak.temp", "lossrate", "max.fh")) %>% filter(!is.na(lossrate))

#smogbelow.pca <- smog.alldata %>% filter(location == "below.sec") %>%
  #subset(select = c("sp.cd", "label", "trial.id","dur", "degsec", 
                    #"peak.temp", "lossrate", "max.fh")) %>% filter(!is.na(lossrate))
