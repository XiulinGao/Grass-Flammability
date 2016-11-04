###final_summary_dataset.R
###grab summarize for peak temp, max. biomass loss rate per trial, 
###architecture data for <10cm and >10cm sections, the create data frame contain 
###summaries for peak temp., mass loss rate and biomass density for each trial

library(dplyr)

source("./hobo-temps.R") #grab peak temp summaries for each trial:temps.sum
source("./burning_trial_summaries.R") #grab mass loss rate for each trial:
                                      #flamingModsCoef
source("./biomass_density_prediction.R")# architecture data

all_burns <- left_join(temps.sum, flamingModsCoef) %>%
   left_join(mass_density)

#filter trials that actually have positive biomass 'loss' rate
#and log total mass
all_burns <- all_burns %>% filter(estimate<0) %>%
  mutate(logtmass=log10(total.mass))

temp_balance <- filter(all_burns, !is.na(peak.temp))#throw away dataset with no 
#temperature data 

#subset data for sec <10 and >10, mainly is used for temp data
temp10 <- filter(temp_balance, location %in% c('base', 'height.10')) 
temp <- filter(temp_balance, location %in% c('height.20', 'height.40'))





  






