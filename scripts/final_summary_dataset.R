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
all_burns <- all_burns %>% filter(estimate<0)

# find all_burns arr missing some canopy data,which should not happen
# since canopy data should be measred for every single trial,check it out.
#cause is wrong data entering for pair of erc29 and species code of cs39
#, thus when they are joined by pair and species code, it gets lost. fixed


  






