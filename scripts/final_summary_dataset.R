## final_summary_dataset.R

## grab summarize for peak temp, max. biomass loss rate per trial, architecture
## data for <10cm and >10cm sections, the create data frame contain summaries
## for peak temp., mass loss rate and biomass density for each trial.

source("./burning_trial_summaries.R") # grab mass loss rate for each trial:
                                      # flamlossr and hobo thermocouple data.
                                      # Produces trials dataframe.
source("./biomass_density_prediction.R")# architecture data

#throw away intervals from trials and tempsec.sum, since it 
#does not work when join trials with other dataset(can't open file),
#it is not necessary to keep it

## trials <- select(trials, -interval)
## temps.sum <- select(temps.sum, -interval) 

trials <- trials[order(trials$label), ]

# 1. data frame has temp summary for above ane below section, trial records and
#canopy traits

alldata <- left_join(trials, mass.density,by="label") %>% 
  left_join(temps.sum)  %>% ##  join by trial.id only!
  mutate(log.tmass=log(total.mass))

## > nrow(temp.alldata)
## [1] 124

## add in lossrate data
alldata <- left_join(alldata, select(flamlossr, label, lossrate), by="label")


#4.missing data occured for different measuremetns of different trials,
# PCA need two seperate datasets which contains all obtained flammability measurements
# but only temp. measurements at either base or above soil surface 

## DWS: But now these lack ability to merge with species identity, etc. Don't do
## this here. If you need to select out a few columns do it RIGHT BY the PCA
## code. 

## pcadata.above <- left_join(trials, flam.loss) %>% left_join(temp.above) %>%
##   select(mconsum, massloss, degsec, dur, lossrate, max.fh, ignition, combustion)

## pcadata.base <- left_join(trials, flam.loss) %>% left_join(temp.base) %>%
##   select(mconsum, massloss, degsec, dur,lossrate, max.fh, ignition, combustion)

#clean up dataset not needed
## DWS: GOOD! BUt also document what you do export.
rm("balance_data", "balance_sum", "bytrial", "flamburn", "flambytrial")
