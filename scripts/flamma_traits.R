###flamma_traits.R

library(stats)
library(Matrix)
library(lme4)
source('./final_summary_dataset.R') 

#dataset explanation
#get the all summaries dataset:all_burns
#which contains all mass loss rate, all hobo data summaries, so it actually
#contains trials that with failed hobo data collection but well worked balance
#data.a sub-dataset from all burns which throws away trials don't have temperature
#data:temp_balance, and two sub-datasets from temp_balance: temp10 is for section
#<10cm, and temp is for section >10cm, thus for differnt analysis, may use
#different sub-dataset or the whole dataset. 

#linear mixed model to see if total biomass has influence on mass loss rate
#sec<10cm

tmloss10FullMod <- lmer(estimate ~ logtmass*sp.cd*location + density10*sp.cd*location + 
                          (1 | label), data=all_burns ) 
#Error in fn(x, ...) : Downdated VtV is not positive definite

#ok, linear mixed model to see is total mass, mass density have influence on peak temp 

#sec<10
#if total biomass matters?
peaktemp10FullMod <- lmer(peak.temp ~ logtmass*sp.cd*location + density10*sp.cd*location +
                         (1 | label), data=temp10, REML= FALSE)

tmass10NullMod <- lmer(peak.temp ~  density10*sp.cd*location + (1 | label), 
                       data=temp10, REML=FALSE)
anova(peaktemp10FullMod, tmass10NullMod) #so total biomass matters

#if density10 matters?
density10NullMod <- lmer(peak.temp ~ logtmass*location*sp.cd + 
                           (1 | label), data=temp10, REML=FALSE)
anova(peaktemp10FullMod, density10NullMod) #so density10 does not matter
# if location matters?
location10NullMod <- lmer(peak.temp ~ logtmass*sp.cd + density10*sp.cd +
                            (1 | label), data=temp10, REML=FALSE)
anova(peaktemp10FullMod, location10NullMod) #location matters
#sec>10
#for peak.temp: does total biomass matters?
peaktempFullMod <- lmer(peak.temp ~ logtmass*sp.cd*location + density*sp.cd*location + 
                           (1 | label), data=temp, REML=FALSE)
tmassNullMod <- lmer(peak.temp ~  density*sp.cd*location + (1 | label), 
                     data=temp, REML=FALSE)
#both get warning:Some predictor variables are on very different scales: consider rescaling 
anova(peaktempFullMod, tmassNullMod)#total biomass matters

#for peak.temp: does mass density matters?
densityNullMod <- lmer(peak.temp ~  logtmass*sp.cd*location + (1 | label), 
                       data=temp, REML=FALSE)
anova(peaktempFullMod, densityNullMod) #density at >10cm section matters

#does locaton matter?
locationNullMod <- lmer(peak.temp ~ density*sp.cd + logtmass*sp.cd +
                          (1 | label), data=temp, REML=FALSE)
anova(peaktempFullMod, locationNullMod) #location matters



