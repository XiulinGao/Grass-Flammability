
library(ggplot2)
library(dplyr)
library(Matrix)
library(lme4)

source('./masspredict_massdensity.R') #grab canopy data
source('./hobo-temps.R') # grab temperature data 

#due to duplication for 2 trials on 2016/08/03, I will drop 
#the first record for each, since the first one usually is
# the failed trial with incomplete records.



peaktemp.sum <- anti_join(temps.sum, drop) 
peaktemp.sum <- peaktemp.sum[order(peaktemp.sum$trial.id, peaktemp.sum$location), ]#organize 

# then calculate above ground biomass for each burn trial

peaktemp.sum$t_mass <- peaktemp.sum$initial.mass - peaktemp.sum$final.mass

peaktemp.sum$logtmass <- sapply(peaktemp.sum$t_mass, log10)#log transfer fixed non-normal distribution 

#1.1 explore how biomass influence peak temperature at each location

ggplot(peaktemp.sum, aes(logtmass, peak.temp)) + geom_smooth(method='lm') +
  geom_point() + facet_wrap(~sp.cd) 
#take a look at the pattern: generally, total mass positively influence peak temp.
#species AGSM, BRINI, ELLO3 and PAAN have relatively stronger response to biomass
# by peak temp.; and ARPUL, CHLA5, ERCUC4 and STNE2 have weaker response to biomass by peak temp. 
# this may be due to variance in total biomass within species. 

#1.2 linear mixed model to look at how total above ground biomass influence peak temps

#exclude trial on 2016/7/28 because no temp data obtained from that day 

masstempMod <- lmer(peak.temp ~ logtmass*sp.cd + (1 + logtmass|label) + (1 + logtmass|location), 
                    data = filter(peaktemp.sum,!trial.date=='7/28/16'), REML=FALSE)
masstempNullMod <- lmer(peak.temp ~ sp.cd + (1 + logtmass|location) + (1 + logtmass|label),
                        data = filter(peaktemp.sum,!trial.date=='7/28/16'), REML=FALSE)
anova(masstempMod, masstempNullMod) #total biomass matters for peak temp.
summary(masstempMod) 

#t_mass has significant influence on peak temp. with agsm as the reference(-275.19),
# ARPUL(slope:316.13), ERCUC4(slope:591.69) and STNE2 (slope:644.49) have the relatively  
# high mean peak temp.,CHLA5(237.39) and ELLO3(177.15), whcih have relatively lower mean peak temp.  
# BRINI(50.42)and PAAN(43.32) have lowest mean peak temp.; Generally, total mass
# has positive infuence on peak temp. with agsm has strongest response(452.42) to total mass 
#in terms of peak temp, which BRINI(-36.66) is pretty similar to.ERCUC4(--384.50),STNE2(-430.98)  
#have the weakest response.  
#and others are:ARPUL(-231.74), CHLA5(-246.95), ELLO3(-178.31) AND PAAN(-144.16)
#so, it looks like species have relatively higher peak.temp actually have weaker
#influence on temp by biomass, and species have relatively lower peak temp have stronger biomass
#influence. difference for AGSM and BRINI, ARPUL and CHLA5, ELLO3 and PAAN, ERCUC4 and STNE2 are 
# more similar than to others.
#therefor, I think biomass is not the only factor influence peak temp. and there should  
# be shreshold for biomass which may associate with allocation pattern: species with biomass below 
# the threshold may reponse more in peak temp. once there is enough biomass, species
# may not response too much in peak temp even biomass increase. 


#2.1 explore how biomass density influence peak temp at 10cm and 40cm(which is associated 
# with my biological questions)

#creat data frame which contains both temperature and biomass density
#full_join arcmass with peaktemp.sum data

tempsum.dens <- full_join(peaktemp.sum, arcmass)

#since peak temp. data is missing for burns on 2016/07/28, exclude this portion of data
tempsum.dens <- tempsum.dens %>% filter(!trial.date=='7/28/16')


 
ggplot(filter(tempsum.dens, location == 'base.B'), aes(logden10, peak.temp, color=sp.cd)) + 
       geom_smooth(method='lm', se=FALSE) + 
       geom_point() 
#except AGSM, CHLA5 and PAAN, all other species showed positive relationship between biomass
#density and peak temp at base B

ggplot(filter(tempsum.dens, location == "height.40"), aes(logden, peak.temp, color = sp.cd))+
  geom_smooth(method='lm', se=FALSE) + 
  geom_point()

#>10cm, BRINI, ARPUL and PAAN show positive relationship between biomass density and peak temp
# at 40cm, however, AGSM, ERCUC4, STNE2, ELLO3 and CHLA5 showed negative relationship.pattern is 
#not constant

#2.2 linear model to look at if biomass density influence peak temp at base B

temparc10Mod <- lm(peak.temp ~ logden10*sp.cd,
                   data = filter(tempsum.dens, location == 'base.B'))
anova(temparc10Mod) #density, species and interaction between the two have sig. inlfuence
summary(temparc10Mod) #AGSM, ELLO3(highest) have higher mean base peak temp.,with STNE2,ERCUC4 similar to 
#AGSM; CHLA5 and PAAN have lowest base temp.(which is opposite to hypothesis). 

#>10cm

temparcMod <- lm(peak.temp ~ logden*sp.cd,
                 data = filter(tempsum.dens, location == "height.40"))
anova(temparcMod) #density does not have influence on peak temp at 40cm, but there are species and 
                  # species*density effects
summary(temparcMod)


