###flammability_species_variation.R
###explore if there is species variation in measured
###flammability when consider biomass influence

source("./final_summary_dataset.R")

#Approach 1: build full model with adding all potential factors 
#that may influence flammability, then build null model with
#taking out interested factor, anova to compare null and full
#model to see if there is significant difference between two models

#1.species variation in dur ?

#build models seperatly for >10 and <10 sections
#>10

durFullMod <- lm( dur ~ logtmass*sp.cd + density*sp.cd, 
                  data = temp.abovesec)
durSpNull <- lm( dur ~ logtmass + density, data = temp.abovesec)

anova(durFullMod, durSpNull) #no species variation

#<10

dur10FullMod <- lm(dur ~ logtmass*sp.cd + density10*sp.cd,
                   data = temp.belowsec)
dur10SpNull <- lm(dur ~ logtmass + density10, data = temp.belowsec)
anova(dur10FullMod, dur10SpNull) #there is species variation in
                                  #duration at <10 section
summary(dur10FullMod)
#So, species variation in duration only exist at section below 10cm,
#no speceis variation in duration at section above 10cm

#2. species variation in degsec ?

#>10

degFullMod <- lm(degsec ~ logtmass*sp.cd + density*sp.cd,
                 data = temp.abovesec)
degSpNull <- lm(degsec ~ logtmass + density, data = temp.abovesec)
anova(degFullMod, degSpNull) #there is species variation in
#total degree of temperature(which is >60) produced at >10 section
summary(degFullMod)

#<10

deg10FullMod <- lm(degsec ~ logtmass*sp.cd + density10*sp.cd,
                   data = temp.belowsec)
deg10SpNull <- lm(degsec ~ logtmass + density10, data= temp.belowsec)
anova(deg10FullMod, deg10SpNull) #there is species variation in degsec
#at <10 section
summary(deg10FullMod)

#3. species variation in maximum flame height?

flamhFullMod <- lm(max.fh ~ logtmass*sp.cd + density10*sp.cd +
                     density*sp.cd + height*sp.cd, data=flam.height)
flamhSpNull <- lm(max.fh ~ logtmass + density10 + density + height, 
                   data=flam.height)
anova(flamhFullMod, flamhSpNull) #there is no species variation in 
#maximum flame height produced


#4. species variation in mass loss rate?
#flaming biomass loss 
flossFullMod <- lm(lossrate ~ logtmass*sp.cd + density10*sp.cd +
                     density*sp.cd, data = flam.loss)
flossSpNull <- lm(lossrate ~ logtmass + density + density10,
                   data = flam.loss)
anova(flossFullMod, flossSpNull) #there is species variation in
#flaming biomass loss rate

summary(flossFullMod)

#smoldering biomass loss
slossFullMod <- lm(lossrate ~ logtmass*sp.cd + density10*sp.cd +
                     density*sp.cd, data = smog.loss)
slossSpNull <- lm(lossrate ~ logtmass + density10 + density,
                     data = smog.loss)
anova(slossFullMod, slossSpNull) #there is no species variation 
#in smoldering biomass loss rate


