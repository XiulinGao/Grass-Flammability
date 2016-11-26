## massdensity_influence.R
## the main question gonna to be answered here is if
## biomass density of two sections has influence on flammability
## at corresponding section or whole plant. at the end, thers is
## a model for examing if plant height affects maximum flame height

#1. if biomass density has effect on dur?

#>10
durFullMod <- lm( dur ~ logtmass*sp.cd + density*sp.cd, 
                  data = temp.abovesec)

durDenNull <- lm(dur ~ logtmass*sp.cd, data = temp.abovesec)
anova(durFullMod, durDenNull) #there is no sig. influence of 
#biomass density on duration at >10 section

#<10
dur10FullMod <- lm( dur ~ logtmass*sp.cd + density10*sp.cd, 
                    data = temp.belowsec)
dur10DenNull <- lm(dur ~ logtmass*sp.cd, data = temp.belowsec)
anova(dur10FullMod, dur10DenNull) #there is no sig. biomass density 
#influence on duration at <10 section
#So, there is no significant effect of biomass density on duration

#2. does biomass density have effect on total degree of temperature
#generated, which is above 60?

#>10
degFullMod <- lm(degsec ~ logtmass*sp.cd + density*sp.cd,
                 data = temp.abovesec)
degDenNull <- lm(degsec ~ logtmass*sp.cd, data = temp.abovesec)
anova(degFullMod, degDenNull)#there is no sig. influence of mass
#density on total degree of temperature at >10 section

#<10
deg10FullMod <- lm(degsec ~ logtmass*sp.cd + density10*sp.cd,
                   data = temp.belowsec)
deg10DenNull <- lm(degsec ~ logtmass*sp.cd, data = temp.belowsec)
anova(deg10FullMod, deg10DenNull)#no sig. influence

#So, there is no significant influence of biomass density on total
#degree of temperature(>60) generated

#3. does biomass density have effect on maximum flame height?

flamhFullMod <- lm(max.fh ~ logtmass*sp.cd + density10*sp.cd +
                     density*sp.cd + height*sp.cd, data = flam.height)

#null model for density >10
flamhDenNull <- lm(max.fh ~ logtmass*sp.cd + density10*sp.cd + 
                    height*sp.cd, data = flam.height)
anova(flamhFullMod, flamhDenNull) #there is no sig. effect 

#null model for density <10
flamh10DenNull <- lm(max.fh ~ logtmass*sp.cd + density*sp.cd + 
                       height*sp.cd, data = flam.height)
anova(flamhFullMod, flamh10DenNull) #NO sig. effect
#So, there is no significant bomass density effect on maximum 
#flame height

#4. does biomass density have influence on mass loss rate?

#flaming biomass loss rate
flossFullMod <- lm(lossrate ~ logtmass*sp.cd + density10*sp.cd +
                     density*sp.cd, data = flam.loss)
#null model for density >10
flossDenNull <- lm(lossrate ~ logtmass*sp.cd + density10*sp.cd,
                  data = flam.loss)
anova(flossFullMod, flossDenNull) #there is no sig. influence 

#null model for density <10
floss10DenNull <- lm(lossrate ~ logtmass*sp.cd + density*sp.cd,
                   data = flam.loss)
anova(flossFullMod, floss10DenNull) #no sig. effect


#smoldering biomass loss
slossFullMod <- lm(lossrate ~ logtmass*sp.cd + density10*sp.cd +
                     density*sp.cd, data = smog.loss)
#null model for density >10
slossDenNull <- lm(lossrate ~ density10*sp.cd + logtmass*sp.cd,
                  data = smog.loss)
anova(slossFullMod, slossDenNull) #there is no sig. effect

#null model for density <10
sloss10DenNull <- lm(lossrate ~ density*sp.cd + logtmass*sp.cd,
                   data = smog.loss)
anova(slossFullMod, sloss10DenNull) #no sig. effect
#So, there is no significant effect of biomass density on biomass
#loss rate either at flaming or smoldering stage

#5. does biomass density influence peak temp?
#>10
tempFullMod <- lm(peak.temp ~ logtmass*sp.cd + density*sp.cd, 
                  data = temp.abovesec)
tempDenNull <- lm(peak.temp ~ logtmass*sp.cd, data =  temp.abovesec)
anova(tempFullMod, tempDenNull) #no sig.

#<10 
temp10FullMod <- lm(peak.temp ~ logtmass*sp.cd + density10*sp.cd, 
                    data = temp.belowsec)
temp10DenNull <- lm(peak.temp ~ logtmass*sp.cd, data = temp.belowsec)
anova(temp10FullMod, temp10DenNull) #no sig.

#Ok, looks like biomass density has no influence on any interested 
#flammability measurement at all. Fine, I should take density off from
#all the modles I guess.

#i don't want to start a new script just for looking for plant
#height influence on maximum flame height, do it here

flamhHeiNull <- lm(max.fh ~ logtmass*sp.cd + density*sp.cd +
                     density10*sp.cd, data = flam.height)
anova(flamhFullMod, flamhHeiNull)#ok, plant height does not
#influence maximum flame height
