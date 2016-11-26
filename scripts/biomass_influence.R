##biomass_influence.R
#the main question gonna to be answered here is
#if there is biomass influence on interested flammability
#measurements and how.

#1. does total biomass have influence on duration for two
#sections? Since I'm interested if biomass allocation will
#influence temperature generated during burn at two sections
#for each section I will use conrresponding biomass density as
#the predictor, and totall biomass will be used for both section

#>10
durFullMod <- lm( dur ~ logtmass*sp.cd + density*sp.cd, 
                  data = temp.abovesec)

durMaNull <- lm(dur ~ density*sp.cd, data = temp.abovesec)
anova(durFullMod, durMaNull) #there is sig. influence of total
#biomass on duration

#<10
dur10FullMod <- lm( dur ~ logtmass*sp.cd + density10*sp.cd, 
                    data = temp.belowsec)
dur10MaNull <- lm(dur ~ density10*sp.cd, data = temp.belowsec)
anova(dur10FullMod, dur10MaNull) #there is sig. biomass influence

#2. does total biomass have influence on total degree of temperature
#generated during the dur?

#>10
degFullMod <- lm(degsec ~ logtmass*sp.cd + density*sp.cd,
                 data = temp.abovesec)
degMaNull <- lm(degsec ~ density*sp.cd, data = temp.abovesec)
anova(degFullMod, degMaNull)#there is sig. influence of total mass

#<10
deg10FullMod <- lm(degsec ~ logtmass*sp.cd + density10*sp.cd,
                   data = temp.belowsec)
deg10MaNull <- lm(degsec ~ density10*sp.cd, data = temp.belowsec)
anova(deg10FullMod, deg10MaNull)#sig. influence

#3. does total biomass have influence on maximum flame height?

flamhFullMod <- lm(max.fh ~ logtmass*sp.cd + density10*sp.cd +
                  density*sp.cd + height*sp.cd, data = flam.height)
flamhMaNull <- lm(max.fh ~ density10*sp.cd + density*sp.cd +
                    height*sp.cd, data = flam.height)
anova(flamhFullMod, flamhMaNull) #there is sig. effect 

#4. does total biomass have influence on mass loss rate?

#flaming biomass loss rate
flossFullMod <- lm(lossrate ~ logtmass*sp.cd + density10*sp.cd +
                     density*sp.cd, data = flam.loss)
flossMaNull <- lm(lossrate ~ density*sp.cd + density10*sp.cd,
                  data = flam.loss)
anova(flossFullMod, flossMaNull) #there is sig. influence of biomass


#smoldering biomass loss
slossFullMod <- lm(lossrate ~ logtmass*sp.cd + density10*sp.cd +
                     density*sp.cd, data = smog.loss)
slossMaNull <- lm(lossrate ~ density10*sp.cd + density*sp.cd,
                  data = smog.loss)
anova(slossFullMod, slossMaNull) #there is no biomass influence of
#total biomass on mass loss rate during smodelring stage.



