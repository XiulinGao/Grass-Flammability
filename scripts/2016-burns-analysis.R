###2016-burns-analysis.R
### R script used for looking if measured plant canopy traits(biomass density, 
### biomass ratio) influence grass flammability after account for total biomass
### effect

library(lme4)
library(AICcmodavg)
library(afex)
library(MuMIn)

# Look for additional grass trait that may influence flammability besides total biomass.
# Steps are as follows:
# 1. Look at how total biomass influence flammability with only including total
#    biomass as predictor to build either linear or non-linear model.
# 2. Take residuals from model built at step one, it'll be detrended flammability.
# 3. Build model with including both mass ratio and density as predictors for
#    detrended flammability from step 2.
# 4. Compare AIC values for random effect. Select model that has lowest AIC value.
# 5. Check VIF value for chosen model to determine whether multicolinearity
#    is a problem when biomass ratio and density are correlated vriables (-0.5).
#    It is suggested that a cutoff of 3 for VIF shoule be used. See Alain et al. 2009
#    http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2009.00001.x/full
# 6. Drop variable with VIF >3; otherwise keep all, that being said even two variables
#    has a correlation coefficient as -0.5, multicolinearity is not a problem need to be
#    concerned for detecting the significance of a fixed effect
# 7. Use afex::mixed() to get p values for the fixed effects in chosen model.

## data loading and plot theme set up

source("./final_summary_dataset.R")
source("./ggplot-theme.R") 

color <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
           "#0072B2", "#D55E00", "#CC79A7")

#### duration of heating >10 cm ~ total biomass, biomass ratio and density #### 
##### graphical exploration #####
ggplot(temp.above, aes(total.mass, dur)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) ## positive linear relationship 
## only look at total biomass effect but not any species effect,
## because we do not want to exhaust any "inherent" species variance 
## in flammability at this step but only want to exclude total biomass effect,
## which often is a linear positive effect (but see max. flame height below) 
 
# take biomass effect out
dura.lmod <- lm(dur ~ total.mass, data=temp.above)
summary(dura.lmod) #sig. biomass effect
# take residuals
temp.above$dtr.dura <- residuals(dura.lmod)
#ggplot(temp.above, aes(total.mass, adj_dura)) + geom_point()
##### graphical exploration to see any biomass ratio or density effect #####

ggplot(temp.above, aes(mratio, dtr.dura, color=sp.name)) + geom_point() + 
  bestfit + scale_color_manual(values=color) 
# negative?  
ggplot(temp.above, aes(tdensity, dtr.dura, color=sp.name)) + geom_point() +
  bestfit + scale_color_manual(values=color)
# positive?

# rescale numeric variables to fit a full model
zscore <- function(x) (x - mean(x)) / sd(x)  
resca_dura <- temp.above %>% mutate_at(c("tdensity", "mratio"),
                                       funs(s = zscore(.)))
## compare random effects using AIC criteria
## only random intercept
# dtrdura.ri <- lmer(dtr.dura ~ mratio_s*tdensity_s + (1 | sp.name),
              # data=resca_dura, REML=FALSE) 
## add ratio as random slope
# dtrdura.rsi <- lmer(dtr.dura ~ mratio_s*tdensity_s + 
               # (1 + mratio_s | sp.name), data=resca_dura, REML=FALSE)
# anova(dtrdura.ri, dtrdura.rsi) ## only intercept

## try density as random slope
# dtrdura.rsi2 <- lmer(dtr.dura ~ mratio_s*tdensity_s + (1 + tdensity_s | sp.name),
               # data=resca_dura, REML=FALSE)
# anova(dtrdura.ri, dtrdura.rsi2) ## ok, only random intercept

dtrdura.mod.full <- lmer (dtr.dura ~ tdensity_s*mratio_s + 
                            (1 | sp.name),
                          data=resca_dura, REML=FALSE)
## use vif.mer() to calculate VIF for each variable. The function is adapted 
## from Austin Frank. See: https://github.com/aufrank/R-hacks/blob/master/mer-utils.R

vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

vif.mer(dtrdura.mod.full) ## so VIF for fixed effect of mratio and interaction term are
                    ## greater than 3, let's drop the variable with VIF>3

## significance of fixed effects for simple modle using afex::mixed
dtrdura.mod.simple <- mixed(dtr.dura ~ tdensity_s+ (1|sp.name), 
                            data= resca_dura, REML=FALSE)
summary(dtrdura.mod.simple)
anova(dtrdura.mod.simple) ## so, no effect of density on duration of heating >10cm

## Note: duration of heating (>60 degree) above 10cm location was only positively 
## influenced by total biomass, no effect of density or ratio is detected

##### duration of heating <10cm ~ total biomass, biomass ratio and density ##### 
######### graphical exploration #########

ggplot(temp.below, aes(total.mass, dur)) + geom_point() +
 geom_smooth(method="lm", se=FALSE)
## positive linear relationship 

## build linear model and take residuals
durb.lmod <- lm(dur ~ total.mass, data=temp.below)
summary(durb.lmod) #sig. biomass effec

temp.below$dtr.durb <- residuals(durb.lmod)

##### graphical exploration ######
ggplot(temp.below, aes(tdensity, dtr.durb, color=sp.name)) + geom_point()+
  bestfit + scale_color_manual(values=color)
## positive 

ggplot (temp.below, aes(mratio, dtr.durb, color=sp.name)) + geom_point()+
  bestfit + scale_color_manual(values=color) 
## negative
## rescale numeric variables
resca_durb <- temp.below %>% mutate_at(c("tdensity", "mratio"),
                                       funs(s = zscore(.)))

## compare random slope, intercept
## only random intercept
#dtrdurb.ri <- lmer (dtr.durb ~ tdensity_s*mratio_s + (1|sp.name),
                    #data=resca_durb, REML=FALSE)
## add density as random slope
#dtrdurb.rsi <- lmer (dtr.durb ~ tdensity_s*mratio_s + (1 + tdensity_s|sp.name),
                     #data=resca_durb, REML=FALSE)
#anova(dtrdurb.ri, dtrdurb.rsi) ## should only include random intercept
## try ratio as random slope
#dtrdurb.rsi2 <- lmer(dtr.durb ~ tdensity_s*mratio_s + (1 + mratio_s|sp.name),
                     #data=resca_durb, REML=FALSE)
#anova(dtrdurb.ri, dtrdurb.rsi2) ## no need to include any random slope

dtrdurb.mod.full <- lmer(dtr.durb ~ mratio_s*tdensity_s + (1|sp.name),
                         data=resca_durb, REML=FALSE) 
## vif
vif.mer(dtrdurb.mod.full) ## drop mratio 
## afex::mixed see significance of effect
dtrdurb.mod.simple <- mixed(dtr.durb ~ tdensity_s + (1|sp.name),
                            data=resca_durb, REML=FALSE)
summary(dtrdurb.mod.simple)
anova(dtrdurb.mod.simple) ##significant effect of density
## Note: duration of heating <10cm were positively influenced by both
## total biomass and biomass density. 

## look at R squared using MuMIn :: r.squaredGLMM()
#r.squaredGLMM(lmer(dtr.durb ~ tdensity_s + (1|sp.name),
                   #data=resca_durb, REML=FALSE))

##### maximum loss rate ~ total biomass, biomass density and ratio ######

ggplot(data=flam.loss, aes(total.mass, lossrate)) + geom_point() +
   geom_smooth(method="lm", se=FALSE)
## triangular, not a good linear relationship
## density?
ggplot(data=flam.loss, aes(tdensity, lossrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) ## negative 
## ratio?
ggplot(data=flam.loss, aes(mratio, lossrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) ## weak positive 

## looks like total biomass is not a good predictor for maximum mass loss rate, 
## to keep method consistent, I will go ahead build linear model with only biomass
## being the only predictor at first step

lossr.lmod <- lm(lossrate ~ total.mass, data=flam.loss)
summary(lossr.lmod) ##residuals is not completely dependent from total.mass?
flam.loss$dtr.lossr <- residuals(lossr.lmod)

#rescale variables
resca_lossr <- flam.loss %>% mutate_at(c("tdensity", "mratio"),
                                       funs(s = zscore(.)))
## only random intercept
#lossr.ri <- lmer(dtr.lossr ~ tdensity_s*mratio_s + 
                   #(1|sp.name), data=resca_lossr, REML=FALSE)
## try density as slope
#lossr.rsi <- lmer(dtr.lossr ~ tdensity_s*mratio_s + 
                     #(1 + tdensity_s|sp.name), data=resca_lossr, REML=FALSE)
#anova(lossr.ri, lossr.rsi) ## no denisty slope
## try ratio as slope
#lossr.rsi2 <- lmer(dtr.lossr ~ tdensity_s*mratio_s + 
                     #(1 + mratio_s|sp.name), data=resca_lossr, REML=FALSE)
#anova(lossr.ri, lossr.rsi2) ## ok, mod should not include any random slope

## vif
lossr.vif.test <- lmer (dtr.lossr ~ tdensity_s*mratio_s +
                          (1|sp.name), data=resca_lossr, REML=FALSE)
vif.mer(lossr.vif.test) ## drop mratio
## afex::mixed to see significance of fixed effects
lossr.mod.simple <- mixed (dtr.lossr ~ tdensity_s +  (1|sp.name), 
                          data=resca_lossr, REML=FALSE)
anova(lossr.mod.simple) ## density has p-value <0.05

summary(lossr.mod.simple)
## Note: maximum biomass loss rate is negatively influenced by
## total mass, however, only 4% total variance can be explained.
## biomass density has additional negative effect on max. biomass loss rate
## this is in consistent with previous study

###### max flame height ~ total biomass, biomass density and ratio ######
######## graphical exploration #########
  ggplot(data=arc.trial, aes(total.mass, max.fh)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)
## max. flame height is saturated at higher value of total mass, not linear   

## instead of linear mod, fit a saturation function 
  maxfh.nlmod <- nls(max.fh ~ a*total.mass/ (total.mass+b), data=arc.trial, 
  start=list(a=100, b=5)) 
  summary(maxfh.nlmod)
  #maxfh.predict <- predict(maxfh.nlmod, newdata=arc.trial)
  #ggplot(arc.trial, aes(total.mass, max.fh)) + geom_point(size=1.5) +
  #geom_line(aes(total.mass, maxfh.predict), size=1.5, color="black") 

#### graphical exploration of ratio and density effect ####
## take residuals  
  arc.trial$dtr.fh <- residuals(maxfh.nlmod)
 #ggplot(arc.trial, aes(total.mass, adj_fh)) + geom_point() 
ggplot(arc.trial, aes(tdensity, dtr.fh)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) ## NO

ggplot(arc.trial, aes(mratio, dtr.fh)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) ## NO

# rescale variables
resca_maxfh <- arc.trial %>% mutate_at(c("tdensity", "mratio"), 
                                              funs( s = zscore(.)))
## only include random intercept 
#maxfh.ri <- lmer(dtr.fh ~ tdensity_s*mratio_s + (1 | sp.name),
                 #data=resca_maxfh, REML=FALSE)
## add tdensity as random slope
## note: because correlated randome slope and intercept won't work for
## this case (keep getting warnings) and I don't think there's need to 
## make random slope and intercept correlated here, so I'll set up this
## random slope and intercept uncorrelated here
#maxfh.rsi <- lmer(dtr.fh ~ tdensity_s*mratio_s + ( 0 + tdensity_s | sp.name) +
                    #(1|sp.name), data=resca_maxfh, REML=FALSE)
#anova(maxfh.ri, maxfh.rsi) ## no need to add tdensity as slope
## try ratio as slope
#maxfh.rsi2 <- lmer(dtr.fh ~ tdensity_s*mratio_s + ( 0 + mratio_s | sp.name) +
                     #(1|sp.name), data=resca_maxfh, REML=FALSE)
#anova(maxfh.ri, maxfh.rsi2) ## ok, no slope

## vif test and drop variable with vif >3
maxfh.vif.test <- lmer (dtr.fh ~ mratio_s*tdensity_s + (1 |sp.name),
                        data=resca_maxfh, REML=FALSE)
vif.mer(maxfh.vif.test) ## no need to drop any here

#drop non-significant interaction term
dtrmaxfh.mod.full <- mixed(dtr.fh ~ mratio_s*tdensity_s + (1 |sp.name),
                      data=resca_maxfh, REML=FALSE)
anova(dtrmaxfh.mod.full) ## very weak signal for density effect

dtrmaxfh.mod.simple <- mixed (dtr.fh ~ tdensity_s + (1 |sp.name),
                           data=resca_maxfh, REML=FALSE)
summary(dtrmaxfh.mod.simple)
anova(dtrmaxfh.mod.simple) ## it is a saturation relationship between max.
                        ## flame height and total mass
## Note: max. flame height seems only affected by total biomass, however, it is
## a saturation function but not linear

##### mass loss ~ total mass, biomass density and ratio #####

#cairo_pdf("example.pdf", family="Arial", width=6, height=6)

ggplot(data=arc.trial, aes(total.mass, mconsum)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) 
  
tmlossLM <- lm(mconsum ~ total.mass, data=arc.trial)
summary(tmlossLM)
arc.trial$dtr.mloss <- residuals(tmlossLM)

resca_mloss <- arc.trial %>% mutate_at(c("tdensity", "mratio"),
                                          funs(s = zscore(.)))
##### graphical exploration ######
ggplot(arc.trial, aes(tdensity, dtr.mloss, color=sp.name)) + geom_point() +
  bestfit + scale_color_manual(values=color) #positive

ggplot(arc.trial, aes(mratio, dtr.mloss, color=sp.name)) + geom_point() +
  bestfit + scale_color_manual(values=color) #negative

## only random intercept
mloss.ri <- lmer(dtr.mloss ~ mratio_s * tdensity_s + 
                   (1 |sp.name), data=resca_mloss, REML=FALSE)
##add mratio as slope
mloss.rsi <- lmer(dtr.mloss ~ mratio_s*tdensity_s + (1 + mratio_s|sp.name), 
                      data=resca_mloss, REML=FALSE)
anova(mloss.ri, mloss.rsi) # need add mratio as slope
## add density
mloss.rsi2 <- lmer(dtr.mloss ~ mratio_s*tdensity_s + (1 + mratio_s + tdensity_s|sp.name), 
                   data=resca_mloss, REML=FALSE)

anova(mloss.rsi, mloss.rsi2) #should only include mratio as random slope
## vif
mloss.vif.test <- lmer(dtr.mloss ~ mratio_s*tdensity_s + (1 + mratio_s|sp.name), 
                       data=resca_mloss, REML=FALSE)
vif.mer(mloss.vif.test) ## no need to drop any variable
## significance of fixed effect
dtrmloss.mod.full <- mixed (dtr.mloss ~ mratio_s*tdensity_s + (1 + mratio_s|sp.name), 
                       data=resca_mloss, REML=FALSE)
summary(dtrmloss.mod.full)
anova(dtrmloss.mod.full) ## biomass consumption is only influenced by total mass
## however, there is a significant cross-over interaction between biomass ratio and
## density

#Conclusion: totla biomass has first order effect on all flammability measurements
#Besides that, biomass density has additional positive effect on duration
#of heating at <10cm location and negative effect on max. mass loss rate 

