###2016-burns-analysis.R
### R script used for looking if measured plant canopy traits(biomass density, 
### biomass ratio) influence grass flammability after account for total biomass
### effect

library(lme4)
#library(AICcmodavg)
library(afex)
#library(MuMIn)
library(pcaMethods)

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

############### dimension of grass flammability ################
## apply principal component analysis to selected flammability measurement 
## for further analysis

flamabove.PCA <- temp.above %>%
  select (ignition, combustion, dur, lossrate, mconsum, degsec) %>% 
  pca(nPcs=6, method="ppca",center=TRUE,scale="uv")
summary(flamabove.PCA) ## first three axes explain 93% total variance
## the 2nd and 3rd are equivalent (16.9%, 13.7%)
biplot(flamabove.PCA, choices = c("PC1","PC2"))
biplot(flamabove.PCA, choices = c("PC1","PC3"))
loadings(flamabove.PCA)

flambase.PCA <- temp.base %>% select(ignition, dur, lossrate, mconsum, degsec,
  max.fh) %>% pca(nPcs=6, method="ppca", center=TRUE, scale="uv")   
summary(flambase.PCA)
loadings(flambase.PCA)
### result is same when use base heating
### given loadings, the first axis captures total heat release related
### measurements (temperature integration, duration above 100 degree, total mass
### loss had the highest loadings); the second axis captures fire intensity related
### measurements(max loss rate and max flame height had the highest loading), and the
### third axis again captures total heat release with total mass loss had the highest 
### loading. Given temperature integration, duration above 100 and total mass loss
### are closely related and temperature integration had the highest loading on first 
### axis, I will choose degsec (temperature integration), max mass loss rate and max
### flame height for further analysis

#### integrated temp above 100 ~ total biomass, biomass ratio and density #### 
##### graphical exploration #####
ggplot(temp.above, aes(total.mass, degsec)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)## positive linear relationship 
## only look at total biomass effect but not any species effect,
## because we do not want to exhaust any "inherent" species variance 
## in flammability at this step

# take biomass effect out
degseca.lmod <- lm(degsec ~ total.mass, data=temp.above)
summary(degseca.lmod) #sig. biomass effect
# take residuals
temp.above$crt.degseca <- residuals(degseca.lmod)
#ggplot(temp.above, aes(total.mass, adj_dura)) + geom_point()
##### graphical exploration to see trait and weather effect #####

ggplot(temp.above, aes(mratio, crt.degseca, color=sp.name)) + geom_point() + 
  bestfit + scale_color_manual(values=color) 
# negative?  two extreme case?
ggplot(temp.above, aes(tdensity, crt.degseca, color=sp.name)) + geom_point() +
  bestfit + scale_color_manual(values=color)
# positive?
ggplot(temp.above, aes(humidity, crt.degseca, color=sp.name)) + geom_point() + 
  bestfit + scale_color_manual(values=color) 
# weak positive?
ggplot(temp.above, aes(temp, crt.degseca, color=sp.name)) + geom_point() + 
  bestfit + scale_color_manual(values=color) 
# weak negative?
# rescale numeric variables to fit a full model
zscore <- function(x) (x - mean(x)) / sd(x)  
resca_degseca <- temp.above %>% mutate_at(c("tdensity", "mratio", "temp", "humidity"),
                                       funs(s = zscore(.)))
## compare random intercept, slope using AIC criteria
## only species random intercept
 crtdegseca.ri <- lmer(crt.degseca ~ mratio_s*tdensity_s + temp_s*(mratio_s + tdensity_s) + 
                      humidity_s*(mratio_s + tdensity_s) + (1 | sp.name), 
                    data = resca_degseca, REML = FALSE) 
## trial random intercept
 crtdegseca.ri2 <- lmer(crt.degseca ~ mratio_s*tdensity_s + temp_s*(mratio_s + tdensity_s) + 
                       humidity_s*(mratio_s + tdensity_s) + (1 | sp.name) +
                       (1 | trial.date), 
                     data = resca_degseca, REML = FALSE)
anova(crtdegseca.ri, crtdegseca.ri2) ## no need to add trial date as another intercept

## add ratio as random slope
 crtdegseca.rsi <- lmer(crt.degseca ~ mratio_s*tdensity_s + 
                          temp_s*(mratio_s + tdensity_s) + 
                       humidity_s*(mratio_s + tdensity_s) + (1 + mratio_s | sp.name), 
                      data = resca_degseca, REML = FALSE)
anova(crtdegseca.ri, crtdegseca.rsi) ## only intercept

## try density as random slope
 crtdegseca.rsi2 <- lmer(crt.degseca ~ mratio_s*tdensity_s +
                           temp_s*(mratio_s + tdensity_s) +
                      humidity_s*(mratio_s + tdensity_s) +
                        (1 + tdensity_s | sp.name),
                      data = resca_degseca, REML = FALSE)
anova(crtdegseca.ri, crtdegseca.rsi2) ## ok, only random intercept

crtdegseca.mod.full <- mixed (crt.degseca ~ tdensity_s*mratio_s + 
                          temp_s*(mratio_s + tdensity_s) +
                          humidity_s *(mratio_s + tdensity_s) +
                            (1 | sp.name),
                          data = resca_degseca, REML = FALSE)
anova(crtdegseca.mod.full) # no interaction effect is significant, drop all
crtdegseca.mod.simple <- lmer(crt.degseca ~ tdensity_s*humidity_s + mratio_s + 
                                temp_s + (1 | sp.name), 
                              data= resca_degseca, REML=FALSE)
## use vif.mer() to calculate variance inflation factor for each variable. 
## The function is adapted from Austin Frank. 
## See: https://github.com/aufrank/R-hacks/blob/master/mer-utils.R

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

vif.mer(crtdegseca.mod.simple) ## keep all

## significance of fixed effects using afex::mixed
crtdegseca.mod.simple <- mixed(crt.degseca ~ tdensity_s*humidity_s + mratio_s + 
                               temp_s +  (1 | sp.name),
                            data= resca_degseca, REML=FALSE)
anova(crtdegseca.mod.simple) ## no need to drop any

## Note: temperature integration (>100 degree) above 10cm location was positively 
## influenced by total biomass, negatively influenced by biomass ratio
## when take out the extreme observations(ec29 & sn03, sn09), result kept same

##### integrated temp (>100) <10cm ~ total biomass, biomass ratio and density ##### 
######### graphical exploration #########

ggplot(temp.base, aes(total.mass, degsec)) + geom_point() +
 geom_smooth(method="lm", se=FALSE)
## positive linear relationship 

## build linear model and take residuals
degsecb.lmod <- lm(degsec ~ total.mass, data= temp.base)
summary(degsecb.lmod) #sig. biomass effec

temp.base$crt.degsecb <- residuals(degsecb.lmod)

##### graphical exploration ######
ggplot(temp.base, aes(tdensity, crt.degsecb, color=sp.name)) + geom_point()+
  bestfit + scale_color_manual(values=color)
## positive 

ggplot (temp.base, aes(mratio, crt.degsecb, color=sp.name)) + geom_point()+
  bestfit + scale_color_manual(values=color) 
## negative
ggplot (temp.base, aes(humidity, crt.degsecb, color=sp.name)) + geom_point()+
  bestfit + scale_color_manual(values=color) 
## positive?
ggplot (temp.base, aes(temp, crt.degsecb, color=sp.name)) + geom_point()+
  bestfit + scale_color_manual(values=color) 
## negative?
## rescale numeric variables
resca_degsecb <- temp.base %>% mutate_at(c("tdensity", "mratio", "humidity", "temp"),
                                       funs(s = zscore(.)))

## compare random slope, intercept
## only random intercept
crtdegsecb.ri <- lmer (crt.degsecb ~ tdensity_s*mratio_s + 
                      humidity_s*(mratio_s + tdensity_s)+
                      temp_s*(mratio_s + tdensity_s)+
                      (1|sp.name), data= resca_degsecb, REML=FALSE)
## trial date intercept
crtdegsecb.ri2 <- lmer (crt.degsecb ~ tdensity_s*mratio_s + 
                       humidity_s*(mratio_s + tdensity_s)+
                       temp_s*(mratio_s + tdensity_s)+
                      (1|sp.name) + (1|trial.date), data=resca_degsecb, REML=FALSE)
anova(crtdegsecb.ri, crtdegsecb.ri2) ## only species as random intercept

## add density as random slope
crtdegsecb.rsi <- lmer (crt.degsecb ~ tdensity_s*mratio_s + 
                      humidity_s*(mratio_s + tdensity_s)+
                     temp_s*(mratio_s + tdensity_s)+
                       (1 + tdensity_s|sp.name),
                     data=resca_degsecb, REML=FALSE)
anova(crtdegsecb.ri, crtdegsecb.rsi) ## no random slope
## try ratio as random slope
crtdegsecb.rsi2 <- lmer(crt.degsecb ~ tdensity_s*mratio_s + 
                      humidity_s*(mratio_s + tdensity_s) +
                       temp_s*(mratio_s + tdensity_s) +
                       (1 + mratio_s|sp.name),
                     data=resca_degsecb, REML=FALSE)
anova(crtdegsecb.ri, crtdegsecb.rsi2) ## no need to include any random slope

crtdegsecb.mod.full <- mixed(crt.degsecb ~ mratio_s*tdensity_s + 
                    humidity_s*(mratio_s + tdensity_s)+
                    temp_s*(mratio_s + tdensity_s)+ 
                    (1|sp.name), data=resca_degsecb, REML=FALSE) 
anova(crtdegsecb.mod.full) # drop all interaction term
crtdegsecb.mod.simple <- lmer(crt.degsecb ~ mratio_s + tdensity_s +
                             humidity_s + temp_s + (1|sp.name),
                           data=resca_degsecb, REML=FALSE)
## vif
vif.mer(crtdegsecb.mod.simple) ## keep all
## afex::mixed see significance of effect
crtdegsecb.mod.simple <- mixed(crt.degsecb ~ tdensity_s + mratio_s+
                                 humidity_s + temp_s + (1|sp.name),
                               data=resca_degsecb, REML=FALSE)
anova(crtdegsecb.mod.simple)

## drop non-significant fixed effect
crtdegsecb.mod.final <- mixed(crt.degsecb ~ tdensity_s + humidity_s + temp_s +
                                (1|sp.name),data=resca_degsecb, REML=FALSE)
anova(crtdegsecb.mod.final)
summary(crtdegsecb.mod.final)
##significant positive effect of density, humidity and temperature on degsec<10cm
## Note: integrated temperature (>100 degree) <10cm were positively influenced by 
## total biomass, biomass density, humidity and temp ( residance time increased? slow rate?)  
## *result is the same when take the one extreme cases (ec29, al01, al15) out* 

## look at R squared using MuMIn :: r.squaredGLMM()
#r.squaredGLMM(lmer(dtr.durb ~ tdensity_s + (1|sp.name),
                   #data=resca_durb, REML=FALSE))

##### maximum loss rate ~ total biomass, biomass density and ratio ######

ggplot(data=flam.loss, aes(total.mass, lossrate)) + geom_point() +
   geom_smooth(method="lm", se=FALSE)
## triangular, not a good linear relationship
## log total biomass
ggplot(data=flam.loss, aes(logtmass, lossrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)

lossr.lmod <- lm(lossrate ~ total.mass, data=flam.loss)
summary(lossr.lmod) 
flam.loss$crt.lossr <- residuals(lossr.lmod)

######## graphical explore effect of density, ratio and weather ##########
ggplot (data=flam.loss, aes(tdensity, crt.lossr)) + geom_point()+
  geom_smooth(method="lm", se=FALSE) ## weak negative?

ggplot (data=flam.loss, aes(mratio, crt.lossr)) + geom_point()+
  geom_smooth(method="lm", se=FALSE) ## weak positive?

ggplot (data=flam.loss, aes(temp, crt.lossr)) + geom_point()+
  geom_smooth(method="lm", se=FALSE) ## weak positive?

ggplot (data=flam.loss, aes(humidity, crt.lossr)) + geom_point()+
  geom_smooth(method="lm", se=FALSE) ## weak negative?

#rescale variables
resca_lossr <- flam.loss %>% mutate_at(c("tdensity", "mratio", "humidity", "temp"),
                                       funs(s = zscore(.)))
## only random intercept
lossr.ri <- lmer(crt.lossr ~ tdensity_s*mratio_s + 
                humidity_s*(tdensity_s + mratio_s) +
                 temp_s*(tdensity_s + mratio_s) +
                   (1 | sp.name), data = resca_lossr, REML = FALSE)
## trial date as another intercept
lossr.ri2 <- lmer(crt.lossr ~ tdensity_s*mratio_s + 
                    humidity_s*(tdensity_s + mratio_s) +
                    temp_s*(tdensity_s + mratio_s) +
                    (1 | sp.name) + (1 | trial.date),
                  data = resca_lossr, REML = FALSE)
anova(lossr.ri, lossr.ri2) ## no need to add trial date as another intercept

## try density as slope
lossr.rsi <- lmer(crt.lossr ~ tdensity_s*mratio_s + 
                  humidity_s*(tdensity_s + mratio_s) +
                  temp_s*(tdensity_s + mratio_s) +
                   (1 + tdensity_s|sp.name),
                  data=resca_lossr, REML=FALSE)
anova(lossr.ri, lossr.rsi) ## no denisty slope

## try ratio as slope
lossr.rsi2 <- lmer(crt.lossr ~ tdensity_s*mratio_s + 
                    humidity_s*(tdensity_s + mratio_s) +
                   temp_s*(tdensity_s + mratio_s) +
                  (1 + mratio_s|sp.name), 
                   data=resca_lossr, REML=FALSE)
anova(lossr.ri, lossr.rsi2) ## no ratio as random slope

lossr.mod.full <- mixed(crt.lossr ~ tdensity_s*mratio_s + 
                          humidity_s*(tdensity_s + mratio_s) +
                          temp_s*(tdensity_s + mratio_s) +
                          (1|sp.name),
                        data=resca_lossr, REML=FALSE)
anova(lossr.mod.full) ## drop interaction terms

lossr.mod.simple <- lmer(crt.lossr ~ tdensity_s + mratio_s + humidity_s +
                           temp_s + (1|sp.name),
                         data=resca_lossr, REML=FALSE)
## vif
vif.mer(lossr.mod.simple) ## keep all
## afex::mixed to see significance of fixed effects
lossr.mod.simple <- mixed (crt.lossr ~ tdensity_s + mratio_s + humidity_s +
                             temp_s + (1|sp.name),
                           data=resca_lossr, REML=FALSE)
anova(lossr.mod.simple) ## no effect, result kept same without extreme cases
lossr.mod.final <- mixed(crt.lossr ~ tdensity_s + (1|sp.name), 
                         data=resca_lossr, REML=FALSE)
anova(lossr.mod.final)
summary(lossr.mod.final)
## Note: maximum biomass loss rate is negatively influenced by
## total mass (logged) only 


###### max flame height ~ total biomass, biomass density and ratio ######
######## graphical exploration #########
ggplot(data=arc.trial, aes(total.mass, max.fh)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)
## max. flame height is saturated at higher value of total mass, not linear 
## try log transfer biomass
ggplot(data=arc.trial, aes(logtmass, max.fh)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) ## ok positive linear

## instead of linear mod, fit a saturation function 
   maxfh.nlmod <- nls(max.fh ~ a*total.mass/ (total.mass+b), data=arc.trial, 
  start=list(a=100, b=5)) 
  summary(maxfh.nlmod)
  maxfh.predict <- predict(maxfh.nlmod, newdata=arc.trial)
  #ggplot(arc.trial, aes(total.mass, max.fh)) + geom_point(size=1.5) +
  #geom_line(aes(total.mass, maxfh.predict), size=1.5, color="black") 
## take out effect of logged biomass on max flame height
maxfh.lmod <- lm(max.fh ~ logtmass, data=arc.trial)
summary(maxfh.lmod)
arc.trial$crt.maxfh <- residuals(maxfh.lmod)

#### graphical exploration of ratio and density effect ####

ggplot(arc.trial, aes(tdensity, crt.maxfh)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) ## weak negative?

ggplot(arc.trial, aes(mratio, crt.maxfh)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) ## NO

ggplot(arc.trial, aes(temp, crt.maxfh)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) ## weak positive?

ggplot(arc.trial, aes(humidity, crt.maxfh)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) ## weak negative?

# rescale variables
resca_maxfh <- arc.trial %>% mutate_at(c("tdensity", "mratio", "humidity", "temp"), 
                                              funs( s = zscore(.)))
## only include species as random intercept 
maxfh.ri <- lmer(crt.maxfh ~ tdensity_s*mratio_s + humidity_s*(tdensity_s + mratio_s) +
                 temp_s*(tdensity_s + mratio_s) +
                   (1 | sp.name),data=resca_maxfh, REML=FALSE)
## trial date as another intercept
maxfh.ri2 <- lmer(crt.maxfh ~ tdensity_s*mratio_s + humidity_s*(tdensity_s + mratio_s) +
                    temp_s*(tdensity_s + mratio_s) +
                    (1 | sp.name) + (1 | trial.date),
                   data=resca_maxfh, REML=FALSE)
anova(maxfh.ri, maxfh.ri2) ## no need to add

## add tdensity as random slope

maxfh.rsi <- lmer(crt.maxfh ~ tdensity_s*mratio_s + humidity_s*(tdensity_s + mratio_s) +
                  temp_s*(tdensity_s + mratio_s) +
                    ( 1 + tdensity_s | sp.name), data=resca_maxfh, REML=FALSE)
anova(maxfh.ri, maxfh.rsi) ## no need to add tdensity as slope
## try ratio as slope
maxfh.rsi2 <- lmer(crt.maxfh ~ tdensity_s*mratio_s + humidity_s*(tdensity_s + mratio_s) + 
                  temp_s*(tdensity_s + mratio_s) +
                     ( 1 + mratio_s | sp.name), data=resca_maxfh, REML=FALSE)
anova(maxfh.ri, maxfh.rsi2) ## ok, no slope

crtmaxfh.mod.full <- mixed(crt.maxfh ~ tdensity_s*mratio_s + 
                          humidity_s*(tdensity_s + mratio_s) +
                          temp_s*(tdensity_s + mratio_s) + (1|sp.name),
                          data= resca_maxfh, REML=FALSE) 
anova(crtmaxfh.mod.full) 
## drop interaction terms 
crtmaxfh.mod.simple <- lmer(crt.maxfh ~ tdensity_s +  temp_s + mratio_s + humidity_s +
                           (1|sp.name), data=resca_maxfh, REML=FALSE)
## vif test and drop variable with vif >3

vif.mer(crtmaxfh.mod.simple) ## no need to drop any here

## look at significance
crtmaxfh.mod.simple <- mixed (crt.maxfh ~ temp_s + tdensity_s +  mratio_s + humidity_s +
                        (1 |sp.name), data=resca_maxfh, REML=FALSE)
anova(crtmaxfh.mod.simple) ## no canopy trait effects


## Note: max flame height is positively influenced by fuel load only







