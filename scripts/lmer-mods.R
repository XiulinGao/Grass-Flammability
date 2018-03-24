###lmer-mods.R
### R script used for looking if measured plant canopy traits(biomass density, 
### biomass ratio) influence grass flammability after account for total biomass
### effect

library(lme4)
   #library(AICcmodavg)
library(afex)
#library(MuMIn)
library(pcaMethods)
library(xtable)
library(MASS)

# Look for additional grass trait that may influence flammability besides total biomass.
# Steps are as follows:
# 1. Look at how total biomass influence flammability with only including total
#    biomass as predictor to build either linear or non-linear model.
# 2. Take residuals from model built at step one, it'll be biomass-corrected flammability.
# 3. Build model with including both mass ratio and density as predictors for
#    flammability from step 2.
# 4. Compare full model with other models with same fixed effect but different
# random effect and select model that has lowest AIC value, or same AIC but simpler model
# 5. look at sigificant of fixed effect and drop non-significant interaction terms 
# 5. Check VIF value for chosen model to determine whether multicolinearity
#    is a problem when biomass ratio and density are correlated vriables (-0.5).
#    It is suggested that a cutoff of 3 for VIF shoule be used. See Alain et al. 2009
#    http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2009.00001.x/full
# 6. Drop variable with VIF >3; otherwise keep all, that being said even two variables
#    has a correlation coefficient as -0.5, multicolinearity is not a problem need to be
#    concerned for detecting the significance of a fixed effect
# 7. Report final model with anova and coefficient table

## data loading and plot theme set up

source("./final_summary_dataset.R")
source("./ggplot-theme.R") 

color <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
           "#0072B2", "#D55E00", "#CC79A7")

############### dimension of grass flammability ################
## apply principal component analysis to selected flammability measurement 
## for further analysis

flamabove.PCA <- pcadata.above %>%
  select (dur, lossrate, massloss, degsec) %>% 
  pca(nPcs=4, method="ppca",center=TRUE,scale="uv")
summary(flamabove.PCA) 
loadings(flamabove.PCA)
## total heat release and rate of heat release are independent axes

biplot(flamabove.PCA, choices = c("PC1","PC2"))

flambase.PCA <- pcadata.base %>% select(dur, lossrate, massloss, degsec) %>% 
  pca(nPcs=4, method="ppca", center=TRUE, scale="uv")   
summary(flambase.PCA)
loadings(flambase.PCA)

### result is same when use base heating

#### integrated temp above 100 ~ total biomass, biomass ratio and density #### 
##### graphical exploration #####
ggplot(temp.above, aes(total.mass, degsec)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)## positive linear relationship 
## only look at total biomass effect but not any species effect,
## because we do not want to exhaust any "inherent" species variance 
## in flammability at this step

# take biomass effect out
degseca.lmod <- lm(degsec ~ total.mass, data=temp.above)
#plot(degseca.lmod) # observation 82 has a cook's D >0.5, which
# is due to the extreme value of predictor (high biomass)
# instead of OLS, I will use robust regression
#degseca.rlmod <- rlm(degsec ~ total.mass,method="MM",data=temp.above)
#when o robust regression, result is same

summary(degseca.lmod) #sig. biomass effect
# take residuals
temp.above$crt.degseca <- residuals(degseca.lmod)
#ggplot(temp.above, aes(total.mass, adj_dura)) + geom_point()
##### graphical exploration to see trait and weather effect #####

ggplot(temp.above, aes(mratio, crt.degseca, color=sp.name)) + geom_point() + 
  bestfit + scale_color_manual(values=color) 
# negative?  
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
## full modle

crtdegseca.mod.full <- lmer(crt.degseca ~ mratio_s*tdensity_s + temp_s*(mratio_s + tdensity_s)+
                              humidity_s*(mratio_s + tdensity_s) + 
                              (1 + tdensity_s + mratio_s | sp.name) + (1|trial.date),
                            data = resca_degseca, REML = FALSE)

## compare random intercept, slope using AIC criteria
## only random species intercept
 crtdegseca.ri <- lmer(crt.degseca ~ mratio_s*tdensity_s + temp_s*(mratio_s + tdensity_s) + 
                      humidity_s*(mratio_s + tdensity_s) + 
                      (1 | sp.name), 
                    data = resca_degseca, REML = FALSE) 
anova(crtdegseca.mod.full, crtdegseca.ri) 
## only species intercept is better

## add trial intercept
crtdegseca.ri2 <- lmer(crt.degseca ~ mratio_s*tdensity_s + 
                             temp_s*(mratio_s + tdensity_s) + 
                             humidity_s*(mratio_s + tdensity_s) + (1 | sp.name) +
                         (1 | trial.date), data = resca_degseca, REML = FALSE)
anova(crtdegseca.ri2, crtdegseca.ri) 
## no need to add trial intercept

## add tdensity slope
 crtdegseca.rsi <- lmer(crt.degseca ~ mratio_s*tdensity_s + temp_s*(mratio_s + tdensity_s) + 
                       humidity_s*(mratio_s + tdensity_s) + (1 + tdensity_s | sp.name),
                     data = resca_degseca, REML = FALSE)
 anova(crtdegseca.ri, crtdegseca.rsi) 
## no need to add density slope

## add mratio slope
 crtdegseca.rsi2 <- lmer(crt.degseca ~ mratio_s*tdensity_s + 
                          temp_s*(mratio_s + tdensity_s) + 
                       humidity_s*(mratio_s + tdensity_s) + (1 + mratio_s | sp.name),
                      data = resca_degseca, REML = FALSE)
anova(crtdegseca.ri, crtdegseca.rsi2)
## add mratio slope does not make model much better,
## very close AIC, so chose the simpler model that only include random intercept

crtdegseca.mod.simple <- mixed (crt.degseca ~ tdensity_s*mratio_s + 
                          temp_s*(mratio_s + tdensity_s) +
                          humidity_s *(mratio_s + tdensity_s) +
                           (1 | sp.name),
                          data = resca_degseca, REML = FALSE)
anova(crtdegseca.mod.simple) # drop non-significant interaction term

crtdegseca.mod.simple <- lmer(crt.degseca ~ humidity_s*tdensity_s + 
                                temp_s + mratio_s + (1|sp.name), 
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

vif.mer(crtdegseca.mod.simple) ## keep both 

## significance of fixed effects using afex::mixed
crtdegseca.mod.simple <- mixed(crt.degseca ~ humidity_s* tdensity_s + mratio_s +
                              temp_s + (1 | sp.name),
                            data= resca_degseca, REML=FALSE)
anova(crtdegseca.mod.simple)
print(xtable(anova(crtdegseca.mod.simple)))
print(xtable(summary(crtdegseca.mod.simple)$coefficients))

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
## result kept same with or without observation 82 (influential point)
## robust regression
#degsecb.rlmod <- rlm(degsec ~ total.mass, data=temp.base)
## result is same when use robust regression
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
## full model
crtdegsecb.mod.full <- lmer(crt.degsecb ~ tdensity_s*mratio_s +
                              humidity_s*(mratio_s + tdensity_s) +
                              temp_s*(mratio_s + tdensity_s) +
                              (1 + mratio_s + tdensity_s | sp.name) +
                              (1|trial.date), data=resca_degsecb, REML = FALSE) 
## compare random slope, intercept
## only species random intercept
crtdegsecb.ri <- lmer (crt.degsecb ~ tdensity_s*mratio_s + 
                      humidity_s*(mratio_s + tdensity_s)+
                      temp_s*(mratio_s + tdensity_s)+
                      (1|sp.name), data= resca_degsecb, REML=FALSE)
anova(crtdegsecb.mod.full, crtdegsecb.ri)
## only species intercept is better
## add trial date intercept
crtdegsecb.ri2 <- lmer (crt.degsecb ~ tdensity_s*mratio_s + 
                       humidity_s*(mratio_s + tdensity_s)+
                       temp_s*(mratio_s + tdensity_s)+
                      (1|sp.name) + (1|trial.date), data=resca_degsecb, REML=FALSE)
anova(crtdegsecb.ri, crtdegsecb.ri2) 
## not better

## add density as random slope
crtdegsecb.rsi <- lmer (crt.degsecb ~ tdensity_s*mratio_s + 
                      humidity_s*(mratio_s + tdensity_s)+
                     temp_s*(mratio_s + tdensity_s)+
                       (1 + tdensity_s|sp.name),
                     data=resca_degsecb, REML=FALSE)
anova(crtdegsecb.ri, crtdegsecb.rsi) 
## not better

## try ratio as random slope
crtdegsecb.rsi2 <- lmer(crt.degsecb ~ tdensity_s*mratio_s + 
                      humidity_s*(mratio_s + tdensity_s) +
                       temp_s*(mratio_s + tdensity_s) +
                       (1 + mratio_s|sp.name),
                     data=resca_degsecb, REML=FALSE)
anova(crtdegsecb.ri, crtdegsecb.rsi2) 
## no need to include any random slope, only random species intercept

crtdegsecb.mod.simple <- mixed(crt.degsecb ~ mratio_s*tdensity_s + 
                    humidity_s*(mratio_s + tdensity_s)+
                    temp_s*(mratio_s + tdensity_s)+ 
                    (1|sp.name), data=resca_degsecb, REML=FALSE) 
anova(crtdegsecb.mod.simple) # drop non-sig interaction term
crtdegsecb.mod.simple <- lmer(crt.degsecb ~ mratio_s + tdensity_s +
                             humidity_s + temp_s + (1|sp.name),
                           data=resca_degsecb, REML=FALSE)
## vif
vif.mer(crtdegsecb.mod.simple) # drop mratio and mratio:tdensity
## afex::mixed see significance of effect
crtdegsecb.mod.simple <- mixed(crt.degsecb ~ tdensity_s + mratio_s +
                                 humidity_s + temp_s + (1|sp.name),
                               data=resca_degsecb, REML=FALSE)
anova(crtdegsecb.mod.simple)
print(xtable(anova(crtdegsecb.mod.simple)))
print(xtable(summary(crtdegsecb.mod.simple)$coefficients))

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

lossr.lmod <- lm(lossrate ~ total.mass, data=flam.loss)
summary(lossr.lmod) 
flam.loss$crt.lossr <- residuals(lossr.lmod)
##
######## graphical explore effect of density, ratio and weather ##########
ggplot (data=flam.loss, aes(tdensity, crt.lossr)) + geom_point()+
  geom_smooth(method="lm", se=FALSE) ## weak negative?

ggplot (data=flam.loss, aes(mratio, crt.lossr)) + geom_point()+
  geom_smooth(method="lm", se=FALSE) ## weak positive?

ggplot (data=flam.loss, aes(temp, crt.lossr)) + geom_point()+
  geom_smooth(method="lm", se=FALSE) ## no ?

ggplot (data=flam.loss, aes(humidity, crt.lossr)) + geom_point()+
  geom_smooth(method="lm", se=FALSE) ## no ?

#rescale variables
resca_lossr <- flam.loss %>% mutate_at(c("tdensity", "mratio", "humidity", "temp"),
                                      funs(s = zscore(.)))
## full model
crtlossr.mod.full <- lmer(crt.lossr ~ tdensity_s*mratio_s +
                            humidity_s*(mratio_s + tdensity_s) +
                            temp_s*(mratio_s + tdensity_s) +
                            (1 + mratio_s + tdensity_s | sp.name) +
                            (1|trial.date), data = resca_lossr, REML=FALSE)
## only species random intercept
crtlossr.ri <- lmer(crt.lossr ~ tdensity_s*mratio_s + 
                humidity_s*(tdensity_s + mratio_s) +
                 temp_s*(tdensity_s + mratio_s) +
                   (1 | sp.name), data = resca_lossr, REML = FALSE)
anova(crtlossr.mod.full, crtlossr.ri)
## only species random intercept is better

## trial date as another intercept
crtlossr.ri2 <- lmer(crt.lossr ~ tdensity_s*mratio_s + 
                    humidity_s*(tdensity_s + mratio_s) +
                    temp_s*(tdensity_s + mratio_s) +
                    (1 | sp.name) + (1 | trial.date),
                  data = resca_lossr, REML = FALSE)
anova(crtlossr.ri, crtlossr.ri2) 
## not better

## try density as slope
crtlossr.rsi <- lmer(crt.lossr ~ tdensity_s*mratio_s + 
                  humidity_s*(tdensity_s + mratio_s) +
                  temp_s*(tdensity_s + mratio_s) +
                   (1 + tdensity_s|sp.name),
                  data=resca_lossr, REML=FALSE)
anova(crtlossr.ri, crtlossr.rsi) 
## no denisty slope

## try ratio as slope
crtlossr.rsi2 <- lmer(crt.lossr ~ tdensity_s*mratio_s + 
                    humidity_s*(tdensity_s + mratio_s) +
                   temp_s*(tdensity_s + mratio_s) +
                  (1 + mratio_s|sp.name), 
                   data=resca_lossr, REML=FALSE)
anova(crtlossr.ri, crtlossr.rsi2) 
## no ratio as random slope, so only include species random intercept

crtlossr.mod.simple <- mixed(crt.lossr ~ tdensity_s*mratio_s + 
                          humidity_s*(tdensity_s + mratio_s) +
                          temp_s*(tdensity_s + mratio_s) +
                          (1|sp.name),
                        data=resca_lossr, REML=FALSE)
anova(crtlossr.mod.simple) ## drop interaction terms

crtlossr.mod.simple <- lmer(crt.lossr ~ tdensity_s + mratio_s + humidity_s +
                           temp_s + (1|sp.name),
                         data=resca_lossr, REML=FALSE)
## vif
vif.mer(crtlossr.mod.simple) ## keep both
## afex::mixed to see significance of fixed effects
crtlossr.mod.simple <- mixed (crt.lossr ~ tdensity_s + mratio_s + humidity_s +
                             temp_s + (1|sp.name),
                           data=resca_lossr, REML=FALSE)
anova(crtlossr.mod.simple) 
## marginally significant effect of density

print(xtable(anova(crtlossr.mod.simple)))
print(xtable(summary(crtlossr.mod.simple)$coefficients))
## Note: maximum biomass loss rate is negatively influenced by
## total mass (logged) only 

###### max flame height ~ total biomass, biomass density and ratio ######
######## graphical exploration #########
#ggplot(data=arc.trial, aes(total.mass, max.fh)) + geom_point() +
  #geom_smooth(method="lm", se=FALSE)
## max. flame height is saturated at higher value of total mass, not linear 
## try log transfer biomass
#ggplot(data=arc.trial, aes(logtmass, max.fh)) + geom_point() +
  #geom_smooth(method="lm", se=FALSE) ## ok positive linear

## instead of linear mod, fit a saturation function 
   #maxfh.nlmod <- nls(max.fh ~ a*total.mass/ (total.mass+b), data=arc.trial, 
  #start=list(a=100, b=5)) 
  #summary(maxfh.nlmod)
  #maxfh.predict <- predict(maxfh.nlmod, newdata=arc.trial)
  #ggplot(arc.trial, aes(total.mass, max.fh)) + geom_point(size=1.5) +
  #geom_line(aes(total.mass, maxfh.predict), size=1.5, color="black") 
## take out effect of logged biomass on max flame height
#maxfh.lmod <- lm(max.fh ~ logtmass, data=arc.trial)
#summary(maxfh.lmod)
#arc.trial$crt.maxfh <- residuals(maxfh.lmod)

#### graphical exploration of ratio and density effect ####

#ggplot(arc.trial, aes(tdensity, crt.maxfh)) + geom_point() +
  #geom_smooth(method="lm", se=FALSE) ## weak negative?

#ggplot(arc.trial, aes(mratio, crt.maxfh)) + geom_point() +
  #geom_smooth(method="lm", se=FALSE) ## NO

#ggplot(arc.trial, aes(temp, crt.maxfh)) + geom_point() +
  #geom_smooth(method="lm", se=FALSE) ## weak positive?

#ggplot(arc.trial, aes(humidity, crt.maxfh)) + geom_point() +
  #geom_smooth(method="lm", se=FALSE) ## weak negative?

# rescale variables
#resca_maxfh <- arc.trial %>% mutate_at(c("tdensity", "mratio", "humidity", "temp"), 
                                              #funs( s = zscore(.)))
## only include species as random intercept 
#maxfh.ri <- lmer(crt.maxfh ~ tdensity_s*mratio_s + humidity_s*(tdensity_s + mratio_s) +
                 #temp_s*(tdensity_s + mratio_s) +
                   #(1 | sp.name),data=resca_maxfh, REML=FALSE)
## trial date as another intercept
#maxfh.ri2 <- lmer(crt.maxfh ~ tdensity_s*mratio_s + humidity_s*(tdensity_s + mratio_s) +
                    #temp_s*(tdensity_s + mratio_s) +
                    #(1 | sp.name) + (1 | trial.date),
                   #data=resca_maxfh, REML=FALSE)
#anova(maxfh.ri, maxfh.ri2) ## no need to add

## add tdensity as random slope

#maxfh.rsi <- lmer(crt.maxfh ~ tdensity_s*mratio_s + humidity_s*(tdensity_s + mratio_s) +
                  #temp_s*(tdensity_s + mratio_s) +
                    #( 1 + tdensity_s | sp.name), data=resca_maxfh, REML=FALSE)
#anova(maxfh.ri, maxfh.rsi) ## no need to add tdensity as slope
## try ratio as slope
#maxfh.rsi2 <- lmer(crt.maxfh ~ tdensity_s*mratio_s + humidity_s*(tdensity_s + mratio_s) + 
                  #temp_s*(tdensity_s + mratio_s) +
                     #( 1 + mratio_s | sp.name), data=resca_maxfh, REML=FALSE)
#anova(maxfh.ri, maxfh.rsi2) ## ok, no slope

#crtmaxfh.mod.full <- mixed(crt.maxfh ~ tdensity_s*mratio_s + 
                          #humidity_s*(tdensity_s + mratio_s) +
                          #temp_s*(tdensity_s + mratio_s) + (1|sp.name),
                          #data= resca_maxfh, REML=FALSE) 
#anova(crtmaxfh.mod.full) 
## drop interaction terms 
#crtmaxfh.mod.simple <- lmer(crt.maxfh ~ tdensity_s +  temp_s + mratio_s + humidity_s +
                           #(1|sp.name), data=resca_maxfh, REML=FALSE)
## vif test and drop variable with vif >3

#vif.mer(crtmaxfh.mod.simple) ## no need to drop any here

## look at significance
#crtmaxfh.mod.simple <- mixed (crt.maxfh ~ temp_s + tdensity_s +  mratio_s + humidity_s +
                        #(1 |sp.name), data=resca_maxfh, REML=FALSE)
#anova(crtmaxfh.mod.simple) ## no canopy trait effects


## Note: max flame height is positively influenced by fuel load only
