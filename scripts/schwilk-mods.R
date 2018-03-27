## 2018-03-27

library(lme4)
library(afex)
library(pcaMethods)
library(xtable)

source("./final_summary_dataset.R")
source("./ggplot-theme.R") 

color <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
           "#0072B2", "#D55E00", "#CC79A7")


### result is same when use base heating

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

ggplot(temp.above, aes(mratio, crt.degseca)) + geom_point(aes(color=sp.name)) +
  geom_smooth(aes(color=sp.name), method="lm", se=FALSE) +
  geom_smooth(method="lm") +
  scale_color_manual(values=color) 
# negative?  
ggplot(temp.above, aes(tdensity, crt.degseca)) + geom_point(aes(color=sp.name)) +
  geom_smooth(aes(color=sp.name), method="lm", se=FALSE) +
  geom_smooth(method="lm") +
  scale_color_manual(values=color) 

# positive?
ggplot(temp.above, aes(humidity, crt.degseca)) + geom_point(aes(color=sp.name)) +
  geom_smooth(aes(color=sp.name), method="lm", se=FALSE) +
  geom_smooth(method="lm") +
  scale_color_manual(values=color) 

ggplot(temp.above, aes(temp, crt.degseca)) + geom_point(aes(color=sp.name)) +
  geom_smooth(aes(color=sp.name), method="lm", se=FALSE) +
  geom_smooth(method="lm") +
  scale_color_manual(values=color) 
# weak negative?  NO, positive within species

# rescale numeric variables to fit a full model
zscore <- function(x) (x - mean(x)) / sd(x)  
resca_degseca <- temp.above %>% mutate_at(c("tdensity", "mratio", "temp", "humidity"),
                                       funs(s = zscore(.)))

# full model, ommitting 3-way interaction (not sig anyway)
mod1 <- mixed(crt.degseca ~ mratio_s*tdensity_s + humidity_s +
                              (1 | sp.name),
                            data = resca_degseca, REML = FALSE)

summary(mod1)
anova(mod1)

## So no effect of architecture other than hunmidity effect is greater (more
## neg) at higher tdensity

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
ggplot(temp.base, aes(tdensity, crt.degsecb, color=sp.name)) + geom_point() +
  bestfit + scale_color_manual(values=color) + geom_smooth(method="lm", se=FALSE)
## positive 

ggplot(temp.base, aes(mratio, crt.degsecb)) + geom_point(aes(color=sp.name))+
  scale_color_manual(values=color) + geom_smooth(method="lm")
## negative
ggplot(temp.base, aes(humidity, crt.degsecb, color=sp.name)) + geom_point()+
  bestfit + scale_color_manual(values=color) + geom_smooth(method="lm", se=FALSE)
## positive?
ggplot (temp.base, aes(temp, crt.degsecb, color=sp.name)) + geom_point()+
  bestfit + scale_color_manual(values=color) + geom_smooth(method="lm", se=FALSE)
## negative?
## rescale numeric variables
resca_degsecb <- temp.base %>% mutate_at(c("tdensity", "mratio", "humidity", "temp"),
                                       funs(s = zscore(.)))
## full model
crtdegsecb.mod.full <- mixed(crt.degsecb ~ tdensity_s*mratio_s*temp_s -
                              tdensity_s:mratio_s:temp_s +
                              (1 + temp_s | sp.name),
                            data=resca_degsecb, REML = FALSE)

crtdegsecb.mod.ri <- mixed(crt.degsecb ~ tdensity_s*mratio_s*temp_s -
                              tdensity_s:mratio_s:temp_s +
                              (1 | sp.name),
                            data=resca_degsecb, REML = FALSE)


x <- mixed(crt.degsecb ~ tdensity_s*mratio_s + humidity_s +
                              (1 | sp.name),
                            data=resca_degsecb, REML = FALSE)



anova(x)
summary(x)


resca_degsecb <- resca_degsecb %>% mutate(date2=mdy(trial.date))
ggplot(resca_degsecb, aes(date2, humidity, color =sp.name)) + geom_jitter()


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
crtlossr.mod.full <- mixed(crt.lossr ~ tdensity_s*mratio_s*humidity_s +
                            (1 | sp.name),
                          data = resca_lossr, REML=FALSE)
anova(crtlossr.mod.full)

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
