###lmer-mods.R
### R script used for looking if measured plant canopy traits(biomass density, 
### biomass ratio) influence grass flammability after account for total biomass
### effect

library(dplyr)
library(afex)
library(pcaMethods)

# Look for additional grass trait that may influence flammability besides total biomass.
# Steps are as follows:
# 1. Look at how total biomass influence flammability with only including total
#    biomass as predictor to build either linear or non-linear model.
# 2. Take residuals from model built at step one, it'll be biomass-corrected flammability.
# 3. Build first-principle model with including both canopy traits 
#    and relative humidity as predictors for biomass-corrected flammability.
# 4. Compare model from step 3 to other models with same fixed effect but different
# random effect and select model that has lowest AIC value
# 5. look at sigificance of fixed effect with afex::mixed

## data loading and plot theme set up
source("./hobo-temps.R") #temp summaries for each trial
source("./burning_trial_summaries.R") #mass loss rate for each trial
source("./biomass_density_prediction.R")# architecture data
source("./ggplot-theme.R") 
color <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
           "#0072B2", "#D55E00", "#CC79A7")

#throw away intervals from trials, since it 
#does not work when join trials with other dataset(can't view file),
#it is not necessary to keep it anyway 

trials <- select(trials, -interval)
trials <- trials[order(trials$label), ]

#1. data frame has all measurements for each observation, observation is in 
# row, each column is a measurement type

alldata <- left_join(trials, mass.density, by=c("label",
           "pair", "treatment", "sp.cd", "sp.name")) %>% 
  left_join(tempsum.base, by="trial.id") %>% 
  left_join(tempsum.above, by="trial.id") %>%
  left_join(flamlossr, by=c("label", "sp.cd", "sp.name"))

############### dimension of grass flammability ################
## apply principal component analysis to selected flammability measurement 
## for further analysis

flamabove.PCA <- alldata %>%
  select (dur.above, lossrate, massloss, degsec.above) %>% 
  pca(nPcs=4, method="ppca",seed=100, center=TRUE,scale="uv")

summary(flamabove.PCA) 
loadings(flamabove.PCA)

## total heat release and rate of heat release are independent axes

biplot(flamabove.PCA, choices = c("PC1","PC2"))

flambase.PCA <- alldata %>% 
  select(dur.base, lossrate, massloss, degsec.base) %>% 
  pca(nPcs=4, method="ppca", center=TRUE, scale="uv")  

summary(flambase.PCA)
loadings(flambase.PCA)
biplot(flambase.PCA)
### result is same when use temperature data at soil surface or at 25 cm 

#### integrated temp above 100 ~ total biomass, biomass ratio and density ####

## temperature and relative humidity are highly correlated (cor=-0.7)
## will not include both in model. due to daily variation in humidity 
## is larger than seasonal variation among trial days, 
## only include relative humidity.

##### graphical exploration #####
ggplot(filter(alldata, !is.na(degsec.above)), 
       aes(total.mass, degsec.above, color=sp.name)) + geom_point() +
  bestfit + scale_color_manual(values=color) 
## positive linear relationship, same pattern within and across species

# take biomass effect out
degseca.lmod <- lm(degsec.above ~ total.mass, data=alldata)
# plot(degseca.lmod) # observation 82 has a cook's D >0.5, which
# is due to the extreme value of biomass in ERCU2
# analysis is performed with and without this extremme case
# same result obtained, decided to include extreme value

summary(degseca.lmod) #sig. biomass effect
# take residuals
temp.above <- filter(alldata, !is.na(degsec.above))
temp.above$crt.degseca <- residuals(degseca.lmod)

##### graphical exploration to see trait and weather effect #####

ggplot(temp.above, aes(mratio, crt.degseca, color=sp.name)) + geom_point() + 
 geom_smooth(method="lm", se=FALSE, aes(color=sp.name)) +
   bestfit + scale_color_manual(values=color) 
# negative?  
ggplot(temp.above, aes(tdensity, crt.degseca, color=sp.name)) + geom_point() +
  geom_smooth(method="lm", se=FALSE, aes(color=sp.name)) +
  bestfit + scale_color_manual(values=color)
# positive?
ggplot(temp.above, aes(humidity, crt.degseca, color=sp.name)) + geom_point() + 
  bestfit + scale_color_manual(values=color) 
# weak positive?!

# rescale numeric variables 
zscore <- function(x) (x - mean(x)) / sd(x)  
resca_degseca <- temp.above %>% mutate_at(c("tdensity", "mratio", "humidity"),
                                       funs(s = zscore(.)))

crtdegseca.mod <- mixed(crt.degseca ~ mratio_s*tdensity_s + 
                                humidity_s + (1 |sp.name), 
                              data= resca_degseca, method = "KR",
                        REML=FALSE)
crtdegseca.mod.rsi <- mixed(crt.degseca ~ mratio_s*tdensity_s + humidity_s +
                             (1 + mratio_s | sp.name), data = resca_degseca,
                            method = "KR", REML = FALSE)
anova(crtdegseca.mod, crtdegseca.mod.rsi)

crtdegseca.mod.rsi2 <- mixed(crt.degseca ~ mratio_s*tdensity_s + humidity_s +
                              (1 + tdensity_s | sp.name), 
                            data = resca_degseca,method = "KR",
                            REML=FALSE)
anova(crtdegseca.mod, crtdegseca.mod.rsi2)
crtdegseca.mod.rsi3 <- mixed(crt.degseca ~ mratio_s*tdensity_s + humidity_s +
                              (1 + humidity_s | sp.name), 
                               data = resca_degseca, 
                             method = "KR", REML = FALSE)
anova(crtdegseca.mod, crtdegseca.mod.rsi3) ## need include humidity as slope

## significance of fixed effects using afex::mixed
crtdegseca.mod.final <- mixed (crt.degseca ~ mratio_s*tdensity_s + 
                          humidity_s + (1 + humidity_s | sp.name), 
                        data= resca_degseca, method = "KR",
                        REML=FALSE)

anova(crtdegseca.mod.final) 
summary(crtdegseca.mod.final)
# no architecture effect

## Note: temperature integration (>100 degree) above 10cm location was positively 
## influenced by total biomass alone

##### integrated temp (>100) <10cm ~ total biomass, biomass ratio and density ##### 
######### graphical exploration #########

ggplot(filter(alldata, !is.na(degsec.base)), aes(total.mass, degsec.base, 
     color=sp.name)) + geom_point() + 
  bestfit + scale_color_manual(values=color)
## positive linear relationship 

## build linear model and take residuals
degsecb.lmod <- lm(degsec.base ~ total.mass, data= alldata)
## result kept same with or without observation 82 (influential point)

summary(degsecb.lmod) #sig. biomass effec

temp.base <- filter(alldata, !is.na(degsec.base))
temp.base$crt.degsecb <- residuals(degsecb.lmod)

##### graphical exploration ######
ggplot(temp.base, aes(tdensity, crt.degsecb, color=sp.name)) + geom_point()+
  geom_smooth(method="lm", se=FALSE, aes(color=sp.name)) + 
  bestfit + scale_color_manual(values=color)
## positive response across species but varying within species response? 

ggplot (temp.base, aes(mratio, crt.degsecb, color=sp.name)) + geom_point()+
  geom_smooth(method = "lm", se=FALSE, aes(color=sp.name)) +
  bestfit + scale_color_manual(values=color) 
## negative response across species but varying within species response?
ggplot (temp.base, aes(humidity, crt.degsecb, color=sp.name)) + geom_point()+
  bestfit + scale_color_manual(values=color) 
## positive?!

## rescale numeric variables
resca_degsecb <- temp.base %>% mutate_at(c("tdensity", "mratio", "humidity"),
                                       funs(s = zscore(.)))

crtdegsecb.mod <- mixed(crt.degsecb ~ mratio_s*tdensity_s +
                              humidity_s + (1|sp.name), 
                       data=resca_degsecb, REML = FALSE) 

crtdegsecb.mod.rsi <- mixed(crt.degsecb ~ mratio_s*tdensity_s +
                             humidity_s + (1 + tdensity_s | sp.name),
                           data = resca_degsecb, REML = FALSE)
# model failed to coverge, bad fit

crtdegsecb.mod.rsi2 <- mixed(crt.degsecb ~ mratio_s*tdensity_s +
                              humidity_s + (1 + mratio_s | sp.name),
                            data = resca_degsecb, REML = FALSE)
# model failed to converge
crtdegsecb.mod.rsi3 <- mixed(crt.degsecb ~ mratio_s*tdensity_s +
                              humidity_s + (1 + humidity_s | sp.name),
                            data = resca_degsecb, REML = FALSE)
# model failed to converge, so decide not to include any slope

crtdegsecb.mod.final <- mixed(crt.degsecb ~ tdensity_s*mratio_s + 
                                humidity_s + (1 | sp.name), 
                              data = resca_degsecb, REML=FALSE)
anova(crtdegsecb.mod.final)
summary(crtdegsecb.mod.final)

##significant negative effect of mratio on degsec<10cm
## Note: integrated temperature (>100 degree) <10cm were positively influenced by 
## total biomass and nagetively influenced by mratio

##### maximum loss rate ~ total biomass, biomass density and ratio ######

ggplot(filter(alldata, !is.na(lossrate)), 
  aes(total.mass, lossrate, color=sp.name)) + 
  geom_point() + 
  bestfit + scale_color_manual(values=color)

lossr.lmod <- lm(lossrate ~ total.mass, data=alldata)
summary(lossr.lmod) 
flam.loss <- filter(alldata, !is.na(lossrate))
flam.loss$crt.lossr <- residuals(lossr.lmod)
##
######## graphical explore effect of density, ratio and weather ##########
ggplot (data=flam.loss, aes(tdensity, crt.lossr, color=sp.name)) + 
  geom_point()+ geom_smooth(method="lm", se=FALSE, aes(color=sp.name))+
  bestfit + scale_color_manual(values=color)
## negative?

ggplot (data=flam.loss, aes(mratio, crt.lossr, color=sp.name)) + 
  geom_point()+ geom_smooth(method="lm", se=FALSE, aes(color=sp.name))+
  bestfit + scale_color_manual(values=color)
## weak positive?

ggplot (data=flam.loss, aes(humidity, crt.lossr, color=sp.name)) + 
  geom_point()+ geom_smooth(method="lm", se=FALSE, aes(color=sp.name))+
  bestfit + scale_color_manual(values=color)
## no ?

#rescale variables
resca_lossr <- flam.loss %>% mutate_at(c("tdensity", "mratio", "humidity"),
                                      funs(s = zscore(.)))

crtlossr.mod <- mixed(crt.lossr ~ mratio_s*tdensity_s + humidity_s +
                        (1 | sp.name), data = resca_lossr, REML = FALSE)
crtlossr.mod.rsi <- mixed(crt.lossr ~ mratio_s*tdensity_s + humidity_s +
                            (1 + tdensity_s | sp.name), 
                          data = resca_lossr, REML = FALSE)
anova(crtlossr.mod, crtlossr.mod.rsi) # no slope

crtlossr.mod.rsi2 <- mixed(crt.lossr ~ mratio_s*tdensity_s + humidity_s +
                            (1 + mratio_s | sp.name), 
                          data = resca_lossr, REML = FALSE)
anova(crtlossr.mod, crtlossr.mod.rsi2) # no slope
crtlossr.mod.rsi3 <- mixed(crt.lossr ~ mratio_s*tdensity_s + humidity_s +
                                                 (1 + humidity_s | sp.name), 
                           data = resca_lossr, REML = FALSE)
anova(crtlossr.mod, crtlossr.mod.rsi3) # no slope

crtlossr.mod.final <- mixed(crt.lossr ~ mratio_s*tdensity_s +
                           + humidity_s + (1|sp.name),
                         data=resca_lossr, REML=FALSE)
anova(crtlossr.mod.final)
summary(crtlossr.mod.final)
## note: max biomass loss rate is only negetaively influenced by 
## total above ground biomass, no architecture effect or weather effect
## is detected