# burning_trial_summaries.R
#

library(broom)
library(lme4)
#library(brms) # Bayesian model fititng via stan and rstan
library(dplyr)

source("./read_balance_data.R")


# first, some very basic summary values per trial
balance_sum <- balance_data %>% group_by(label, trial, utrial, sp.cd) %>%
  summarize(balance.initial = mean(mass[nsec<40]),
            balance.final = mean(mass[nsec > (max(nsec) - 10)]),
            balance.burned = balance.initial - balance.final) %>%
  left_join(trials) 

# plot to see how calculated biomass loss match to measured biomass loss during 
# each burning trial
massmatch <- balance_sum %>% select(label, trial, utrial, balance.burned, mconsum)
ggplot(massmatch, aes(mconsum, balance.burned)) + geom_point() + geom_abline(slope=1, intercept=0)+
  labs(x="measured mass loss", y="calcualted mass loss")

# plot to see if balance burned mass deviates from record burned mass significantly
## ggplot(balance_sum, aes(balance.burned, initial.mass - final.mass - fuel.residual, color=sp.cd)) +
## geom_point() + geom_abline(intercept=0, slope=1)

##########################################################################
## Temporary approach 1: separate models per trial
bytrial <- balance_data %>% group_by(label, sp.cd, utrial)

flambytrial <- bytrial %>% filter(is.flaming) %>%
  mutate(decaymass=mass-min(mass))

flamingMods <- flambytrial %>% group_by(label, sp.cd, utrial) %>%
  do(flamingMod = lm(mass ~ nsec, data = .)) 

#smolderingMods <- bytrial %>% filter(is.smoldering) %>%
  #do(smolderingMod = lm(mass ~ nsec, data = .))

# use broom::tidy to grab coefficents easily
flamingModsCoef <- tidy(flamingMods, flamingMod)
#%>% 
#filter(term=="nsec",  p.value < 0.01) %>%#throw non-sig
#filter(estimate<0)
#colnames(flamingModsCoef)[5] <- "lossrate" #change estimate to lossrate
  
#smolderingModsCoef <- tidy(smolderingMods, smolderingMod)  %>% 
  #filter(term=="nsec",p.value <0.01) %>%#throw non-sig
  #filter(estimate<0)
#colnames(smolderingModsCoef)[5] <- "lossrate"
#Mods does not contain all burns. not included burns are "ec27-04"  "bi01-09"  "erc27-04" "erc27-01" "apn09-16"
#Need to check out reason: ok, ec27 just has 6 observation, the others have no ignition time recorded

#ggplot(flamingModsCoef, aes(sp.cd, estimate)) + geom_violin()
#ggplot(smolderingModsCoef, aes(sp.cd, estimate)) + geom_violin()

## Approach 2: exponential decay models

decayID <- sort(unique(flambytrial$utrial))
nlaics <- numeric(length(decayID))

flamingNLModsCoef <- data.frame(label=character(),
                          sp.cd=character(),
                          utrial=character())

for (i in 1:length(decayID)) {
  
  subdecay <- filter(flambytrial, utrial==decayID[i])
  flamingNLMod <- nls(decaymass ~ a*exp(b*nsec), data=subdecay, 
                     start=list(a=subdecay$decaymass[1],b=0))
  nlaics[i] <- AIC(flamingNLMod) #calculate AIC for each fit
  mod_coef <- tidy(flamingNLMod) #get model coef as data frame
  mod_coef$label <- subdecay$label[1]
  mod_coef$sp.cd <- subdecay$sp.cd[1]
  mod_coef$utrial <- decayID[i]
  mod_coef <- mod_coef[, c(6:8, 1:5)]
  flamingNLModsCoef<- rbind(flamingNLModsCoef, mod_coef)
}
flamlossr <- flamingNLModsCoef %>% filter(term=="b") %>% 
  filter(estimate<0) %>% filter(p.value<0.01)
colnames(flamlossr)[5] <- "lossrate"
flamlossr <- flamlossr %>% mutate(lossrate=abs(lossrate))
##compare AIC for linear and non-linear mods
laics <- sapply(flamingMods$flamingMod, AIC)
plot(nlaics ~ laics)

#clean up env

rm("massmatch", "subdecay", "mod_coef", "decayID", "i")

## plot fitted value from both linear and non-linear fit to see how they 
## match the original data


##########################################################################
## Approach 3: mixed linear models

# Run model with sp.cd and trial as factors
##flame.mod <- lmer(log(mass) ~ nsec*sp.cd + (1 + nsec|label),
                      #data = filter(balance_data, is.flaming))
##flame.mod.null <- lmer(log(mass) ~ nsec + (1 + nsec|label),
                       #data = filter(balance_data, is.flaming))

##anova(flame.mod.null, flame.mod)
# so species matters

##summary(flame.mod)

