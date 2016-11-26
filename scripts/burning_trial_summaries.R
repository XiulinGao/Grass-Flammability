# burning_trial_summaries.R
#

library(broom)
library(lme4)
library(brms) # Bayesian model fititng via stan and rstan
library(dplyr)

source("./read_balance_data.R")


# first, some very basic summary values per trial
balance_sum <- balance_data %>% group_by(label, trial, utrial, sp.cd) %>%
  summarize(balance.initial = mean(mass[nsec<45]),
            balance.final = mean(mass[nsec > (max(nsec) - 30)]),
            balance.burned = balance.initial - balance.final) %>%
  left_join(trials) 
# plot to see if balance burned mass deviates from record burned mass significantly
## ggplot(balance_sum, aes(balance.burned, initial.mass - final.mass - fuel.residual, color=sp.cd)) +
## geom_point() + geom_abline(intercept=0, slope=1)

##########################################################################
## Temporary approach 1: separate models per trial
bytrial <- balance_data %>% group_by(label, sp.cd, utrial)

flamingMods <- bytrial %>% filter(is.flaming) %>%
  do(flamingMod = lm(mass ~ nsec, data = .)) 
smolderingMods <- bytrial %>% filter(is.smoldering) %>%
  do(smolderingMod = lm(mass ~ nsec, data = .))

# use broom::tidy to grab coefficents easily
flamingModsCoef <- tidy(flamingMods, flamingMod) %>% 
  filter(term=="nsec",  p.value < 0.01) %>%#throw non-sig
  filter(estimate<0)
  colnames(flamingModsCoef)[5] <- "lossrate" #change estimate to lossrate
  
smolderingModsCoef <- tidy(smolderingMods, smolderingMod)  %>% 
  filter(term=="nsec",p.value <0.01) %>%#throw non-sig
  filter(estimate<0)
colnames(smolderingModsCoef)[5] <- "lossrate"
#Mods does not contain all burns. not included burns are "ec27-04"  "bi01-09"  "erc27-04" "erc27-01" "apn09-16"
#Need to check out reason: ok, ec27 just has 6 observation, the others have no ignition time recorded

#ggplot(flamingModsCoef, aes(sp.cd, estimate)) + geom_violin()
#ggplot(smolderingModsCoef, aes(sp.cd, estimate)) + geom_violin()

##########################################################################
## Approach 2: mixed linear models

# Run model with sp.cd and trial as factors
##flame.mod <- lmer(log(mass) ~ nsec*sp.cd + (1 + nsec|label),
                      #data = filter(balance_data, is.flaming))
##flame.mod.null <- lmer(log(mass) ~ nsec + (1 + nsec|label),
                       #data = filter(balance_data, is.flaming))

##anova(flame.mod.null, flame.mod)
# so species matters

##summary(flame.mod)

