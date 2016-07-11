# burning_trial_summaries.R
#

library(broom)
library(lme4)
library(brms) # Bayesian model fititng via stan and rstan

source("./read_balance_data.R")


# first, some very basic summary values per trial
balance_sum <- balance_data %>% group_by(label, trial, utrial, sp.cd) %>%
  summarize(balance.initial = mean(mass[nsec<40]),
            balance.final = mean(mass[nsec > (max(nsec) - 30)]),
            balance.burned = balance.initial - balance.final) %>%
  left_join(burns)


##########################################################################
## Temporary approach 1: separate models per trial
bytrial <- balance_data %>% group_by(label, sp.cd, utrial)

flamingMods <- bytrial %>% filter(is.flaming) %>%
  do(flamingMod = lm(log(mass) ~ nsec, data = .))
smolderingMods <- bytrial %>% filter(is.smoldering) %>%
  do(smolderingMod = lm(log(mass) ~ nsec, data = .))

# use broom::tidy to grab coefficents easily
flamingModsCoef <- tidy(flamingMods, flamingMod) %>% filter(term=="nsec")
smolderingModsCoef <- tidy(smolderingMods, smolderingMod)  %>% filter(term=="nsec")

ggplot(flamingModsCoef, aes(sp.cd, estimate)) + geom_violin()
ggplot(smolderingModsCoef, aes(sp.cd, estimate)) + geom_violin()


##########################################################################
## Approach 2: mixed linear models

# Run model with sp.cd and trial as factors
flame.mod <- lmer(log(mass) ~ nsec*sp.cd + (1 + nsec|label),
                      data = filter(balance_data, is.flaming))
flame.mod.null <- lmer(log(mass) ~ nsec + (1 + nsec|label),
                       data = filter(balance_data, is.flaming))

anova(flame.mod.null, flame.mod)
# so species matters

summary(flame.mod)

##########################################################################
## Approach 3: Same as 2 but full bayesian. Use rstan via brms. SLOW! But good
## coefficient estimates. Currently does not converge.

#flame.mod.bayes <- brm(log(mass) ~ nsec*sp.cd + (1 + nsec|label),
#                       data = filter(balance_data, is.flaming))

# save this!
#saveRDS(flame.mod.bayes, "../results/flame_mod_bayes.rds")

#summary(flame.mod.bayes)


# flaming and smoldering:
comb.mod.bayes <- brm(log(mass) ~ nsec*sp.cd + (1 + nsec|label),
                       data = filter(balance_data, is.flaming | is.smoldering))

# save this!
saveRDS(comb.mod.bayes, "../results/flame_mod_bayes.rds")

summary(comb.mod.bayes)
