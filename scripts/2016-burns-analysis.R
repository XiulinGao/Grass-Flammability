###2016-burns-analysis.R

library(pcaMethods)
library(car)
library(lme4)

## data loading and plot theme set up

source("./final_summary_dataset.R")
source("./ggplot-theme.R") # duplicate from Schwilk, made minor edit to font size

species <- c("Agropyron smithii", "Aristida purpurea", "Bromus inermis",
             "Chasmanthium latifolium", "Elymus longifolius", "Eragrostis curvula",
             "Panicum anceps", "Stipa neomexicana")

color <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
           "#0072B2", "#D55E00", "#CC79A7")

## 1. PCA for flammability measurements

flamabove.PCA <- temp.above %>%
  select(dur, lossrate, massloss, degsec, max.fh) %>%
  pca(nPcs=4, method="ppca", center=TRUE, scale="uv")

flamabove.loads <- as.data.frame(loadings(flamabove.PCA))
flamabove.scores <- as.data.frame(scores(flamabove.PCA)) %>% 
  subset(select=c("PC1","PC2"))
summary(flamabove.PCA) # first two axes explained 80% of total variance

biplot(flamabove.PCA, xlab = "Principle component 1",
       ylab = "Principle component 2") 

## 2. total biomass and biomass ratio influence on duration of heating 

ggplot(temp.above, aes(total.mass, dur)) + geom_point() +
  bestfit + 
  labs( y="Duration of heating above 10cm (s)", x="Total above-ground biomass (g)") + 
  scale_x_continuous(limits=c(0.001, 360), expand=c(0,0)) + 
  scale_y_continuous(limits=c(0, 800), expand=c(0,0)) + pubtheme.nogridlines 

tm_duraLMod <- lm(dur ~ total.mass, data=temp.above)
summary(tm_duraLMod) #sig. biomass effec

temp.above$adj_dura <- residuals(tm_duraLMod)

# species mean
duramean <- temp.above %>% group_by(sp.cd) %>% summarise(mratio=mean(mratio),
                                                         adj_dura = mean(adj_dura))

ggplot(temp.above, aes(mratio, adj_dura, color=sp.cd)) + 
  geom_point( size= 0.5, alpha= 0.4)+
  scale_color_manual(label=species, values=color) + 
  bestfit  +
  geom_point(data=duramean, size=2.0, alpha=1) + 
  scale_y_continuous(limits = c(-200, 200)) +
  labs( y="Biomass corrected duration above 10cm (s)", 
        x="Biomass ratio", color="\n") + pubtheme.nogridlines

ratio_duraLMM <- lmer(adj_dura ~ mratio + (mratio|sp.cd), 
                      data=temp.above, REML=FALSE)
summary(ratio_duraLMM)
duraNull <- lmer(adj_dura ~ 1 + (mratio|sp.cd), data=temp.above, REML=F)
anova(ratio_duraLMM, duraNull)# sig. biomass ratio influence on duration 
                              # of heating at >10cm location, also looked for 
                              # density, however, no sig. effect

##3. total biomass and biomass ratio influence on duration of heating <10cm

ggplot(temp.below, aes(total.mass, dur)) + geom_point() +
  bestfit + 
  labs( y="Duration of heating below 10cm (s)", x="Total above-ground biomass (g)") + 
  scale_x_continuous(limits=c(0.001, 360), expand=c(0,0)) + 
  scale_y_continuous(limits=c(0, 1500),expand=c(0,0)) + pubtheme.nogridlines 

tm_durbLMod <- lm(dur ~ total.mass, data=temp.below)
summary(tm_durbLMod) #sig. biomass effec

temp.below$adj_durb <- residuals(tm_durbLMod)

# species mean
durbmean <- temp.below %>% group_by(sp.cd) %>% summarise(mratio=mean(mratio),
                                                         adj_durb = mean(adj_durb))

ggplot(temp.below, aes(mratio, adj_durb, color=sp.cd)) + 
  geom_point( size= 0.5, alpha= 0.4)+
  scale_color_manual(label=species, values=color) + 
  bestfit  +
  geom_point(data=durbmean, size=2.0, alpha=1) + 
  scale_y_continuous(limits = c(-600, 600)) +
  labs( y="Biomass corrected duration above 10cm (s)", 
        x="Biomass ratio", color="\n") + pubtheme.nogridlines

ratio_durbLMM <- lmer(adj_durb ~ mratio + (mratio|sp.cd), 
                      data=temp.below, REML=FALSE)
summary(ratio_durbLMM)
durbNull <- lmer(adj_durb ~ 1 + (mratio|sp.cd), data=temp.below, REML=F)
anova(ratio_durbLMM, durbNull) # sig. mass ratio influence on duration
                               # of heating below 10cm, no density10 influence

##4. total biomass and biomass density influence on maximum loss rate

ggplot(data=flam.loss, aes(tdensity, lossrate, color=sp.cd)) + geom_point()+
  geom_smooth(method="lm", se=F) +
  labs(y= expression("Biomass loss rate" ~ (s^{-1})), 
       x=expression("Density" ~ (g/cm^{-3})), 
       color="\n") + scale_color_manual(labels=species, values=color)+
  scale_y_continuous(expand = c(0,0), limits=c(0.01, 0.15)) +
  scale_x_continuous(expand = c(0,0))+ pubtheme.nogridlines # neither density nor total
# biomass alone can explain loss rate

lossrFuLMM <- lmer(lossrate ~ tdensity + total.mass +
                       (total.mass|sp.cd) + (tdensity|sp.cd), 
                     data=flam.loss, REML=FALSE) #getting warning

summary(lossrFuLMM)

denNull <- lmer(lossrate ~ total.mass + (total.mass|sp.cd)+
                  (tdensity|sp.cd),
                data=flam.loss, REML=FALSE)
anova(lossrFuLMM, denNull) # density has sig. negative influence on loss rate

tmassNull <- lmer(lossrate ~ tdensity + (tdensity|sp.cd) + (total.mass|sp.cd),
                   data=flam.loss, REML=FALSE) #getting warning

anova(lossrFuLMM, tmassNull) # total biomass has no influence on loss rate

##5. total biomass and biomass density influence on max flame height

ggplot(data=arc.trial, aes(total.mass, max.fh, color=sp.cd)) + geom_point()+
  geom_smooth(method="lm", se=F) +
  labs(y= "Maximum flame height (cm)", 
       x= "Total above groud biomass (g)", 
       color="\n") + scale_color_manual(labels=species, values=color)+
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0))+ pubtheme.nogridlines 

 maxfhFuLMM <- lmer( max.fh ~ total.mass + tdensity + (total.mass|sp.cd) +
                       (tdensity|sp.cd), data=arc.trial, REML=FALSE)
summary(maxfhFuLMM)

fhtmassNull <- lmer(max.fh ~ tdensity + (total.mass|sp.cd) +
                      (tdensity|sp.cd), data=arc.trial, REML=FALSE)
anova(maxfhFuLMM, fhtmassNull) #total mass has significant positive 
                               #influence on max. flame height
fhdenNull <- lmer(max.fh ~ total.mass + (total.mass|sp.cd) +
                    (tdensity|sp.cd), data=arc.trial, REML=FALSE)
anova(maxfhFuLMM, fhdenNull) #density has sig. (p<0.05) negative
                             # influence on max. flame height

##6. total biomass and biomass ratio influence on mass loss

ggplot(data=arc.trial, aes(total.mass, mconsum)) + geom_point()+
  bestfit +
  labs(y= "Mass loss (g)", x="Total above ground biomass (g)", 
       color="\n") + scale_color_manual(labels=species, values=color)+
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0))+ pubtheme.nogridlines
                             
tmlossLM <- lm(mconsum ~ total.mass, data=arc.trial)
summary(tmlossLM)

arc.trial$adj_mloss <- residuals(tmlossLM)

#group mean
mlossgm <- arc.trial %>% group_by(sp.cd) %>%
  summarise(mratio=mean(mratio), adj_mloss=mean(adj_mloss))
  
ggplot(data=arc.trial, aes(mratio, adj_mloss, color=sp.cd)) + 
  geom_point(size=0.5, alpha=0.4)+ bestfit + 
  labs(y= "Biomass corrected mass loss (g)",
       x="Biomass ratio",color="\n") + 
  scale_color_manual(labels=species, values=color) + 
  geom_point(data=mlossgm, size=2.0, alpha=1.0) + 
  scale_y_continuous(limits = c(-25, 10))+ 
  scale_x_continuous(expand= c(0,0), limits=c(0.001, 8)) + 
  pubtheme.nogridlines

ratio_mlossLMM <- lmer(adj_mloss ~ mratio + (mratio|sp.cd),
                       data=arc.trial, REML=FALSE)
summary(ratio_mlossLMM)
ratiolossNull <- lmer(adj_mloss ~ 1+(mratio|sp.cd), 
                      data=arc.trial, REML=FALSE)
anova(ratio_mlossLMM, ratiolossNull) #sig. negative influence 
