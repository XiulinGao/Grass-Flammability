###2016-burns-analysis.R

library(pcaMethods)
library(lme4)
library(AICcmodavg)

## data loading and plot theme set up

source("./final_summary_dataset.R")
source("./ggplot-theme.R") # duplicate from Schwilk, made minor edit to font size

species <- c( "Pascopyrum smithii", "Aristida purpurea", "Bromus inermis",
             "Chasmanthium latifolium", "Elymus longifolius", "Eragrostis curvula",
             "Panicum anceps",  "Hesperostipa neomexicana")

color <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
           "#0072B2", "#D55E00", "#CC79A7")


##total biomass and biomass ratio influence on duration of heating 

ggplot(temp.above, aes(total.mass, dur, color=sp.cd)) + geom_point(size=0.5) +
  bestfit + 
  labs( y="Duration of heating >10cm (s)", x="Total above ground biomass (g)",
        color="\n") + scale_color_manual(label=species, values=color) +
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
  scale_y_continuous(limits = c(-200, 160)) +
  labs( y="Biomass corrected duration >10cm(s)", 
        x="Biomass ratio", color="\n") + pubtheme.nogridlines



ratio_duraLMM <- lmer(adj_dura ~ mratio + (mratio|sp.cd), 
                      data=temp.above, REML=FALSE)
den_duraLMM <- lmer(adj_dura ~ tdensity + (tdensity|sp.cd),
                    data=temp.above, REML=FALSE)
summary(ratio_duraLMM)
summary(den_duraLMM)
duraNull <- lmer(adj_dura ~ 1 + (mratio|sp.cd), data=temp.above, REML=F)
anova(ratio_duraLMM, duraNull)# sig. biomass ratio influence on duration 
                              # of heating at >10cm location, also looked for 
                              # density, however, no sig. effect

# approach 2 : variable selection

# rescale numeric variables

zscore <- function(x) (x - mean(x)) / sd(x)  

resca_dura <- temp.above %>% mutate_at(c("total.mass", "tdensity", "mratio"),
                                       funs(s = zscore(.)))
duraFullMod <- lmer (dur ~ total.mass_s* mratio_s*tdensity_s +
                       (1 + total.mass_s + mratio_s + tdensity_s|sp.cd),
                     data=resca_dura, REML=FALSE)
mod1 <- lmer(adj_dura ~ mratio_s*tdensity_s +
               (1 + mratio_s + tdensity_s|sp.cd), data=resca_dura, REML=FALSE)
summary(mod1)
summary(duraFullMod) # seems total biomass (+), mratio (-) does have sig.
                     # influence on duration of heating based on the T value

tmassNull <- lmer(dur ~ mratio_s +
                    (1 + total.mass_s + mratio_s|sp.cd),
                  data=resca_dura, REML=FALSE)
mratioNull <- lmer(dur ~ total.mass_s + 
                      (1 + total.mass_s + mratio_s|sp.cd),
                    data=resca_dura, REML=FALSE)


# grab aic for each model with AICcmodavg::aictab

aictab(list(duraFullMod, tmassNull, mratioNull))
# full mode with both total mass and mass ratio as
# predictors is the best model. Do anova to see if 
# both have sig. effect on duration of heating

anova(duraFullMod, tmassNull) #total biomass has sig. effect
anova(duraFullMod, mratioNull) # mass ratio has sig. (p<0.05) effect


##3. total biomass and biomass ratio influence on duration of heating <10cm

ggplot(temp.below, aes(total.mass, dur, color=sp.cd)) + geom_point(size=0.5) +
  bestfit + 
  labs( y="Duration of heating below 10cm (s)", x="Total above ground biomass (g)",
        color="\n") + scale_color_manual(label=species, values=color) +
  scale_x_continuous(limits=c(0.001, 360), expand=c(0,0)) + 
  scale_y_continuous(limits=c(0, 1200),expand=c(0,0)) + pubtheme.nogridlines 

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
  labs( y="Biomass corrected duration <10cm (s)", 
        x="Biomass ratio", color="\n") + pubtheme.nogridlines

ratio_durbLMM <- lmer(adj_durb ~ mratio + (mratio|sp.cd), 
                      data=temp.below, REML=FALSE)
summary(ratio_durbLMM)
durbNull <- lmer(adj_durb ~ 1 + (mratio|sp.cd), data=temp.below, REML=F)
anova(ratio_durbLMM, durbNull) # sig. mass ratio influence on duration
                               # of heating below 10cm, no density10 influence

# approach 2
resca_durb <- temp.below %>% mutate_at(c("total.mass", "tdensity", "mratio"),
                                       funs(s = zscore(.)))
durbFullMod <- lmer (dur ~ total.mass_s + tdensity_s +
                       (1 + total.mass_s|sp.cd),
                     data=resca_durb, REML=FALSE)  
# I tried use biomass, mass ratio and density as predictors and found density 
# actually has more sig. influence on duration of heating below 10cm, so I switched
# to density here

summary(durbFullMod) # seems total biomass (+), density (+) does have sig.
                     # influence on duration of heating based on the T value

durb_tmassNull <- lmer(dur ~ tdensity_s +
                    (1 + total.mass_s|sp.cd),
                  data=resca_durb, REML=FALSE)
durb_densityNull <- lmer(dur ~ total.mass_s + 
                           (1 + total.mass_s|sp.cd),
                         data=resca_durb, REML=FALSE)

# grab aic for each model with AICcmodavg::aictab

aictab(list(durbFullMod, durb_tmassNull, durb_densityNull))
# full mode with both total mass and density as
# predictors is the best model. Do anova to see if 
# both have sig. effect on duration of heating

anova(durbFullMod, durb_tmassNull) #total biomass has sig. effect(+)
anova(durbFullMod, durb_densityNull) # density has sig. effect(+)

# so, it looks like that plants hold more biomass off ground tend to 
# produce less heat above 10cm, this may be explained by more biomass
# tend to fall for those plants; meanwhile, plants with more dense mass
# tend to produce more heat below 10cm? is this related to heat release 
# rate? plant burn fast (more dense plant tend to burn slow) will produce 
# less heat at  near ground location?

##4. total biomass and biomass density influence on maximum loss rate

#ggplot(data=flam.loss, aes(tdensity, lossrate, color=sp.cd)) + geom_point()+
  #geom_smooth(method="lm", se=F) +
  #labs(y= expression("Biomass loss rate" ~ (s^{-1})), 
       #x=expression("Density" ~ (g/cm^{-3})), 
       #color="\n") + scale_color_manual(labels=species, values=color)+
  #scale_y_continuous(expand = c(0,0), limits=c(0.01, 0.15)) +
  #scale_x_continuous(expand = c(0,0))+ pubtheme.nogridlines # neither density nor total
                                                            #alone can explain loss rate

  ggplot(data=flam.loss, aes(total.mass, lossrate, color=sp.cd)) + geom_point(size=0.5)+
    bestfit+
  labs(y= expression("Biomass loss rate" ~ (s^{-1})), 
       x="Total above ground biomass (g)",color="\n") + 
  scale_color_manual(labels=species, values=color)+
  scale_y_continuous(expand = c(0,0), limits=c(0.01, 0.15)) +
  scale_x_continuous(expand = c(0,0), limits=c(0.001, 220))+ 
  pubtheme.nogridlines

#mlossrNLMod <- nls(lossrate ~ a*total.mass/ (total.mass+b), 
                   #data=flam.loss, start=list(a=0.1, b=0)) 
#summary(mlossrNLMod)
#mlossr.predict <- predict(mlossrNLMod, newdata=flam.loss)
#p + geom_line(aes(total.mass, mlossr.predict), size=1.5, color="black")

# approach 1

tmlossrLM <- lm(lossrate ~ total.mass, data=flam.loss)
adj_lossr <- as.data.frame(residuals(tmlossrLM))
colnames(adj_lossr) <- "adj_lossr"
flam.loss <- cbind(flam.loss, adj_lossr)

#group mean
lossrgm <- flam.loss %>% group_by(sp.cd) %>% summarise(tdensity=mean(tdensity),
                                                          adj_lossr=mean(adj_lossr))

ggplot(data=flam.loss, aes(tdensity, adj_lossr, color=sp.cd)) + geom_point(size=0.5, 
                                                                            alpha=0.4)+
  bestfit + 
  labs(y= expression("Biomass corrected loss rate" ~ (s^{-1})), 
       x=expression("Biomass density" ~ (g/cm^{-3})), color="\n") + 
  scale_y_continuous(limits=c(-0.05, 0.10))+
  scale_color_manual(labels=species, values=color) +
  geom_point(data=lossrgm, size=2.0, alpha=1) + pubtheme.nogridlines

den_lossrLMM <- lmer(adj_lossr ~ tdensity * mratio + (mratio+tdensity|sp.cd), 
                      data=flam.loss, REML=FALSE)
summary(den_lossrLMM)
lossrNull <- lmer(adj_lossr ~ 1 + (tdensity|sp.cd), data=flam.loss, REML=FALSE)
anova(den_lossrLMM, lossrNull)

# apporach 2

resca_lossr <- flam.loss %>% mutate_each(funs(s = zscore(.)), total.mass, 
                                          tdensity, mratio)
lossrdenFull <- lmer(lossrate ~ tdensity_s  +
                      (1 + tdensity_s|sp.cd), 
                     data=resca_lossr, REML=FALSE) 

summary(lossrdenFull) # seems density has sig. influence (-) on loss rate

lossrdenNull <- lmer(lossrate ~ 1 +
                            (1 + tdensity_s|sp.cd),
                            data=resca_lossr, REML=FALSE)
anova(lossrdenFull, lossrdenNull) # density has sig. negative influence on loss rate


lossrmassFull <- lmer(lossrate ~ total.mass_s  +
                        (1 + total.mass_s|sp.cd), 
                      data=resca_lossr, REML=FALSE) 
summary(lossrmassFull)
lossrmassNull <- lmer(lossrate ~ 1  +
                        (1 + total.mass_s|sp.cd), 
                      data=resca_lossr, REML=FALSE) 
anova(lossrmassFull, lossrmassNull) # total biomass also has sig. negative 
                                    # effect on loss rate. I looked how
                                    # total biomss is correlated with density
                                    # i will say at least for 4 species, there 
                                    # is positive relation between total biomass
                                    # and biomass density

##5. total biomass and biomass density influence on max flame height

p <- ggplot(data=arc.trial, aes(total.mass, max.fh, color=sp.cd)) + geom_point(size=0.5)+
  labs(y= "Maximum flame height (cm)", 
       x= "Total above ground biomass (g)", color="\n") + 
  scale_color_manual(labels=species, values=color)+
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0))+ pubtheme.nogridlines 

maxfhNLMod <- nls(max.fh ~ a*total.mass/ (total.mass+b), data=arc.trial, 
                  start=list(a=100, b=5)) 
summary(maxfhNLMod)
maxfh.predict <- predict(maxfhNLMod, newdata=arc.trial)
p + geom_line(aes(total.mass, maxfh.predict), size=1.5, color="black") 

# rescale variables

resca_maxfh <- arc.trial %>% mutate_each(funs(s = zscore(.)), total.mass, 
                                         tdensity, mratio)
 maxfhmassFull <- lmer( max.fh ~ total.mass_s +
                         (1 + total.mass_s |sp.cd),
                       data=resca_maxfh, REML=FALSE)
summary(maxfhmassFull) # seems total mass has sig. effect(+)

maxfhmassNull <- lmer(max.fh ~ 1 + (total.mass_s|sp.cd),
                       data=resca_maxfh, REML=FALSE)
anova(maxfhmassFull, maxfhmassNull) #total mass has significant positive 
                                    #influence on max. flame height

maxfhdenFull <- lmer(max.fh ~ tdensity_s + (1 + tdensity_s|sp.cd),
                    data=resca_maxfh, REML=FALSE)
maxfhdenNull <- lmer(max.fh ~ 1 + (1 + tdensity_s|sp.cd),
                     data=resca_maxfh, REML=FALSE)
anova(maxfhdenFull, maxfhdenNull) # density has no sig.
                                  # influence on max. flame height

##6. total biomass and biomass ratio influence on mass loss

#cairo_pdf("example.pdf", family="Arial", width=6, height=6)

ggplot(data=arc.trial, aes(total.mass, mconsum, color=sp.cd)) + 
  geom_point(size=0.5)+
  bestfit +
  labs(y= "Biomass consumption (g)", x="Total above ground biomass (g)", 
       color="\n") + scale_color_manual(labels=species, values=color)+
  scale_y_continuous(expand = c(0,0), limits=c(0,200)) +
  scale_x_continuous(expand = c(0,0), limits=c(0,200))+ pubtheme.nogridlines 



tmlossLM <- lm(mconsum ~ total.mass, data=arc.trial)
summary(tmlossLM)
arc.trial$adj_mloss <- residuals(tmlossLM)

#group mean
mlossgm <- arc.trial %>% group_by(sp.cd) %>%
  summarise(mratio=mean(mratio), adj_mloss=mean(adj_mloss))

  
ggplot(data=arc.trial, aes(mratio, adj_mloss, color=sp.cd)) + 
  geom_point(size=0.5, alpha=0.4)+ bestfit + 
  labs(y= "Biomass corrected mass consumption(g)",
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

# approach 2

resca_mloss <- arc.trial %>% mutate_each(funs(s = zscore(.)), total.mass, 
                                         tdensity, mratio)

mlossFull <- lmer(mconsum ~ total.mass_s * tdensity_s*mratio_s+
                    (1 + total.mass_s + tdensity_s + mratio_s|sp.cd),
                  data=resca_mloss, REML=FALSE)
summary(mlossFull) # seems mratio (-) and total mass(+) has sig. effects

