### 2016-burn-pub-plot.R
### this R script is used to produce publication figures 
### showing significant fixed effect of plant traits
### on flammability found in 2016-burns-analysis.R
### project done in 2016

library(ggplot2)
#library(grid)
library(dplyr)

source("./final_summary_dataset.R")
source("./ggplot-theme.R")

color <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
           "#0072B2", "#D55E00", "#CC79A7")
ptsize=1.5
lnsize=0.8

#### effect of total biomass on biomass combusted ####
  ggplot(arc.trial, aes(total.mass, mconsum)) + geom_point(size=ptsize, alpha=0.8) +
  geom_smooth(method="lm", se=FALSE, color = "black", size=lnsize) +
  ylim(c(0, 200)) + xlim(c(0,200)) +
  ylab("Biomass loss (g)") + 
  xlab("Total above ground biomass (g)") +
  pubtheme.nogridlines

ggsave("../results/massloss_biomass.pdf", width=col1, height=0.9*col1, units="cm")

mloss.lmod <- lm(mconsum ~ total.mass, data= arc.trial)
#arc.trial$dtr.mloss <- residuals(mloss.lmod)

###### biomass effect on duration above 60 degree #######

##remove columns which are not needed 
alldata <- temp.alldata %>% select (label, pair, sp.name, trial.date, trial.num,
                                    max.fh, total.mass, trial.id, utrial, mconsum,
                                    massloss, location, dur, degsec, lossrate,
                                    p.value, tdensity, mratio)
## rename labeller for facet
location_names <- list("above.sec" = "> 10 cm",
                       "below.sec" = "< 10 cm")

## function to assign new labeller value from location_names
labeller_replace <- function (variable, value){
  return(location_names[value])  
}

alldata %>% filter(!is.na(location)) %>%
ggplot(aes(total.mass, dur)) + geom_point(size=ptsize, alpha=0.8) +
  facet_grid(. ~ location, labeller = labeller_replace) + 
  ylim(c(0,2000)) + xlim(c(0, 200)) +
  geom_smooth(method="lm", se=FALSE, size=lnsize,color="black") + 
  ylab(expression("Duration above 60" *~degree*C*" (s)")) +
  xlab("Total above ground biomass (g)") + 
   pubtheme.nogridlines

ggsave("../results/duration_biomass.pdf", width = col1, height = 0.9*col1, units="cm")
## detrend duration <10cm, no additional canopy effect on duration>10cm, omit

## biomass effect on duration < 10cm
tmdurLM <- lm(dur ~ total.mass, data=temp.below)
temp.below$dtr.dur <- residuals(tmdurLM)

###### effect of biomass density on detrend duration <10cm ######

ggplot(temp.below, aes(tdensity, dtr.dur, color=sp.name)) + 
  geom_point(size=ptsize, alpha=0.8, shape=16) +  scale_color_manual(values=color) +
  geom_smooth(method="lm", se=FALSE, size=lnsize, color="black") +
  xlim(c(0, 0.006)) + ylim(c(-600, 600)) +
  ylab("Detrended duration < 10 cm (s)") +
  xlab(expression("Biomass density" ~(g/cm^{3}))) + 
  pubtheme.nogridlines + theme(legend.key.width = unit(0.5, "lines"),
                               legend.position = "bottom",
                               legend.title = element_blank())

ggsave("../results/detrended_duration_density.pdf", width = col1, height= 0.9*col1, 
       units="cm")

###### maximum flame height saturated as total biomass increased #######

### non-linear model 
maxfh.nlmod <- nls(max.fh ~ a*total.mass/ (total.mass+b), data=arc.trial, 
                   start=list(a=100, b=5)) 
arc.trial <- arc.trial %>% mutate(fh.prdt = predict(maxfh.nlmod, newdata=arc.trial))
### plot predict data with observation
ggplot(arc.trial, aes(total.mass, max.fh)) + 
  geom_point(size=ptsize, alpha=0.8) + xlim(c(0, 250)) +
  ylab("Maximum flame height (cm)") +
  xlab("Total above ground biomass (g)") + pubtheme.nogridlines + 
 geom_line(aes(total.mass, fh.prdt), size=lnsize, color="black") 

ggsave("../results/maxfh_biomass.pdf", width = col1, height= 0.9*col1, 
       units="cm")

##### total biomass effect on max. mass loss rate ######
ggplot(flam.loss, aes(total.mass, lossrate)) + geom_point(size=ptsize, alpha=0.8) +
  geom_smooth(method="lm", se=FALSE, size=lnsize, color="black") +
  xlim(c(0, 220)) + ylim(c(0, 0.15)) +
  ylab(expression("Maximum mass loss rate" ~ (s^{-1}))) +
  xlab("Total above ground biomass (g)") + pubtheme.nogridlines

ggsave("../results/lossrate_biomass.pdf", width = col1, height= 0.9*col1, 
       units="cm")

lossr.lmod <- lm(lossrate ~ total.mass, data=flam.loss)
flam.loss$dtr.lossr <- residuals(lossr.lmod)

####### effect of biomass density on detrended max. biomass loss rate ########

ggplot(flam.loss, aes(tdensity, dtr.lossr, color=sp.name)) +
  geom_point(size=ptsize, alpha=0.8, shape=16) +  scale_color_manual(values=color) +
 xlim(c(0, 0.006))+ ylim(c(-0.05, 0.10))+
  geom_smooth(method="lm", se=FALSE, size=lnsize, color="black") +
  ylab(expression("Detrended mass loss rate" ~(s^{-1}))) +
  xlab(expression("Biomass density" ~(g/cm^{3}))) + 
  pubtheme.nogridlines + theme(legend.key.width = unit(0.5,"lines"),
                               legend.position = "bottom",
                               legend.title = element_blank())

ggsave("../results/detrended_lossrate_density.pdf", width = col1, height= 0.9*col1, 
       units="cm")

############ Principle component analysis of flammability data ############
## biplot of PCA with ggplot2
## due to some missing data, use pcaMethods package
library(pcaMethods)

#apply pca to selected flammability measurements
flamabove.PCA <- temp.above %>%
  select ( dur, lossrate, massloss, degsec,
           max.fh) %>% pca(nPcs=4, method="ppca",
                           center=TRUE,scale="uv")
summary(flamabove.PCA)
#extract pca loading and score for biplot
flamabove.loads <- as.data.frame(loadings(flamabove.PCA))
flamabove.scores <- as.data.frame(scores(flamabove.PCA))
#extract grouping label for pca scores and variable names for loading 
flamabove.scores$sp.name <- temp.above$sp.name
varnames <- c("Duration above 60", "Maximum loss rate", "Mass loss", 
              "Temperature integration", "Maximum flame height")

#adjust relative length of loading segments on biplot 
mult <- min(
  (max(flamabove.scores$PC2) - min(flamabove.scores$PC2)/(max(flamabove.loads$PC2)-min(flamabove.loads$PC2))),
  (max(flamabove.scores$PC1) - min(flamabove.scores$PC1)/(max(flamabove.loads$PC1)-min(flamabove.loads$PC1)))
)

flamabove.loads <- transform(flamabove.loads,
                             v1 = 0.9 * mult * PC1,
                             v2 = 0.9 * mult * PC2
)

#adjust position of degsec
text.cor <- flamabove.loads %>% select(v1, v2)
text.cor$v1[1] <- text.cor$v1[1] + 0.1
text.cor$v1[5] <- text.cor$v1[5] + 0.1 
text.cor$v1[3] <- text.cor$v1[3] + 0.1
text.cor$v1[4] <- text.cor$v1[4] + 0.05
text.cor$v1[2] <- text.cor$v1[2] -0.1

#final biplot 
ggplot()+
  geom_blank(data = flamabove.scores, aes(x=PC1, y=PC2)) +
  labs(x="Principle component 1", y="Principle component 2") + 
  geom_point(data = flamabove.loads, aes(x=v1, y=v2), shape=15,
             size=ptsize-0.5, color = "black")+
  xlim(c(-4.0, 2.5)) + ylim (c(-4, 4)) +
  geom_text(data = text.cor, aes(x=v1, y=v2, label=varnames),
            size = 2, vjust=0.5, hjust= "inward", color="black") +
  pubtheme.nogridlines + theme(legend.position = "none")
ggsave("../results/pca-biplot.pdf", width=col1, height=0.9*col1, units="cm")
## same dimension was detected when looking at PC1&PC3, except the meaning
## of two axes was swapped 