### ms-figs.R
### this R script is used to produce publication figures 
### showing significant fixed effect of plant traits
### on flammability found in lmer-mods.R and also model
### anova and coefficient tables

#library(grid)
library(dplyr)
library(tidyr)
library(pcaMethods)
library(ggplot2)
library(xtable)
library(afex)
#library(qwraps2)

source("./final_summary_dataset.R")
source("./ggplot-theme.R")

#colpalette <- c("#836B43", "#18B0D6", "#E69F00", "#009E73", "#F0E442",
           #"#0072B2", "#D55E00", "#CC79A7")
colpalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ptsize=1.5
lnsize=0.8

############ Principal component analysis of flammability data ############
## biplot of PCA with ggplot2
## due to some missing data, use pcaMethods package

#pca on selected flammability measurements
#temperature measurements included are 25 cm measurements
#results kept same if PCA is done with temp measurements
#at soil surface

flamabove.PCA <- pcadata.above %>%
  select ( dur, lossrate, massloss, degsec) %>% 
  pca(nPcs = 4, method="ppca", seed = 100, center=TRUE,scale="uv")
# warning message due to the number of PCs, need to check
biplot(flamabove.PCA)
summary(flamabove.PCA)

#extract pca loading and score for biplot
flamabove.loads <- as.data.frame(loadings(flamabove.PCA))
flamabove.scores <- as.data.frame(scores(flamabove.PCA))

#variable names for loading 
varnames <- c("Duration above 100", "Mass loss rate", "Mass loss", 
              "Temperature integration")

## adjust projection scale of loading segments on biplot 

## function to get the minimum ratio of score scale to loading scale on
## two axes, which can be used to propotionally extend loading segments 
## without changing interpretation of biplot

get_ratio <- function(pcscores, pcloads) {
  # get the ratio for PC1-PC2 biplot
  mult1 <- min(
    (max(pcscores$PC2) - min(pcscores$PC2)/(max(pcloads$PC2)-min(pcloads$PC2))),
    (max(pcscores$PC1) - min(pcscores$PC1)/(max(pcloads$PC1)-min(pcloads$PC1)))
  )
 # get the ratio for PC1-PC3 biplot
  mult2 <- min(
    (max(pcscores$PC3) - min(pcscores$PC3)/(max(pcloads$PC3)-min(pcloads$PC3))),
    (max(pcscores$PC1) - min(pcscores$PC1)/(max(pcloads$PC1)-min(pcloads$PC1)))
  )
return(c(mult1, mult2))
}

mult <- get_ratio(flamabove.scores,flamabove.loads)

## produce new loading coordinates by the score/loading ratio 
## to propotionally extend loading segments on biplot

flamabove.loads <- transform(flamabove.loads,
                             v12 = 0.7 * mult[1] * PC1,
                             v2 = 0.7 * mult[1] * PC2,
                             v13 = 0.7 * mult[2] * PC1,
                             v3 = 0.7 * mult[2] * PC3
)
## get_ratio was used to produce biplot for PC1-PC2 and 
## PC1-PC3, now only need for PC1-PC2

## plot PC2 against PC1 using transformed loading coordinates

## adjust text coordinate for each variable name to avoid overlap of
## text
  text.cor <- flamabove.loads %>% select(v12, v2, v13, v3)
    text.cor$v12[1] <- text.cor$v12[1] + 0.15
    text.cor$v12[2] <- text.cor$v12[2] - 0.15 
    text.cor$v12[3] <- text.cor$v12[3] + 0.15
    text.cor$v12[4] <- text.cor$v12[4] + 0.2
  

  ggplot()+
  geom_blank(data = flamabove.scores, aes(x=PC1, y=PC2)) +
    xlim(c(-3.0, 2)) + ylim(c(0, 4.5)) +
  ylab("Principal component 2") +  xlab("Principal component 1") +
  geom_point(data = flamabove.loads, aes(x=v12, y=v2), shape=15,
             size=ptsize, color = "black")+
  geom_text(data = text.cor, aes(x=v12, y=v2, label=varnames),
            size = 2, vjust=0.5, hjust="inward", color="black") +
  pubtheme.nogridlines + theme(legend.position = "none")
  
ggsave("../results/Fig2_flam_biplot.pdf", width = col1, height = 0.9*col1, units="cm")
                              
###### effect of biomass on temperature integration #######

##remove columns which are not needed 
alldata <- temp.alldata %>% left_join(flam.loss) %>%
  select (label, pair, sp.name, trial.date, trial.num,
                                    max.fh, total.mass, trial.id, utrial, mconsum,
                                    massloss, location, dur, degsec, lossrate,
                                    p.value, tdensity, mratio)
## rename facet labeller
location_names <- list("above.sec" = "25 cm",
                       "base" = "soil surface")

## function to assign new labeller value from locations
labeller_replace <- function (variable, value){
  return(location_names[value])  
}


alldata %>% filter(!is.na(location)) %>%
ggplot(aes(total.mass, degsec)) + geom_point(size=ptsize, shape=16) +
  #scale_color_manual(values=color) +
  facet_grid(. ~ location, labeller = labeller_replace) + 
  geom_smooth(method="lm", se=FALSE, size=lnsize,color="black") + 
  #remove obervations of 0 in plot
  #ylim(c(0.00001,350000)) + xlim(c(0, 212)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Total above ground biomass (g)") + 
   pubtheme.nogridlines

ggsave("../results/Fig3_duration_biomass.pdf", width = col1, height = 0.9*col1, units="cm")

###### effect of biomass height ratio on temp integration at soil surface ######

## residuals of biomass-degsec at soil surface
tmdegsecbLM <- lm(degsec ~ total.mass, data=temp.base)
temp.base$crt.degsecb <- residuals(tmdegsecbLM)
## best model fit with z-scored variables
zscore <- function(x) (x - mean(x)) / sd(x) 

resca_degsecb <- temp.base %>% mutate_at(c("tdensity", "temp", "humidity", "mratio"),
                                         funs(s = zscore(.)))
crtdegsecb.mod.final <- lmer(crt.degsecb ~ tdensity_s*mratio_s + 
                                humidity_s + (1 | sp.name), 
                              data = resca_degsecb, REML=FALSE)
## predict degsec with scaled variables
crtdegsecb.predic <- resca_degsecb %>% select(c(tdensity_s, mratio_s, 
                  humidity_s, sp.name, mratio))

crtdegsecb.predic$crt.degsecb <- predict(crtdegsecb.mod.final, 
                                         newdata = crtdegsecb.predic)
## only keep mratio (unscaled) and fitted degsec 
crtdegsecb.predic <- select(crtdegsecb.predic, mratio, crt.degsecb, sp.name)

## species mean of mratio and mass-corrected temp integration
degsecb.byspecies <- temp.base %>% group_by(sp.name) %>%
  summarize(mratio = mean(mratio), crt.degsecb = mean(crt.degsecb))

ggplot(temp.base, aes(mratio, crt.degsecb, color=sp.name)) + 
  geom_point(size=ptsize, alpha=0.5, shape=16) + 
  geom_blank(data = crtdegsecb.predic) + 
  geom_smooth(method = "lm", se = FALSE, size=lnsize, color = "black") +
  geom_point(data=degsecb.byspecies, size = ptsize+1, alpha=1, shape=16,
             aes(color=sp.name))+
  ylab(expression(Mass ~ corrected ~ temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Biomass height ratio") + 
  scale_color_manual(values=colpalette) +
  pubtheme.nogridlines + theme(legend.key.width = unit(0.5, "lines"),
                               legend.position = "bottom",
                               legend.title = element_blank())

ggsave("../results/Fig4_temp_mratio.pdf", width = col1, height= 0.9*col1, 
       units="cm")

##### total biomass effect on max. mass loss rate ######
ggplot(flam.loss, aes(total.mass, lossrate)) + geom_point(size=ptsize, shape=16) +
  geom_smooth(method="lm", se=FALSE, size=lnsize, color="black") +
  ylab(expression("Maximum mass loss rate" ~ (s^{-1}))) +
  xlab("Total above ground biomass (g)") + pubtheme.nogridlines

ggsave("../results/Fig5_lossrate_biomass.pdf", width = col1, height= 0.9*col1, 
       units="cm")


##### Table of species name, shade tolerance and species mean of measurements #####

summary.data <- temp.alldata %>% left_join(flam.loss) %>%
  select ( sp.name, shade.tolerance, total.mass,tdensity, 
           mratio, location, degsec,lossrate) %>% 
  spread(location, degsec)
# dorp the "NA" column
summary.data <- summary.data[, -9]
# join observation with biomass-predicted measurements
#mod <- lm(degsec ~ total.mass, data=temp.above)
#mod2 <- lm(degsec ~ total.mass, data=temp.base)
#mod3 <- lm(lossrate ~ total.mass, data=flam.loss)
#temp25 <- temp.above %>% select(sp.name, shade.tolerance, utrial,total.mass) %>%
                                #mutate(prdc.degsec25 = predict(mod, newdata=.))
#tempb <- temp.base %>% select (sp.name, shade.tolerance, utrial,total.mass) %>%
  #mutate(prdc.degsec = predict(mod2, newdata=.))
#lossr <- flam.loss %>% select (sp.name, shade.tolerance, utrial,total.mass) %>%
  #mutate(prdc.lossr = predict(mod3, newdata=.))

# join observation with predict data
#summary.data <- summary.data %>% left_join(temp25) %>% left_join(tempb) %>%
  #left_join(lossr)
# summarize species mean of measurements, also can use qwraps2::summary_table

## first get shade tolerance tab
shad.char <- summary.data %>% group_by (sp.name) %>% 
  summarize( shade.tolerance = shade.tolerance[1])
## get specie mean of each measurements
summary.tab <- summary.data %>% group_by (sp.name) %>% 
  summarize_at ( c("total.mass", "mratio", "tdensity", "above.sec", "base", 
                   "lossrate"),
    funs(mean = mean(., na.rm = TRUE), sd = sd(., na.rm = TRUE)) 
) 

summary.tab <- left_join (shad.char, summary.tab)  
##clean significant digites in temp mean 
##and decimal places in other measurements
summary.tab <- summary.tab %>% 
mutate_at(c("above.sec_mean","base_mean", "above.sec_sd", "base_sd"), 
          funs(signif(.,5))) %>%
  mutate_at (c("total.mass_mean", "mratio_mean", "total.mass_sd", "mratio_sd",
               "lossrate_mean", "lossrate_sd"),
             funs(round(.,2))) %>%
  mutate_at (c("tdensity_mean", "tdensity_sd"),
             funs(round(.,4)))
## rename column
summary.tab <- summary.tab %>% rename("Species" = sp.name, "Shade tolerance" = shade.tolerance,
                                      "Total mass (g)" = total.mass_mean, 
                                      "Biomass height ratio" = mratio_mean,
                                      "Density (g cm^-3)" = tdensity_mean, 
                                      "Canopy temperature integration (°C.s)" = above.sec_mean,
                                      #"Predict temperature integration (0cm)" = prdc.degsec,
                                      "Surface temperature integration (°C.s)" = base_mean,
                                      #"Predict temperature integration (25cm)" = prdc.degsec25,
                                      "Mass loss rate (s^-1)" = lossrate_mean)
print(xtable(summary.tab, digits = c(0,0, 0, 2, 2, 4, 0, 0, 2, 2, 2, 4, 1, 1, 2)), 
      type="html",file="../results/tab1_species_summary.html")

## model tables
### biomass effect on flammability ###
degseca.lmod <- lm(degsec ~ total.mass, data=temp.above)

print(xtable(anova(degseca.lmod)), type = "html", 
      file = "../results/anova-tab-mass-tempa-mod.html")
print(xtable(summary(degseca.lmod)$coefficients), type = "html", 
      file = "../results/coef-tab-mass-tempa-mod.html")

degsecb.lmod <- lm(degsec ~ total.mass, data=temp.base)

print(xtable(anova(degsecb.lmod)), type = "html", 
      file = "../results/anova-tab-mass-tempb-mod.html")
print(xtable(summary(degsecb.lmod)$coefficients), type = "html", 
      file = "../results/coef-tab-mass-tempb-mod.html")

lossr.lmod <- lm(lossrate ~ total.mass, data=flam.loss)

print(xtable(anova(lossr.lmod)), type = "html", 
      file = "../results/anova-tab-mass-lossr-mod.html")
print(xtable(summary(lossr.lmod)$coefficients), type = "html", 
      file = "../results/coef-tab-mass-lossr-mod.html")

### final model of traits effects on flammability ###

##traits effect on temperature integration above soil surface
## take residuals and rescal variable
temp.above$crt.degseca <- residuals(degseca.lmod)
resca_degseca <- temp.above %>% mutate_at(c("tdensity", "mratio", "humidity"),
                                          funs(s = zscore(.)))
crtdegseca.mod.final <- mixed(crt.degseca ~ mratio_s*tdensity_s + 
                                 humidity_s + (1 + humidity_s | sp.name), 
                               data= resca_degseca, REML=FALSE)            

print(xtable(anova(crtdegseca.mod.final)), type="html",
      file="../results/anova-tab-degseca-final-mod.html")

print(xtable(summary(crtdegseca.mod.final)$coefficients), type="html",
      file="../results/coef-tab-degseca-final-mod.html")

##traits effect on temperature integration at soil surface 

crtdegsecb.mod.final <- mixed(crt.degsecb ~ tdensity_s*mratio_s + 
                               humidity_s + (1 | sp.name), 
                             data = resca_degsecb, REML=FALSE)

print(xtable(anova(crtdegsecb.mod.final)), type="html",
      file="../results/anova-tab-degsecb-final-mod.html")

print(xtable(summary(crtdegsecb.mod.final)$coefficients), type="html",
      file="../results/coef-tab-degsecb-final-mod.html")

##traits effect on maximum biomass loss rate
# obtain residuals and rescale predictors first

flam.loss$crt.lossr <- residuals(lossr.lmod)
resca_lossr <- flam.loss %>% mutate_at(c("tdensity", "mratio", "humidity", "temp"),
                                       funs(s = zscore(.)))
# mod and summary table
crtlossr.mod.final <- mixed(crt.lossr ~ mratio_s*tdensity_s +
                              + humidity_s + (1|sp.name),
                            data=resca_lossr, REML=FALSE)
print(xtable(anova(crtlossr.mod.final), digits = c(0,0,3,3,3)), type="html",
      file="../results/anova-tab-lossr-final-mod.html")

print(xtable(summary(crtlossr.mod.final)$coefficients, digits = c(0,3,3,3)), 
      type="html",file="../results/coef-tab-lossr-final-mod.html")
