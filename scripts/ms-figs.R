### ms-figs.R
### this R script is used to produce publication figures 
### showing significant fixed effect of plant traits
### on flammability found in architecture-flammability.R and also model
### anova and coefficient tables


library(dplyr)
library(tidyr)
library(pcaMethods)
library(ggplot2)
library(xtable)
library(afex)

source("./architecture-flammability.R")

ptsize=1.5
lnsize=0.8

############ Principal component analysis of flammability data ############
## biplot of PCA with ggplot2
## due to some missing data, use pcaMethods package

#pca on selected flammability measurements
#temperature measurements included are soil surface measurements
#results kept same if PCA is done with temp measurements
#at soil surface

flamabove.PCA <- alldata %>%
  select ( dur.above, lossrate, massloss, degsec.above) %>% 
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
 # get the ratio for PC1-PC3 biplot if needed
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
                             v12 = 0.8 * mult[1] * PC1,
                             v2 = 0.8 * mult[1] * PC2,
                             v13 = 0.8 * mult[2] * PC1,
                             v3 = 0.8 * mult[2] * PC3
)
## get_ratio was used to produce biplot for PC1-PC2 and 
## PC1-PC3, now only need for PC1-PC2

## plot PC2 against PC1 using transformed loading coordinates

## adjust text coordinate for each variable name to avoid overlap of
## text
  text.cor <- flamabove.loads %>% select(v12, v2)
  
    text.cor$v12[1] <- text.cor$v12[1] + 0.15
    text.cor$v12[2] <- text.cor$v12[2] - 0.15 
    text.cor$v12[3] <- text.cor$v12[3] + 0.15
    text.cor$v12[4] <- text.cor$v12[4] + 0.2
  

  ggplot()+
  geom_blank(data = flamabove.scores, aes(x=PC1, y=PC2)) +
    #xlim(c(-3.0, 2)) + ylim(c(0, 4.5)) +
  ylab("Principal component 2") +  xlab("Principal component 1") +
  geom_point(data = flamabove.loads, aes(x=v12, y=v2), shape=15,
             size=ptsize, color = "black")+
  geom_text(data = text.cor, aes(x=v12, y=v2, label=varnames),
            size = 2, vjust=0.5, hjust="inward", color="black") +
  pubtheme.nogridlines + theme(legend.position = "none")
  
ggsave("../results/Fig2_flam_biplot.pdf", width = col1, height = 0.9*col1, units="cm")
                              
###### effect of biomass on temperature integration #######

## long data format is more useful for making figures
## use tempsec.sum, flamlossr, trials and mass.density
#to make long dataset and remove columns which are not needed 

plotdata <- full_join(trials, tempsec.sum, by="trial.id") %>% 
  left_join(flamlossr, by=c("label", "sp.cd", "sp.name")) %>%
  left_join(mass.density, by=c("label", "pair", "treatment", "sp.cd", "sp.name")) %>%
  select (label, pair, sp.name, shade.tolerance, trial.date, trial.num,
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


plotdata %>% filter(!is.na(location)) %>%
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

## fit the best model(from architecture-flammability.R)
## using lmer, we need to use this to predict degsec

crtdegsecb.predic.mod <- lmer(crt.degsecb ~ tdensity_s*mratio_s + 
                                humidity_s + (1 | sp.name), 
                              data = resca_degsecb, REML=FALSE)
## predict degsec with scaled variables
crtdegsecb.predic <- resca_degsecb %>% select(c(tdensity_s, mratio_s, 
                  humidity_s, sp.name, mratio))

crtdegsecb.predic$crt.degsecb <- predict(crtdegsecb.predic.mod, 
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

tabledata <- plotdata %>%
  select ( sp.name, shade.tolerance, total.mass,tdensity, 
           mratio, location, degsec,lossrate) %>% 
  spread(location, degsec)
# dorp the "NA" column
tabledata <- tabledata[, -9]

## first get shade tolerance tab
shad.char <- tabledata %>% group_by (sp.name) %>% 
  summarize( shade.tolerance = shade.tolerance[1])
## get specie mean of each measurements
summary.tab <- tabledata %>% group_by (sp.name) %>% 
  summarize_at ( c("total.mass", "mratio", "tdensity", "above.sec", "base", 
                   "lossrate"),
    funs(mean = mean(., na.rm = TRUE), sd = sd(., na.rm = TRUE)) 
) 

summary.tab <- left_join (shad.char, summary.tab, by="sp.name")  
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
                                      "Surface temperature integration (°C.s)" = base_mean,
                                      #"Predict temperature integration (25cm)" = prdc.degsec25,
                                      "Mass loss rate (s^-1)" = lossrate_mean)
print(xtable(summary.tab, digits = c(0,0, 0, 2, 2, 4, 0, 0, 2, 2, 2, 4, 1, 1, 2)), 
      type="html",file="../results/tab1_species_summary.html")

## model tables
### biomass effect on flammability ###
#all models from architecture-flammability.R

#1.model table of biomass effect on 25cm degsec
print(xtable(anova(degseca.lmod)), type = "html", 
      file = "../results/anova-tab-mass-tempa-mod.html")
print(xtable(summary(degseca.lmod)$coefficients), type = "html", 
      file = "../results/coef-tab-mass-tempa-mod.html")

#2.moddel table of biomass effect on surface degsec
print(xtable(anova(degsecb.lmod)), type = "html", 
      file = "../results/anova-tab-mass-tempb-mod.html")
print(xtable(summary(degsecb.lmod)$coefficients), type = "html", 
      file = "../results/coef-tab-mass-tempb-mod.html")

#3.model table of biomass effect on mass loss rate
print(xtable(anova(lossr.lmod)), type = "html", 
      file = "../results/anova-tab-mass-lossr-mod.html")
print(xtable(summary(lossr.lmod)$coefficients), type = "html", 
      file = "../results/coef-tab-mass-lossr-mod.html")

#4. final model of traits effects on 25 and surface heating
#25 cm 
print(xtable(anova(crtdegseca.mod.final)), type="html",
      file="../results/anova-tab-degseca-final-mod.html")

print(xtable(summary(crtdegseca.mod.final)$coefficients), type="html",
      file="../results/coef-tab-degseca-final-mod.html")

#soil surface
print(xtable(anova(crtdegsecb.mod.final)), type="html",
      file="../results/anova-tab-degsecb-final-mod.html")

print(xtable(summary(crtdegsecb.mod.final)$coefficients), type="html",
      file="../results/coef-tab-degsecb-final-mod.html")

#5.final model of traits effect on maximum biomass loss rate
print(xtable(anova(crtlossr.mod.final), digits = c(0,0,3,3,3)), type="html",
      file="../results/anova-tab-lossr-final-mod.html")

print(xtable(summary(crtlossr.mod.final)$coefficients, digits = c(0,3,3,3)), 
      type="html",file="../results/coef-tab-lossr-final-mod.html")
