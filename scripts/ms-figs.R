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

# DWS: No documention on color pallette? Why a
#manual palette? At least document it.
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
flamabove.PCA <- alldata %>%
  select ( dur_canopy, lossrate, massloss, degsec_canopy) %>% 
  pca(nPcs = 4, method="ppca", seed = 100, center=TRUE,scale="uv")
# warning message due to the number of PCs, need to check
biplot(flamabove.PCA)
summary(flamabove.PCA)

#extract pca loading and score for biplot
flamabove.loads <- as.data.frame(loadings(flamabove.PCA))
flamabove.scores <- as.data.frame(scores(flamabove.PCA))
#variable names for loading 
flamabove.loads$varnames <- c("Duration above 100", "Mass loss rate", "Mass loss", 
              "Temperature integration")

flamabove_pca_sum <- alldata
flamabove_pca_sum$PC1 <- flamabove.scores$PC1
flamabove_pca_sum$PC2 <- flamabove.scores$PC2
flamabove_pca_sum <- flamabove_pca_sum %>% group_by(sp.name) %>%
  summarize(PC1 = mean(PC1), PC2 = mean(PC2))

## This is less than ideal but works: a bit hackish. best would be to write our
## own clean function

#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)

# ggbiplot expects a prcmp tyle object (boo) so we make a dummy one and
# replace the imporant parts
dummydata <- alldata %>%
  select ( dur_canopy, lossrate, massloss, degsec_canopy)
dummydata[is.na(dummydata)]<-99 #  
pca.obj <- prcomp(dummydata, center=TRUE, scale.=TRUE)

pca.obj$x<-flamabove.PCA@scores 
pca.obj$rotation<-flamabove.PCA@loadings 
pca.obj$sdev<-flamabove.PCA@sDev
pca.obj$center<-flamabove.PCA@center
pca.obj$scale<-flamabove.PCA@scale

# now use ggbiplot
P2 <- ggbiplot(pca.obj,
              obs.scale = 1, 
              var.scale=1,
              ellipse=TRUE,
              circle=FALSE,
              varname.size=1,
              var.axes=TRUE,
              groups=alldata$sp.name, 
              alpha=0)
  
P2$layers <- c(geom_point(aes(color=alldata$sp.name), cex=5), P2$layers)
P2 +  pubtheme.nogridlines + theme(legend.title = element_blank())
ggsave("../results/Fig2v2_flam_biplot.pdf", width = col2, height = 0.9*col2, units="cm")

## adjust projection scale of loading segments on biplot 

## function to get the minimum ratio of score scale to loading scale on
## two axes, which can be used to propotionally extend loading segments 
## without changing interpretation of biplot

## DWS: I don't see why this is all necessary? How did you come up with this?

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
## DWS: I don't understand this below
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
#    xlim(c(-4, 2)) + ylim(c(-5, 0)) +
  ylab("Principal component 2") +  xlab("Principal component 1") +
  geom_point(data = flamabove.loads, aes(x=v12, y=v2), shape=15,
             size=ptsize, color = "black") +
    geom_text(data = text.cor, aes(x=v12, y=v2, label=flamabove.loads$varnames),
              size = 2, vjust=0.5, hjust="inward", color="black") +
#  geom_point(data=flamabove_pca_sum, aes(x=PC1, y=PC2, color=sp.name)) +
  pubtheme.nogridlines + theme(legend.position = "none")
  
ggsave("../results/Fig2_flam_biplot.pdf", width = col1, height = 0.9*col1, units="cm")



###### effect of biomass on temperature integration #######
  ##   remove columns which are not needed
  
## rename facet labeller
location_names <- list("base" = "soil surface",
                       "canopy" = "25 cm")


## function to assign new labeller value from locations
labeller_replace <- function (variable, value){
  return(location_names[value])  
}


alldata %>% select(sp.name, total.mass, degsec_canopy, degsec_base) %>%
  gather(location, degsec, -sp.name,-total.mass) %>%
  mutate(location=str_sub(location, 8)) %>%
  ggplot(aes(total.mass, degsec)) + geom_point(size=ptsize, shape=16) +
  #scale_color_manual(values=color) +
  facet_grid(. ~ location , labeller = labeller_replace) + 
  geom_smooth(method="lm", se=FALSE, size=lnsize,color="black") + 
  #remove obervations of 0 in plot
  #ylim(c(0.00001,350000)) + xlim(c(0, 212)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Total above ground biomass (g)") + 
   pubtheme.nogridlines

ggsave("../results/Fig3_duration_biomass.pdf", width = col1, height = 0.9*col1, units="cm")

###### effect of biomass height ratio on temp integration at soil surface ######

## residuals of biomass-degsec at soil surface
temp.base <- filter(alldata, !is.na(degsec_base))
tmdegsecbLM <- lm(degsec_base ~ total.mass, data=temp.base)
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
  dplyr::summarize(mratio = mean(mratio), crt.degsecb = mean(crt.degsecb))

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
ggplot(alldata, aes(total.mass, lossrate)) + geom_point(size=ptsize, shape=16) +
  geom_smooth(method="lm", se=FALSE, size=lnsize, color="black") +
  ylab(expression("Maximum mass loss rate" ~ (s^{-1}))) +
  xlab("Total above ground biomass (g)") + pubtheme.nogridlines

ggsave("../results/Fig5_lossrate_biomass.pdf", width = col1, height= 0.9*col1, 
       units="cm")


##### Table of species name, shade tolerance and species mean of measurements #####



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
shad.char <- alldata %>% group_by (sp.name) %>% 
  dplyr::summarize( shade.tolerance = shade.tolerance[1])
## DWS Isn't this species data rather than trial data? Backwards way to do this

## get species mean of each measurements
summary.tab <- alldata %>% group_by (sp.name) %>% 
  summarize_at ( c("total.mass", "mratio", "tdensity", "lossrate", "degsec_base", "degsec_canopy"),
    funs(mean = mean(., na.rm = TRUE), sd = sd(., na.rm = TRUE)) 
) 

summary.tab <- left_join (shad.char, summary.tab)  
##clean significant digites in temp mean 
##and decimal places in other measurements
summary.tab <- summary.tab %>% 
mutate_at(c("degsec_canopy_mean","degsec_base_mean", "degsec_canopy_sd", "degsec_base_sd"), 
          funs(signif(.,5))) %>%
  mutate_at (c("total.mass_mean", "mratio_mean", "total.mass_sd", "mratio_sd",
               "lossrate_mean", "lossrate_sd"),
             funs(round(.,2))) %>%
  mutate_at (c("tdensity_mean", "tdensity_sd"),
             funs(round(.,4)))
## rename column
summary.tab <- summary.tab %>% dplyr::rename("Species" = sp.name, "Shade tolerance" = shade.tolerance,
                                      "Total mass (g)" = total.mass_mean, 
                                      "Biomass height ratio" = mratio_mean,
                                      "Density (g cm^-3)" = tdensity_mean, 
                                      "Canopy temperature integration (°C.s)" = degsec_canopy_mean,
                                      #"Predict temperature integration (0cm)" = prdc.degsec,
                                      "Surface temperature integration (°C.s)" = degsec_base_mean,
                                      #"Predict temperature integration (25cm)" = prdc.degsec25,
                                      "Mass loss rate (s^-1)" = lossrate_mean)
print(xtable(summary.tab, digits = c(0,0, 0, 2, 2, 4, 0, 0, 2, 2, 2, 4, 1, 1, 2)), 
      type="html",file="../results/tab1_species_summary.html")

## model tables
### biomass effect on flammability ###
degseca.lmod <- lm(degsec_canopy ~ total.mass, data=alldata)

print(xtable(anova(degseca.lmod)), type = "html", 
      file = "../results/anova-tab-mass-tempa-mod.html")
print(xtable(summary(degseca.lmod)$coefficients), type = "html", 
      file = "../results/coef-tab-mass-tempa-mod.html")

degsecb.lmod <- lm(degsec_base ~ total.mass, data=alldata)

print(xtable(anova(degsecb.lmod)), type = "html", 
      file = "../results/anova-tab-mass-tempb-mod.html")
print(xtable(summary(degsecb.lmod)$coefficients), type = "html", 
      file = "../results/coef-tab-mass-tempb-mod.html")

lossr.lmod <- lm(lossrate ~ total.mass, data=alldata)

print(xtable(anova(lossr.lmod)), type = "html", 
      file = "../results/anova-tab-mass-lossr-mod.html")
print(xtable(summary(lossr.lmod)$coefficients), type = "html", 
      file = "../results/coef-tab-mass-lossr-mod.html")

### final model of traits effects on flammability ###

##traits effect on temperature integration above soil surface
## take residuals and rescal variable
temp.above <- filter(alldata, !is.na(degsec_canopy))
                     
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
flam.loss <- filter(alldata, !is.na(lossrate))
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
