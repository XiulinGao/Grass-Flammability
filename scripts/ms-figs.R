### ms-figs.R
### this R script is used to produce publication figures 
### showing significant fixed effect of plant traits
### on flammability found in lmer-mods.R

#library(grid)
library(dplyr)
library(tidyr)
library(pcaMethods)
library(ggplot2)
library(xtable)
library(qwraps2)

source("./final_summary_dataset.R")
source("./ggplot-theme.R")

#colpalette <- c("#836B43", "#18B0D6", "#E69F00", "#009E73", "#F0E442",
           #"#0072B2", "#D55E00", "#CC79A7")
colpalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ptsize=1.5
lnsize=0.8

# Multiple plot function

# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols: Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.

#multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  #require(grid)
  # Make a list from the ... arguments and plotlist
  #plots <- c(list(...), plotlist)
  #numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  #if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    #layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     #ncol = cols, nrow = ceiling(numPlots/cols))
  #}
  #if (numPlots==1) {
    #print(plots[[1]])
  #} else {
    # Set up the page
    #grid.newpage()
    #pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    #for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      #matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      #print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      #layout.pos.col = matchidx$col))
    #}
  #}
#}

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
                              
## plot PC3 against PC1
## adjust text position of each variable name to avoid overlap of
## labels
 
 #text.cor$v13[1] <- text.cor$v13[1] - 0.1
 #text.cor$v13[2] <- text.cor$v13[2] + 0.1 
 #text.cor$v13[3] <- text.cor$v13[3] - 0.1
 #text.cor$v13[4] <- text.cor$v13[4] - 0.1
 #text.cor$v13[5] <- text.cor$v13[5] - 0.1
 
#p2 <- ggplot()+
  #geom_blank(data = flamabove.scores, aes(x=PC1, y=PC3)) +
  #labs(x="Principal component 1", y="Principal component 3") + 
  #xlim(c(-2.5, 2.5)) + ylim (c(-2, 2.0)) +
  #geom_point(data = flamabove.loads, aes(x=v13, y=v3), shape=15,
             #size=ptsize, color = "black")+
  #geom_text(data = text.cor, aes(x=v13, y=v3, label=varnames),
            #size = 6, vjust=0.5, hjust= "inward", color="black") +
  #pubtheme.nogridlines + theme(legend.position = "none", 
                               #axis.text.x = element_text(size=textsize+6),
                               #axis.text.y = element_text(size=textsize+6),
                               #axis.title.x = element_text(size=textsize+12),
                               #axis.title.y = element_text(size=textsize+12))
## combine p1 and p2 as one figure

#pdf("../results/fig1_biplot.pdf", width=col1, height=0.9 * col1)
#multiplot(p1, p2)
#dev.off()

###### effect of biomass on temperature integration #######

##remove columns which are not needed 
alldata <- temp.alldata %>% select (label, pair, sp.name, trial.date, trial.num,
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
  ylim(c(0.00001,350000)) + xlim(c(0, 200)) +
  ylab(expression("Temperature integration ("*degree*C*" s)")) +
  xlab("Total above ground biomass (g)") + 
   pubtheme.nogridlines

ggsave("../results/Fig3_duration_biomass.pdf", width = col1, height = 0.9*col1, units="cm")

###### effect of biomass ratio on biomass corrected temp integration at 25 cm ######

## residuals of biomass-degsec at 25cm
tmdegsecaLM <- lm(degsec ~ total.mass, data=temp.above)
temp.above$crt.degseca <- residuals(tmdegsecaLM)

## best model fit with z-scored variables
zscore <- function(x) (x - mean(x)) / sd(x) 
resca_degseca <- temp.above %>% mutate_at(c("tdensity", "mratio","temp", "humidity"),
                                         funs(s = zscore(.)))

crtdegseca.mod.final <- lmer(crt.degseca ~ tdensity_s*humidity_s + mratio_s + 
                                 temp_s +  (1 | sp.name),
                               data= resca_degseca, REML=FALSE)

## predict degsec with scaled variables
crtdegseca.predic <- resca_degseca %>% select(c(tdensity_s, temp_s, humidity_s, mratio_s, 
                                                sp.name, mratio))
crtdegseca.predic$crt.degseca <- predict(crtdegseca.mod.final, 
                                         newdata = crtdegseca.predic)
## only keep mratio (unscaled) and fitted degsec 
crtdegseca.predic <- select(crtdegseca.predic, mratio, crt.degseca, sp.name)

## species mean of density and mass-corrected temp integration >10cm
degseca.byspecies <- temp.above %>% group_by(sp.name) %>%
  summarize(mratio = mean(mratio), crt.degseca = mean(crt.degseca))

ggplot(temp.above, aes(mratio, crt.degseca, color=sp.name)) + 
  geom_point(size=ptsize, alpha=0.6, shape=16) + 
  geom_blank(data=crtdegseca.predic) +
  geom_smooth(method="lm", se=FALSE, size=lnsize, color="black") +
  geom_point(data=degseca.byspecies, size=ptsize+1, alpha=1, shape=16,
             aes(color=sp.name))+
  xlim(c(0, 6)) + ylim(c(-25000, 25000)) +
  ylab(expression("Mass corrected temperature integration ("*degree*C*" s)")) +
  xlab("Biomass height ratio") + 
  scale_color_manual(values=colpalette) +
  pubtheme.nogridlines + theme(legend.key.width = unit(0.5, "lines"),
                               legend.position = "bottom",
                               legend.title = element_blank())

ggsave("../results/Fig4_temp_mratio.pdf", width = col1, height= 0.9*col1, 
       units="cm")

###### effect of biomass density on mass-corrected temp integration at soil surface ######

## residuals of biomass-degsec at soil surface
tmdegsecbLM <- lm(degsec ~ total.mass, data=temp.base)
temp.base$crt.degsecb <- residuals(tmdegsecbLM)

## best model fit with z-scored predictors
resca_degsecb <- temp.base %>% mutate_at(c("tdensity", "temp", "humidity", "mratio"),
                                         funs(s = zscore(.)))
crtdegsecb.mod.final <- lmer(crt.degsecb ~ tdensity_s + humidity_s + temp_s +
                               (1|sp.name),data=resca_degsecb, REML=FALSE)

## predict degsec with scaled variables
crtdegsecb.predic <- resca_degsecb %>% select(c(tdensity_s, temp_s, humidity_s, sp.name, 
                                                tdensity))
crtdegsecb.predic$crt.degsecb <- predict(crtdegsecb.mod.final, 
                                         newdata = crtdegsecb.predic)
## only keep tdensity (unscaled) and fitted degsec 
crtdegsecb.predic <- select(crtdegsecb.predic, tdensity, crt.degsecb, sp.name)

## species mean of density and mass-corrected temp integration
degsecb.byspecies <- temp.base %>% group_by(sp.name) %>%
  summarize(tdensity = mean(tdensity), crt.degsecb = mean(crt.degsecb))

ggplot(temp.base, aes(tdensity, crt.degsecb, color=sp.name)) + 
  geom_point(size=ptsize, alpha=0.6, shape=16) + 
  geom_blank(data = crtdegsecb.predic) + 
  geom_smooth(method = "lm", se = FALSE, size=lnsize, color = "black") +
  geom_point(data=degsecb.byspecies, size = ptsize+1, alpha=1, shape=16,
             aes(color=sp.name))+
  ylim(c(-200000, 200000)) +
  ylab(expression("Mass corrected temperature integration ("*degree*C*" s)")) +
  xlab(expression("Biomass density" ~(g~cm^{-3}))) + 
  scale_color_manual(values=colpalette) +
  pubtheme.nogridlines + theme(legend.key.width = unit(0.5, "lines"),
                               legend.position = "bottom",
                               legend.title = element_blank())

ggsave("../results/Fig5_temp_density.pdf", width = col1, height= 0.9*col1, 
       units="cm")

##### total biomass effect on max. mass loss rate ######
ggplot(flam.loss, aes(total.mass, lossrate)) + geom_point(size=ptsize, shape=16) +
  geom_smooth(method="lm", se=FALSE, size=lnsize, color="black") +
  xlim(c(0, 220)) + ylim(c(0, 0.15)) +
  ylab(expression("Maximum mass loss rate" ~ (s^{-1}))) +
  xlab("Total above ground biomass (g)") + pubtheme.nogridlines

ggsave("../results/Fig6_lossrate_biomass.pdf", width = col1, height= 0.9*col1, 
       units="cm")

####### effect of biomass density on mass-corrected biomass loss rate ########

## residuals of mass-loss rate model
lossr.lmod <- lm(lossrate ~ total.mass, data=flam.loss)
flam.loss$crt.lossr <- residuals(lossr.lmod)

## best model fit with z-scored variables
resca_lossr <- flam.loss %>% mutate_at(c("tdensity", "mratio", "humidity", "temp"),
                                       funs(s = zscore(.)))
crtlossr.mod.final <- lmer (crt.lossr ~ tdensity_s + (1|sp.name), 
                         data=resca_lossr, REML=FALSE)

## predict max. loss rate with scaled variable
crtlossr.predic <- resca_lossr %>% select(c(tdensity_s, sp.name, tdensity))
crtlossr.predic$crt.lossr <- predict(crtlossr.mod.final, 
                                         newdata = crtlossr.predic)
## only keep tdensity (unscaled) and fitted loss rate 
crtlossr.predic <- select(crtlossr.predic, tdensity, crt.lossr, sp.name)

## species mean of density and mass-corrected loss rate
lossr.byspecies <- flam.loss %>% group_by(sp.name)%>%
  summarize(tdensity = mean(tdensity),crt.lossr = mean(crt.lossr))

ggplot(flam.loss, aes(tdensity, crt.lossr, color=sp.name)) +
  geom_point(size=ptsize, alpha=0.6, shape=16) + 
  geom_blank(data=crtlossr.predic) + 
  geom_smooth(method="lm", se=FALSE, size=lnsize, color="black") +
  geom_point(data=lossr.byspecies, size=ptsize+1, alpha=1,shape=16,
             aes(color=sp.name))+
  xlim(c(0, 0.006))+ ylim(c(-0.05, 0.05))+
  ylab(expression("Mass corrected mass loss rate" ~(s^{-1}))) +
  xlab(expression("Biomass density" ~(g~cm^{-3}))) + 
  scale_color_manual(values=colpalette) +
  pubtheme.nogridlines + theme(legend.key.width = unit(0.5,"lines"),
                               legend.position = "bottom",
                               legend.title = element_blank())
ggsave("../results/Fig7_lossrate_density.pdf", width = col1, height= 0.9*col1, 
       units="cm")

###### effect of total biomass on maximum flame height  #######

### non-linear model 
#maxfh.nlmod <- nls(max.fh ~ a*total.mass/ (total.mass+b), data=arc.trial, 
                   #start=list(a=100, b=5)) 
#arc.trial <- arc.trial %>% mutate(fh.prdt = predict(maxfh.nlmod, newdata=arc.trial))

### plot predict data with observation
#ggplot(arc.trial, aes(total.mass, max.fh)) + 
  #geom_point(size=ptsize, alpha=0.8) + xlim(c(0, 250)) +
  #ylab("Maximum flame height (cm)") +
  #xlab("Total above ground biomass (g)") + pubtheme.nogridlines + 
  #geom_line(aes(total.mass, fh.prdt), size=lnsize, color="black") 
#ggplot(arc.trial, aes(logtmass, max.fh)) +
  #geom_point(size=ptsize, alpha=0.8) + 
  #geom_smooth(method="lm", se=FALSE, size=lnsize, color="black")+
  #xlab("Log transformed total above ground biomass") +
  #ylab("Maximum flame height (cm)") +
  #pubtheme.nogridlines

#ggsave("../results/fig8_maxfh_biomass.pdf", width = col1, height= 0.9*col1, 
       #units="cm")

##### Table of species name, shade tolerance and species mean of measurements #####

summary.data <- temp.alldata %>% select ( sp.name, shade.tolerance, total.mass, 
                                          tdensity, mratio, location, degsec, 
                                          lossrate) %>% spread(location, degsec)
# dorp the "NA" column
summary.data <- summary.data[, -9]
# join observation with biomass-predicted measurements
mod <- lm(degsec ~ total.mass, data=temp.above)
mod2 <- lm(degsec ~ total.mass, data=temp.base)
mod3 <- lm(lossrate ~ total.mass, data=flam.loss)
temp25 <- temp.above %>% select(sp.name, shade.tolerance, utrial,total.mass) %>%
                                mutate(prdc.degsec25 = predict(mod, newdata=.))
tempb <- temp.base %>% select (sp.name, shade.tolerance, utrial,total.mass) %>%
  mutate(prdc.degsec = predict(mod2, newdata=.))
lossr <- flam.loss %>% select (sp.name, shade.tolerance, utrial,total.mass) %>%
  mutate(prdc.lossr = predict(mod3, newdata=.))

# join observation with predict data
summary.data <- summary.data %>% left_join(temp25) %>% left_join(tempb) %>%
  left_join(lossr)
# summarize species mean of measurements, also can use qwraps2::summary_table
summary.tab <- summary.data %>% group_by (sp.name) %>% 
  summarize( shade.tolerance = shade.tolerance[1],
        # use qwraps2::mean_sd function to get mean and standard deviation
        mean.tm = mean_sd(total.mass, digits = 2, denote_sd = "paren", show_n ="never", na_rm=TRUE),  
        mean.mhr= mean_sd(mratio,digits = 2, denote_sd = "paren", show_n ="never", na_rm=TRUE),
        mean.den = mean_sd(tdensity, digits = 4, denote_sd = "paren", show_n ="never",na_rm=TRUE),
        mean.degsec = mean_sd(base, digits = 2, denote_sd = "paren", show_n ="never", na_rm=TRUE),
        prdc.degsec = mean_sd(prdc.degsec, digits = 2, denote_sd = "paren", show_n ="never", na_rm=TRUE),
        mean.degsec25 = mean_sd(above.sec, digits = 2, denote_sd = "paren", show_n ="never", na_rm=TRUE),
        prdc.degsec25 = mean_sd(prdc.degsec25, digits = 2, denote_sd = "paren", show_n ="never", na_rm=TRUE),
        mean.lossr = mean_sd(lossrate, digits = 3, denote_sd = "paren", show_n ="never", na_rm=TRUE),
        prdc.lossr = mean_sd(prdc.lossr, digits = 3, denote_sd = "paren", show_n ="never", na_rm=TRUE))
summary.tab
# rename column
summary.tab <- summary.tab %>% rename("Species" = sp.name, "Shade tolerance" = shade.tolerance,
                                      "Total mass" = mean.tm, "Biomass height ratio" = mean.mhr,
                                      "Density" = mean.den, 
                                      "Temperature integration (0cm)" = mean.degsec,
                                      "Predict temperature integration (0cm)" = prdc.degsec,
                                      "Temperature integration (25cm)" = mean.degsec25,
                                      "Predict temperature integration (25cm)" = prdc.degsec25,
                                      "Mass loss rate" = mean.lossr,
                                      "Predict mass loss rate" = prdc.lossr)

print(xtable(summary.tab, digits = c(0,0, 0, 2, 2, 4, 2, 2, 2, 2, 3, 3)),
      type="html", file="../results/tab1_species_summary.html")
# looks funny and may be too large to include in manuscript, as appedix?