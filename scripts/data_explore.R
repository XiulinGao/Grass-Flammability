#data_explore.R
###this script is used for exploring the pattern in collected 
###datasets, which will be used as a guide for analysis

source("./final_summary_dataset.R")
#if peak temp. is correlated to mass loss rate

ggplot(temp_balance, aes(peak.temp, estimate, color=location)) + geom_smooth(method='lm', se=F) +
  geom_point() + facet_wrap(~sp.cd, ncol=2) + labs(x = "peak_temp", y= "mass_loss_rate") +
  ggtitle("correlation between peak temp and mass loss rate ")
#there is a negative correlation between peak.temp
#and mass loss rate(flaming), cor:-0.37

#PCA? but mainly I just have biomass loss rate, peak temperature and max. flame height for
#each trial, so it is necessary to do a PCA? and now peak temp and biomass loss rate are 
#correlated 
flamma <- subset(temp_balance, select=c(sp.cd, peak.temp, max.fh, estimate))
flamma.pca <- prcomp(flamma[, 2:4],
                     center = TRUE,
                     scale. = TRUE) 
print(flamma.pca)
plot(flamma.pca, type='l')
summary(flamma.pca)

#1.plot to see how peak temp is related to log total biomass
ggplot(temp_balance, aes(logtmass, peak.temp, color=location)) + 
  geom_smooth(method='lm', se=FALSE)+geom_point() + facet_wrap(~sp.cd, ncol=2) + 
  ggtitle("logtmass~peak.temp") #positive pattern

#2.how mass loss rate is related to log total biomass, using all_burns dataset
ggplot(all_burns, aes(logtmass, estimate, color=sp.cd)) + ylab("mass_loss_rate") +
  geom_smooth(method='lm', se=FALSE)+ geom_point() + ggtitle("logtmass~massloss") 
#nagetive pattern

#<10
#3.how peak temp is related to mass density 
ggplot(temp10, aes(density10, peak.temp, color=location)) + 
  geom_smooth(method='lm', se=FALSE)+geom_point() + facet_wrap(~sp.cd, ncol=2) + 
  ggtitle("density10~peak.temp")
#really varies among species and also locations within species. negative, 
#positive or no influence

#4.how mass loss rate is related to mass deensity
ggplot(all_burns, aes(density10, estimate, color=sp.cd)) +  ylab("mass_loss_rate") +
  geom_smooth(method='lm', se=FALSE) + geom_point() + ggtitle("density10~massloss")
#positive or negative, depends on species

#>10
#5.how peak temp is related to mass density
ggplot(temp, aes(density, peak.temp, color=location)) + geom_smooth(method='lm', se=FALSE)+
  geom_point() + facet_wrap(~sp.cd, ncol=2) + ggtitle("density~peak.temp")
#varies
#6.how mass loss rate is realted to mass density
ggplot(all_burns, aes(density, estimate, color=sp.cd)) +  ylab("mass_loss_rate") +
  geom_smooth(method='lm', se=FALSE) + geom_point() + ggtitle("density~massloss")
#varies among species

#ok, let's just lookt at how peak temp and mass loss rate look like for 
#all the species

#mass loss rate for 8 species
ggplot(all_burns, aes(sp.cd, estimate)) + geom_violin() + labs(y="mass loss rate", 
                       x="species code") + ggtitle("mass loss rate for species")

#peak temp at one location across species
ggplot(temp_balance, aes(sp.cd, peak.temp, color=sp.cd)) + geom_violin() + 
  facet_wrap(~location) + labs(x="spcies code", y="peak temperatue") + 
  ggtitle("peak temperature at one location across species")
