#data_explore.R
###this script is used for exploring the pattern in collected 
###datasets, which will be used as a guide for analysis

source("./final_summary_dataset.R")


##1. If there is species variation in mass loss rate, duration,
##total degree of temp which is above 60 and flame height?
#1)speceis variation in mass loss rate
ggplot(flam.loss, aes(sp.cd, lossrate)) + geom_boxplot()
#2)species variation in duration
#below section(<10cm)
ggplot(temp.belowsec, aes(sp.cd, dur)) + geom_boxplot()
#above section(>10cm)
ggplot(temp.abovesec, aes(sp.cd, dur)) + geom_boxplot()
#3)species variation in total degree of temp which is above 60
#below section
ggplot(temp.belowsec, aes(sp.cd, degsec)) + geom_boxplot()
#above section
ggplot(temp.abovesec, aes(sp.cd, degsec)) + geom_boxplot()
#4)species variation in peak temp
#below section
ggplot(temp.belowsec, aes(sp.cd, peak.temp)) + geom_boxplot()
#above section
ggplot(temp.abovesec, aes(sp.cd, peak.temp)) + geom_boxplot()
#5)species variation in max flame height
ggplot(trials, aes(sp.cd, max.fh)) + geom_boxplot()


#2.how interested flammability measurements correlated to one
#another?
#1)mass loss rate ~ max flame height :positive correlation
ggplot(flam.loss, aes(max.fh, lossrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd) + 
  scale_y_continuous(trans = "reverse")

#2)mass loss rate ~ duration: I will say, no certain pattern
ggplot(flatemp.above, aes(dur, lossrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd) +
  scale_y_continuous(trans = "reverse")

ggplot(smotemp.below, aes(dur, lossrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd) +
  scale_y_continuous(trans = "reverse")

#3)mass loss rate ~ degsec: no pattern
ggplot(flatemp.above, aes(degsec, lossrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd) +
  scale_y_continuous(trans = "reverse")

ggplot(smotemp.below, aes(degsec, lossrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd) +
  scale_y_continuous(trans = "reverse")

#4)mass loss rate ~ peak temp
ggplot(flatemp.above, aes(peak.temp, lossrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd) +
  scale_y_continuous(trans = "reverse")

ggplot(smotemp.below, aes(peak.temp, lossrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd) +
  scale_y_continuous(trans = "reverse")

#5)max flame height ~ peak temp, for which I just will look at
#peak temp for above and below 10cm section respectively
#positive?

ggplot(temp.abovesec, aes(max.fh, peak.temp)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd) 

ggplot(temp.belowsec, aes(max.fh, peak.temp)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd) 

  
#3.If total biomass influences mass loss rate, peak temp.,
#duration, total degree of temp above 60 and max flame height
#1)total biomass ~ mass loss rate
ggplot(flam.loss, aes(logtmass, lossrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd) +
  scale_y_continuous(trans="reverse")

#2)total biomass ~ peak temp
ggplot(temp.abovesec, aes(logtmass, peak.temp)) + geom_point() + 
         geom_smooth(method="lm", se=FALSE) + facet_wrap(~ sp.cd)

ggplot(temp.belowsec, aes(logtmass, peak.temp)) + geom_point() + 
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~ sp.cd)

#3)total biomass ~ duration
ggplot(temp.abovesec, aes(logtmass, dur)) + geom_point() +  
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd)

ggplot(temp.belowsec, aes(logtmass, dur)) + geom_point() +  
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd)

#4)total biomss ~ total degree of temp >60
ggplot(temp.abovesec, aes(logtmass, degsec)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd)

ggplot(temp.belowsec, aes(logtmass, degsec)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd)

#5)total biomass ~ max flame height
ggplot(trials, aes(logtmass, max.fh)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd)
#ok, almost total biomass has positive influence on all those
#measurements.

#4.Does mass density have influence on those measurements?

#1) mass density ~ mass loss rate
#smoldering~logden10 & flaming~logden?

ggplot(flam.loss, aes(logden, lossrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd) +
  scale_y_continuous(trans="reverse")

ggplot(flam.loss, aes(logden10, lossrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd) +
  scale_y_continuous(trans="reverse")

ggplot(smog.loss, aes(logden10, lossrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd) +
  scale_y_continuous(trans="reverse")

ggplot(smog.loss, aes(logden, lossrate)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd) +
  scale_y_continuous(trans="reverse")

#2)mass density ~ duration

ggplot(temp.abovesec, aes(logden, dur)) + geom_point() + 
  geom_smooth(method="lm",se = FALSE) + facet_wrap(~sp.cd)

ggplot(temp.abovesec, aes(logden10, dur)) + geom_point() + 
  geom_smooth(method="lm",se = FALSE) + facet_wrap(~sp.cd)

ggplot(temp.belowsec, aes(logden10, dur)) + geom_point() + 
  geom_smooth(method="lm",se = FALSE) + facet_wrap(~sp.cd)

ggplot(temp.belowsec, aes(logden, dur)) + geom_point() + 
  geom_smooth(method="lm",se = FALSE) + facet_wrap(~sp.cd)

#3)mass density ~ degsec

ggplot(temp.abovesec, aes(logden, degsec)) + geom_point() + 
  geom_smooth(method="lm",se = FALSE) + facet_wrap(~sp.cd)

ggplot(temp.abovesec, aes(logden10, degsec)) + geom_point() + 
  geom_smooth(method="lm",se = FALSE) + facet_wrap(~sp.cd)

ggplot(temp.belowsec, aes(logden10, degsec)) + geom_point() + 
  geom_smooth(method="lm",se = FALSE) + facet_wrap(~sp.cd)

ggplot(temp.belowsec, aes(logden, degsec)) + geom_point() + 
  geom_smooth(method="lm",se = FALSE) + facet_wrap(~sp.cd)

#mass desnity ~ max flame height

ggplot(flam.alldata, aes(logden10, max.fh)) + geom_point()+
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd)

ggplot(flam.alldata, aes(logden, max.fh)) + geom_point()+
  geom_smooth(method="lm", se=FALSE) + facet_wrap(~sp.cd)


