###clean_up_trial.R
###this scrips is excused to clean up recorded trial data
###main mission is to drop failed duplicated trial and also
###do basic summary for each trial: time duration for each burn
###total above ground biomass.

library(dplyr)
library(lubridate)
library(stringr)

TZ = "CST6CDT"

trials <- read.csv("../data/2016-burns.csv", stringsAsFactors=FALSE,
                   na.strings = c("", "N/A")) 
trials <- trials %>% mutate(temp = (temp-32)*5/9) %>% 
  filter(!sp.cd=="ARPU9") %>%
  mutate(total.mass = initial.mass-final.mass) %>%
  mutate(start.time = mdy_hm(str_c(trial.date, " ",
                                   start.t), tz=TZ)) %>%
  mutate(end.time = mdy_hm(str_c(trial.date, " ", end.t), tz=TZ),
         trial.id = paste(trial.date, trial.num, sep="_"))

#drop duplicated trial, always the first one failed
#find the duplicated trial id
n1 <- trials$trial.date[duplicated(trials$label)]
n2 <- trials$trial.num[duplicated(trials$label)] -1 #the one needs to be droped is 
#the first one
dropid <- paste(n1, n2, sep="_")
#drop the rows of duplicated trials
trials <- trials %>% filter(!trial.id %in% dropid)

trials$interval <- interval(trials$start.time, trials$end.time)

#fix ignition time NA issue by assiging average ignition time to NA

get_average_ignition <- function(ignition){
  for (i in 1:length(ignition)){
    if(is.na(ignition[i]))
      ignition[i] <- round(mean(trials$ignition[trials$sp.cd==trials$sp.cd[i]],
                               na.rm=T),2)
  }
  return(ignition)
}

trials$ignition <- get_average_ignition(trials$ignition)
