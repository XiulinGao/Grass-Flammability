# Read hobo data logger temperature data, split into sections by burn trial, and
# summarize for each trial

library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

TZ = "CST6CDT"

trials <- read.csv("../data/2016-burns.csv", stringsAsFactors=FALSE) 
# unfortunately, there are no "ending times" in this file so this is a bit
# tricky. This data is fragile because of this oversight: we must assume that
# the order of observations tells us something about end times.
trials <- trials %>% mutate(start.time = mdy_hm(str_c(trial.date, " ", start.t), tz=TZ)) %>%
  mutate(end.time = mdy_hm(str_c(trial.date, " ", end.t), tz=TZ))
          
#PAD_TIME <- duration(1, units="minutes")
#MAX_DURATION <- duration(2, units="hours")
#trials$end.time <- c(trials$start.time[2:length(trials$start.time)] - PAD_TIME,
                     #trials$start.time[length(trials$start.time)] + MAX_DURATION)
#trials$end.time[trials$end.time - trials$start.time > MAX_DURATION] <-
    #trials$start.time[trials$end.time - trials$start.time > MAX_DURATION] + MAX_DURATION
trials$interval <- interval(trials$start.time, trials$end.time)

read_hobo_file <- function(filename) {
    hobo <- read.csv(filename, skip=2, header=FALSE)
    names(hobo)[1:3] <- c("row", "time", "temp")
    hobo <- hobo %>% select(time, temp) %>%
        # we use floor_date() below to round to seconds so we can line up our
        # measurements across HOBOs
        mutate(time = floor_date(mdy_hms(time, tz=TZ), "second"))
    return(hobo)
}


concat_hobo_files <- function(filelist, label){
    l <- lapply(filelist, read_hobo_file)
    r <- rbind_all(l)
    names(r) <- c("time", label)
    return(r)
}


# get sets for each of four thermocouple locations
basea <- concat_hobo_files(list.files("../data/hobo",
                                      full.names=TRUE, pattern = "*0a.csv"), "base.A")
baseb <- concat_hobo_files(list.files("../data/hobo",
                                      full.names=TRUE, pattern = "*0b.csv"), "base.B")
height10 <- concat_hobo_files(list.files("../data/hobo",
                                         full.names=TRUE, pattern = "*10.csv"), "height.10")
height20 <- concat_hobo_files(list.files("../data/hobo",
                                         full.names=TRUE, pattern = "*20.csv"), "height.20")
height40 <- concat_hobo_files(list.files("../data/hobo",
                                         full.names=TRUE, pattern = "*40.csv"), "height.40")

# Now merge all of these together to get one continuous time series (wide
# data). Do we even need this? Really only necessary if we ever compare temps
# across thermocouples.
thermocouples.wide <- basea %>% full_join(baseb) %>% full_join(height10) %>% 
                      full_join(height20) %>% full_join(height40)
                      

## get a trial ID from a time point
get_trial_id <- function(time) {
    matches <- time %within% trials$interval
    if(! any(matches)) return(NA)
    return(trials$trial[which.max(matches)])
}

# assign trial ids
thermocouples.wide$trial <- unlist(sapply(thermocouples.wide$time, get_trial_id)) #can't work here


























































































































# throw away data outside of a trial
thermocouples.wide <- thermocouples.wide %>% filter(! is.na(trial))

# Long form data porbably more useful for per thermcouple summaries.
thermocouples.long <- thermocouples.wide %>% gather(location, temperature, -time, -trial)

## then do the summary
threshold=60 # temperature threshold in degrees C
temps.sum <- thermocouples.long %>% group_by(trial, location) %>%
    summarise(dur = sum(temperature > threshold),
              degsec = sum(temperature[temperature > threshold]),
              peak.temp = max(temperature),
              peak.time = time[which.max(temperature)],
              num.NA = sum(is.na(temperature))) %>%
    full_join(trials)

