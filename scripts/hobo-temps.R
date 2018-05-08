# Read hobo data logger temperature data, split into sections by burn trial, and
# summarize for each trial
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

source("./clean_up_trial.R") #grab basic trial summary data

read_hobo_file <- function(filename) {
    hobo <- read.csv(filename, skip=2, header=FALSE)
    names(hobo)[1:3] <- c("row", "time", "temp")
    hobo <- hobo %>% select(time, temp) %>%
        # we use floor_date() below to round to seconds so we can line up our
        # measurements across HOBOs
        mutate(time = floor_date(mdy_hms(time, tz=TZ), "second"))
    return(hobo) #change timezone
}


concat_hobo_files <- function(filelist, label){
    l <- lapply(filelist, read_hobo_file)
    r <- bind_rows(l)
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

## DWS: ok, but better practice when storing data in file names is to extract
## column of id variables from file names vector.

# Now merge all of these together to get one continuous time series (wide
# data). Do we even need this? Really only necessary if we ever compare temps
# across thermocouples.
thermocouples.wide <- basea %>% full_join(baseb) %>% 
                      full_join(height10) %>% 
                      full_join(height20) %>% full_join(height40)

#take average temp for base, then get ave for below and above 10cm
thermocouples.wide$base <- rowMeans(subset(thermocouples.wide, select= c(base.A, base.B)),
                                    na.rm = TRUE) 
#thermocouples.wide$below.sec <- rowMeans(subset(thermocouples.wide, select=c(base, 
                                    #height.10)), na.rm = TRUE) 
# since 10cm, 20cm and 40cm are correlated measurements, take avergae as canopy
thermocouples.wide$above.sec <- rowMeans(subset(thermocouples.wide, select=c(height.10,
                                    height.20,height.40)), na.rm=TRUE)

## DWS: names here are confusing why is one "base" and one "above.sec". SHould
## be "base" and "canopy" or "below" and "above", yes?

thermocouples.wide <- thermocouples.wide %>% mutate_at(c("base", 
                                                         "above.sec"),funs(round(.,2)))
thermocouples.wide <- thermocouples.wide[, -c(2:6)]#get rid off base.A, base.B

## get a trial ID from a time point
get_trial_id <- function(time) {
    matches <- time %within% trials$interval
    if(! any(matches)) return(NA)
    return(trials$trial.id[which.max(matches)])
}


# assign trial ids
thermocouples.wide$trial.id <- unlist(sapply(thermocouples.wide$time, get_trial_id))
#bi21 is dropped off at this step, and I don't know why
## DWS: I can't figuire out the aboove statement. Itrial ids have the from
## date_num, so what is "bi21" A plant id? If something is dropped we must
## solve that.


# throw away data outside of a trial
thermocouples.wide <- thermocouples.wide %>% filter(! is.na(trial.id))

# Long form data probably more useful for per thermcouple summaries.
thermocouples.long <- thermocouples.wide %>% gather(location, temperature, -time, -trial.id)

## then do the summary
threshold=100 # temperature threshold in degrees C
temps.sum <- thermocouples.long %>% group_by(trial.id, location) %>%
    summarise(dur = sum(temperature > threshold),
              degsec = sum(temperature[temperature > threshold]),
              peak.temp = max(temperature, na.rm=TRUE),
              peak.time = time[which(peak.temp == temperature)[1]],
              num.NA = sum(is.na(temperature))) %>%
    full_join(trials)

#clean up env
rm("concat_hobo_files", "get_trial_id", "read_hobo_file", "threshold",
   "thermocouples.wide", "thermocouples.long", "height10", "height20",
   "height40", "basea", "baseb")

# exports temps.sum to workspace

  
