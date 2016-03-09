# Read hobo data logger temperature data, split into sctions by burn trial, and
# summarize for each trial

library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

TZ = "UTC" # for now should be fine

trials <- read.csv("../data/burning-trials.csv", stringsAsFactors=FALSE)

# unfortunately, there are no "ending times" in this file so this is a bit
# tricky. This data is fragile because of this oversight: we must assume that
# the order of observations tells us something about end times.
trials <- trials %>% mutate(start.time = mdy_hm(str_c(date, " ", start.t), tz=TZ))
PAD_TIME <- duration(1, units="minutes")
MAX_DURATION <- duration(2, units="hours")
trials$end.time <- c(trials$start.time[2:length(trials$start.time)] - PAD_TIME,
                     trials$start.time[length(trials$start.time)] + MAX_DURATION) 



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

temperature_summaries <- function(temps) {
    threshold <- 60 # todo
    above.th <- filter(temps, temperature > threshold)
    dur <- length(above.th$time)
    degsec <- sum(above.th$temperature)
    peak.temp <- max(temps$temperature)
    peak.time <- temps$time[which.max(temps$temperature)]
    return(data.frame(dur, degsec, peak.temp, peak.time))
}

# get sets for each of four thermocouple locations
basea <- concat_hobo_files(list.files("../data/hobo",
                                      full.names=TRUE, pattern = "*0a.csv"), "base.A")
baseb <- concat_hobo_files(list.files("../data/hobo",
                                      full.names=TRUE, pattern = "*0b.csv"), "base.B")
height20 <- concat_hobo_files(list.files("../data/hobo",
                                         full.names=TRUE, pattern = "*20.csv"), "height.20")
height40 <- concat_hobo_files(list.files("../data/hobo",
                                         full.names=TRUE, pattern = "*40.csv"), "height.40")

# Now merge all of these together to get one continuous time series (wide
# data). Do we even need this? Really only necessary if we ever compare temps
# across thermocouples.
thermocouples.wide <- basea %>% full_join(baseb) %>% full_join(height20) %>% full_join(height40)
# Long form data porbably more useful for per thermcouple summaries.
thermocouples.long <- thermocouples.wide %>% gather(location, temperature, -time)

## TODO: add column for trial using start and end times

## then do the summary
# temps.sum <- thermocouples.long %>% group_by(trial, location) %>% temperature_summaries()
