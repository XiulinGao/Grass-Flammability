# Read hobo data logger temperature data, split into sctions by burn trial, and
# summarize for each trial

library(dplyr)
library(lubridate)
library(stringr)

TZ = "UTC" # for now should be fine

trials <- read.csv("../data/burning-trials.csv", stringsAsFactors=FALSE)

# unfortunately, there are no "ending times" in this file so this is a bit
# tricky. This data is fragile because of this oversight: we must assume that
# the order of observations tells us something about end times.
trials <- trials %>% mutate(start.time = mdy_hm(str_c(date, " ", start.t), tz=TZ))
PAD_TIME <- duration(1, units="minutes")
MAX_DATETIME <- mdy_hm("1/1/2020 0:0")
trials$end.time <- c(trials$start.time[2:length(trials$start.time)], MAX_DATETIME) - PAD_TIME



read_hobo_file <- function(filename) {
    hobo <- read.csv(filename, skip=2, header=FALSE)
    names(hobo) <- c("row", "time", "temp", "S", "H" , "St", "eof")
    hobo <- hobo %>% select(time, temp) %>% mutate(time = mdy_hms(time))
    return(hobo)
}



# test
tf <- "../data/hobo/201510230a.csv"
h <- read_hobo_file(tf)
