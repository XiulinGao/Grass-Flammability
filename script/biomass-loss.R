## biomass-loss-rate.R
## -------------------
## plot biomass variation based on time in seconds 
## and do linear model for calculating biomass loss rate

setwd("../result")
library(plyr)
library(stringr)
library(ggplot2)

# read mutiple .csv files at once and rbind them as a file
load_data <- function(path) { 
  files <- dir(path, pattern = '1023', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}
path <- '../burn-data'
data <- load_data(path)

# clean up data by attaching species id to each row and convert mass to 
# integer
shapedata <- ddply(data, 'time', mutate, sp = str_sub(toString(mass), 5, 8))
sbalance <- ddply(shapedata, 'time', mutate, 
                  mloss = as.integer(str_sub(toString(mass), -18, -1)))
sbalance <- sbalance[, -2]

# graph biomass loss based on time in seconds and do linear model for each
# trial species

ID <- unique(sbalance$sp)
for (i in 1: length(unique(ID))){
  
  mass <- sbalance[sbalance$sp==ID[i], ] # split data into subset in 
                                         # one species
  mass$seconds <- 1:length(unique(mass$time)) # attach seconds column
  mod.mloss <- lm(mloss ~ seconds, data = mass) #linear model log(mass)?
  lm.summary <- summary(mod.mloss) # lm summary info
  capture.output(lm.summary, file = paste(ID[i],'.txt')) # write out summary
  pdf(file=paste(ID[i], ".pdf", sep="")) # plot in pdf
  print(qplot(seconds, mloss, data=mass, geom="line"))
  dev.off()
  
}



