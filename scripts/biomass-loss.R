## biomass-loss-rate.R
## -------------------
## plot biomass variation based on time in seconds 
## and do linear model for calculating biomass loss rate


library(dplyr)
library(ggplot2)
library(stringr)

# read and clean a single balance file produced by serial-balance.py. See
# https://github.com/schwilklab/serial-balance
read_balance_file <- function(filename) {
  bdf <- read.csv(filename, sep="\t", stringsAsFactors=FALSE)
  names(bdf) <- c("datet", "label", "nsec", "mass")
  bdf <- bdf[-1,] # strip out first line which has the label prompt text in the
  # first col

  bname <- basename(filename)
  
  bdf$label <- str_sub(bname, 11 , -5)
  bdf$trial <- str_sub(bname, 9, 10)
    
  return(bdf)
}


# read mutiple .csv files at once and rbind them as a file
concat_csv_dir <- function(path) { 
  files <- dir(path, pattern = '.csv', full.names = TRUE)
  tables <- lapply(files, read_balance_file)
  return(bind_rows(tables))
}

balance_data <- concat_csv_dir('../data/balance')
balance_data <- balance_data %>% group_by(label) %>%
  mutate(mloss= first(mass) - mass, mass2= mass-min(mass), utrial=paste(label, trial, sep="-"))


# graph biomass loss based on time in seconds and do linear model for each
# trial species

ID <- unique(balance_data$utrial)
for (i in 1: length(unique(ID))) {  
  onelabel <- filter(balance_data, utrial==ID[i]) # subset
  mod.mass <- lm(mass ~ nsec, data = onelabel) #linear model log(mass)?
  lm.summary <- summary(mod.mass) # lm summary info
   # write out summary:
  capture.output(lm.summary, file = file.path("../results/", paste(ID[i],'.txt')))
  pdf(file.path("../results", file=paste(ID[i], ".pdf", sep=""))) # plot in pdf
  print(qplot(nsec, mass, data=onelabel, geom="line"))
  dev.off()
}

ggplot(filter(balance_data, utrial!="sn25-08"), aes(nsec, mass2)) + geom_line() + facet_wrap(~ utrial)

