## read_balance_data.R
## -------------------
## Read and clean biomass loss data

library(dplyr)
library(ggplot2)
library(stringr)

#get data per trial

source("./clean_up_trial.R")

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

balance_data <- concat_csv_dir('../data/balance') %>% 
  filter(!label=='ap01') #throw away ARPU9

balance_data <- left_join(balance_data, trials, by = "label")
balance_data <- balance_data %>%
  mutate(mass = mass * 0.001) %>% # mass to g
  group_by(label) %>% 
  mutate(
    is.flaming = nsec > 50+ignition & nsec < 50+ignition+combustion,
    is.smoldering = nsec > 50+ignition+combustion & nsec < 50+ignition+combustion+smoldering,
    septime = 50+ignition+combustion)
         

# graph biomass loss based on time in seconds
ID <- unique(balance_data$utrial)
for (i in 1: length(unique(ID))) {  
  onelabel <- filter(balance_data, utrial==ID[i]) # subset
  flamburn <- filter(onelabel, is.flaming) # only flaming stage
  pdf(file.path("../results", file=paste(ID[i], ".pdf", sep=""))) # plot in pdf
  print(qplot(nsec, mass , data=flamburn, geom="point") + 
          geom_vline(xintercept = onelabel$septime[1]))
  dev.off()
}

#clean up env
rm("i", "ID", "concat_csv_dir", "read_balance_file", "onelabel")

## ggplot(balance_data, aes(nsec, log(total.mass))) +
##   geom_line() + facet_wrap(~ utrial)

## ggplot(balance_sum, aes(balance.burned, initial.mass - end.mass + fuel.residual, color=sp.cd)) +
## geom_point() + geom_abline(intercept=0, slope=1)
## ggsave("../results/biomass_burned_plot.png")


## ggplot(balance_sum, aes(sp.cd, balance.burned)) + geom_violin()
