# Grass-Flammability
##########
Project goal: Detecting difference in flammability among grass species, and 
investigate the causes of variation.
##########
# Data
##########
Each data file referred to has an associate machine readable metadata file.
IF the file is named file.csv,the metadata file is file-metadata.csv. The 
metadata files describe each variable(column) in the data files.
##########
## Sample data
##########
During 2015 summer, study was mainly conducted in eastern New Mexico in Kiowa
National Grassland, we obatained 10 species there. There was another 
species we found in western Oklahoma in Black Kettle National Grassland. For
each species, we collected 8-10 pairs.
#########
Sample ID, date, area, latitude and longitude, etc are stored in 
data/sample-data.csv
#########
## Trait data
#########
### Fuel moisture and LMA
For each paired samples, we collected sub-sample for measuring field 
moisture content. Greenhouse moisture will also be measured just before 
burning trial on paired samples by taking sub-smaples. Leaf area will
be scanned, records will contain both fresh mass, dry mass and leaf area.Data
is stored in data/FMC-LMA-burned.csv and ./FMC-LMA-destroyed.csv with one
row per sample.
##########
For grass with needle leaves, leaf area will be measured by taking two
diameters of needle which perpendicular to each other, and needle length.
Data is stored in FMC-LAM-needle-burned and FMC-LAM-needle-destroyed.csv
with one row per sample.
########
### Architecture data
########
For paired samples, plant height and width, tiller number and basal area will
be taken and stored in data/architecture-burned.csv and ./architecture-destroyed.csv
with one row per sample.
######## 
### Biomass density and Bulk density data
########
For destroyed smaples, each indivdual will be cut into 5 equal parts along 
height. Volume of entire plant will be measured. Oven dry weight of each part
and corresponding height and volume data are stored in
data/biomass-bulk-density.csv with one row per sample.
########
### Surface area volume ratio data
########
NO idea now!
########
## Burning  data
########
Burning  data contains two fiels:HOBO data and Flammability conponents data.
########
### HOBO data
########
Sample ID, trial number, date, time, temperature are stored in 
data/hobo-raw-data.csv with labeling by burning date.
########
### Flammability data
########
Sample ID, date, ignition time, combustion time, loss biomass, total biomass,
maximum flame height will be stored in
data/flammability-components.csv with one row per sample.
#######
Biomass loss rate data will be stored in
data/biomass-loss-rate.csv with per fiel per trial by labeling with burning date. 
Records mainly include sample ID, time, biomass.
