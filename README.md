# Grass-Flammability
##########
Project goal: Detecting difference in flammability among grass species, and 
investigate the causes of difference.
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
species we found in western Oklahoma. For each species, we collected 8-10 pairs.
#########
Sample ID, date, area, latitude and longitude, etc are stored in 
data/sample_data.csv
#########
## Trait data
#########
### Fuel moisture and LMA
For each paired samples, we collected sub-sample for measuring field 
moisture content. Greenhouse moisture will also be measured just before 
burning trial on paired samples by taking sub-smaples. Leaf area will
be scanned, records will contain both fresh mass, dry mass and leaf area.Data
will be stored in data/Trait/Burned/FMC_LMA.csv and ./Destroyed/FMC_LMA.csv
with one row per sample.
##########
For grass with needle leaves, leaf area will be measured by taking needle 
diameter and length. These data will be stored in FMC-LAM-needle.csv with 
one row per sample seperatly in Bunred and Destroyed file.
########
### Architecture data
########
For paired samples, plant height and width, tiller number and basal area will
be taken and stored in ./Burned/Architecture.csv and ./Destroyed/
Architecture.csv with one row per sample. 
### Biomass density and Bulk density data
########
For destroyed smaples, each indivdual will be cut into 5 equal parts along 
height. Volume of entire plant will be measured. Oven dry weight of each part
and corresponding height and volume will be stored in
data/Trait/Destroyed/Biomass-Bulk-density-data.csv with one row per sample.
########
### Surface area volume ratio data
########
NO idea now!
## Burning  data
########
Burning  data contains two fiels:HOBO data and Flammability conponents data.
########
### HOBO data
######## 
Sample ID, trial number, date, time, temperature will be stored in 
data/HOBO_raw_data/fiel.csv with one fiel for each day of burning.
########
### Flammability data
########
Sample ID, date, ignition time, combustion time, loss biomass, total biomass,
maximum flame height will be stored in
 data/Flammability_data/Flammability_components_data.csv with one row per sample.
#######
Biomass loss rate data will be stored in
data/Flamambility_data/Biomass_loss_rate/file.csv with per fiel per trial. 
Records mainly include sample ID, time, biomass.
